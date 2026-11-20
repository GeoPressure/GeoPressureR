#' @family geopressure_map
#' @rdname geopressure_map
#' @export
geopressure_map_mismatch <- function(
  tag,
  max_sample = 250,
  margin = 30,
  thr_mask = 0.9,
  timeout = 300,
  workers = "auto",
  era5_dataset = "single-levels",
  compute_known = FALSE,
  keep_mask = TRUE,
  keep_mask_split = FALSE,
  debug = FALSE,
  quiet = FALSE
) {
  # Check tag
  tag_assert(tag, "setmap")

  # Assert input
  assertthat::assert_that(is.numeric(max_sample))
  assertthat::assert_that(0 < max_sample)
  assertthat::assert_that(is.numeric(margin))
  assertthat::assert_that(0 < margin)
  assertthat::assert_that(is.logical(keep_mask))
  assertthat::assert_that(is.logical(keep_mask_split))
  assertthat::assert_that(is.numeric(thr_mask))
  assertthat::assert_that(thr_mask >= 0 & thr_mask <= 1)
  assertthat::assert_that(is.numeric(timeout))
  assertthat::assert_that(era5_dataset %in% c("single-levels", "land", "both"))
  assertthat::assert_that(is.numeric(workers) | workers == "auto")
  assertthat::assert_that(is.logical(debug))
  assertthat::assert_that(is.logical(quiet))

  if (!quiet) {
    cli::cli_progress_step("Pre-process pressure data")
  }

  capture_debug_output <- function(expr, trace_file) {
    con <- file(trace_file, open = "wt")
    sink(con)
    on.exit({
      sink()
      close(con)
    })
    force(expr)
  }

  # Prepare data
  pres <- geopressure_map_preprocess(tag, compute_known = compute_known)

  body <- list(
    time = as.numeric(as.POSIXct(pres$date)),
    label = pres$stapelev,
    pressure = round(pres$value * 100), # convert from hPa to Pa
    W = tag$param$tag_set_map$extent[1],
    E = tag$param$tag_set_map$extent[2],
    S = tag$param$tag_set_map$extent[3],
    N = tag$param$tag_set_map$extent[4],
    scale = tag$param$tag_set_map$scale,
    maxSample = max_sample,
    margin = margin,
    includeMask = keep_mask,
    maskThreshold = thr_mask,
    dataset = era5_dataset
  )
  if (keep_mask_split) {
    body$includeMaskSplit <- TRUE
  }

  if (debug) {
    temp_file <- tempfile("log_geopressure_map_", fileext = ".json")
    write(jsonlite::toJSON(body, auto_unbox = TRUE, pretty = TRUE), temp_file)
    cli::cli_alert_info("Debug body request file: {.file {temp_file}}")
    trace_file <- tempfile("log_geopressure_map_http_", fileext = ".log")
    cli::cli_alert_info("Debug HTTP trace file: {.file {trace_file}}")
  }

  if (!quiet) {
    cli::cli_progress_step(
      msg = "Generate requests for {.val {length(unique(pres$stapelev))}} stapelev on {.url glp.mgravey.com/GeoPressure/v2/map/}: {.field {unique(pres$stapelev)}}",
      msg_done = "Generate requests for {.val {length(unique(pres$stapelev))}} stapelev on {.url glp.mgravey.com/GeoPressure/v2/map/}"
    )
  }

  # Request URLS
  req <- httr2::request("https://glp.mgravey.com/GeoPressure/v2/map/") |>
    httr2::req_body_json(body) |>
    httr2::req_timeout(timeout) |>
    httr2::req_error(body = function(resp) {
      if (debug) {
        print(httr2::resp_body_json(resp))
      }
      c(
        "x" = "Error with your request on https://glp.mgravey.com/GeoPressure/v2/map/.",
        ">" = httr2::resp_body_json(resp)$errorMessage,
        "i" = if (debug) {
          glue::glue("Body request file: {temp_file}\nHTTP trace file: {trace_file}")
        } else {
          "Please try again with `debug=TRUE`"
        },
        provenance_api_error(resp)
      )
    })

  if (debug) {
    req <- httr2::req_verbose(
      req,
      body_req = TRUE,
      body_resp = TRUE,
      info = TRUE
    )
  }

  # Perform the request and convert the response to json
  if (debug) {
    resp_json <- capture_debug_output(
      {
        resp <- httr2::req_perform(req)
        resp_json <- httr2::resp_body_json(resp)
        print(resp_json)
        resp_json
      },
      trace_file
    )
  } else {
    resp <- httr2::req_perform(req)
    resp_json <- httr2::resp_body_json(resp)
  }

  # Get urls
  urls <- resp_json$data$urls
  urls[vapply(urls, is.null, logical(1))] <- NA
  urls <- unlist(urls)
  labels <- unlist(resp_json$data$labels)
  # Reason reported by the API for each failed url. NULL on older API versions.
  errors <- resp_json$data$errors
  # Band names of the GeoTIFFs, only returned with the mask split. Older API versions return
  # neither, so read the bands by position.
  bands <- unlist(resp_json$data$bands)
  if (keep_mask_split && is.null(bands)) {
    cli::cli_warn(c(
      "!" = "GeoPressureAPI did not return the above/below split of the mask.",
      "i" = "Continuing without {.field frac_above} and {.field frac_below}."
    ))
  }
  if (is.null(bands)) {
    bands <- c("mse", if (keep_mask) "probAlt")
  }

  # Check that the urls exist
  if (anyNA(urls)) {
    i_na <- which(is.na(urls))
    reasons <- unique(unlist(errors[i_na]))
    # cli interpolates these strings, and a server message may contain braces
    reasons <- gsub("}", "}}", gsub("{", "{{", reasons, fixed = TRUE), fixed = TRUE)
    names(reasons) <- rep(">", length(reasons))

    msg <- c(
      "!" = "No url returned for stapelev {.val {labels[i_na]}}.",
      reasons,
      "i" = "Earth Engine failed to compute these maps. A coarser {.arg scale} or a \\
             lower {.arg max_sample} often helps."
    )

    if (all(is.na(urls))) {
      cli::cli_abort(msg)
    } else {
      cli::cli_warn(msg)
    }
    labels <- labels[-i_na]
    urls <- urls[-i_na]
  }

  # Download the GeoTIFFs, computed by GEE when requested. GEE allows up to 100 requests at the
  # same time, so we keep the number of simultaneous requests a little bit below.
  if (workers == "auto") {
    workers <- min(parallel::detectCores(), length(urls))
  }
  assertthat::assert_that(workers > 0 & workers < 100)
  workers <- min(workers, length(urls))
  labels_stap <- as.numeric(sub("\\|.*", "", labels))
  files <- vapply(seq_along(urls), \(i) tempfile(fileext = ".geotiff"), character(1))
  trace_files <- vapply(
    labels,
    \(l) {
      tempfile(
        glue::glue("log_geopressure_map_http_{gsub('[^[:alnum:]_]+', '_', l)}_"),
        fileext = ".log"
      )
    },
    character(1)
  )
  reqs <- lapply(seq_along(urls), function(i) {
    req_i <- httr2::request(urls[[i]]) |>
      httr2::req_timeout(timeout) |>
      httr2::req_error(body = function(resp) {
        error_body <- trimws(httr2::resp_body_string(resp))
        if (debug) {
          print(error_body)
        }
        c(
          "x" = glue::glue(
            "Error while computing/downloading the GeoTIFF for stapelev {labels[[i]]} \\
            (stap {labels_stap[[i]]})."
          ),
          ">" = if (nzchar(error_body)) {
            error_body
          } else {
            glue::glue("HTTP {httr2::resp_status(resp)}.")
          },
          "i" = if (debug) {
            glue::glue("Failing URL: {urls[[i]]}\nHTTP trace file: {trace_files[[i]]}")
          } else {
            glue::glue("Failing URL: {urls[[i]]}")
          }
        )
      })
    if (debug) {
      req_i <- httr2::req_verbose(req_i, body_req = TRUE, body_resp = TRUE, info = TRUE)
    }
    req_i
  })

  if (!quiet) {
    cli::cli_progress_step(
      msg = "Compute (on GEE server) and download .geotiff for {.val {length(urls)}} stapelev (on {.val {workers}} workers)",
      msg_done = "Compute (on GEE server) and download .geotiff for {.val {length(urls)}} stapelev"
    )
  }
  if (debug) {
    # One request at a time, so that each HTTP trace goes to its own file
    for (i in seq_along(reqs)) {
      capture_debug_output(httr2::req_perform(reqs[[i]], path = files[[i]]), trace_files[[i]])
    }
  } else {
    httr2::req_perform_parallel(
      reqs,
      paths = files,
      max_active = workers,
      progress = if (!quiet) {
        list(
          format = "{cli::pb_bar} {cli::pb_percent} ({cli::pb_current}/{cli::pb_total}) | {cli::pb_eta_str}",
          format_done = "{cli::col_green(cli::symbol$tick)} [{cli::pb_elapsed}]",
          clear = TRUE
        )
      } else {
        FALSE
      }
    )
  }

  # Get maps
  idx <- if (!quiet) {
    cli::cli_progress_along(seq_along(files), name = "Read .geotiff")
  } else {
    seq_along(files)
  }
  map <- lapply(idx, function(i_u) {
    map_i <- terra::rast(files[[i_u]])
    names(map_i) <- bands
    map_i
  })

  if (!quiet) {
    cli::cli_progress_step("Post-process maps")
  }

  # Find the number of sample (datapoint) for each map
  nb_sample <- pmin(
    max_sample,
    unlist(lapply(labels, function(l) {
      sum(pres$stapelev == l)
    }))
  )

  # Diagnostics per stapelev before its levels are merged: land pixels, pixels passing the mask
  # (-1 masked, -2 water), smallest MSE (Pa^2 to hPa^2) and, with the mask split, the mean share
  # of samples above and below the ground over the land pixels rejected by the mask
  stapelev <- do.call(
    rbind,
    lapply(map, \(m) {
      v <- terra::values(m, mat = TRUE)
      # Water from a mask band when available: a few pixels have mse = -1 but probAlt = -2
      land <- which(v[, intersect(c("probAlt", "probAbove", "mse"), colnames(v))[1]] > -1.5)
      mse <- v[land, "mse"]
      valid <- which(mse >= 0)
      masked <- land[which(mse == -1)]
      frac <- \(b) if (b %in% colnames(v) && length(masked)) mean(v[masked, b]) else NA_real_
      data.frame(
        n_pixel = length(land),
        n_valid = length(valid),
        mse_min = if (length(valid)) min(mse[valid]) / 1e4 else NA_real_,
        frac_above = frac("probAbove"),
        frac_below = frac("probBelow")
      )
    })
  )
  stapelev <- cbind(
    data.frame(stapelev = labels, stap_id = labels_stap, nb_sample = nb_sample),
    stapelev
  )[order(labels_stap, labels), ]
  rownames(stapelev) <- NULL

  # Keep the bands combined per stap: mse and, with keep_mask, the mask
  map <- lapply(map, \(m) m[[intersect(c("mse", "probAlt"), names(m))]])

  # Initialize the return list from tag$stap to make sure all stap are present
  mse <- vector("list", nrow(tag$stap))
  if (keep_mask) {
    mask <- vector("list", nrow(tag$stap))
  }
  tag$stap$nb_sample <- 0

  for (i_stap in unique(labels_stap)) {
    # Find all stapelevs that belong to this stap
    i_label <- which(labels_stap == i_stap)

    # compute the total number of sample for that stap.
    tag$stap$nb_sample[i_stap] <- sum(nb_sample[i_label])

    # When elev is present, use an average of the mse and mask maps weighted by the number of
    # sample
    tmp <- Reduce(
      `+`,
      mapply(
        function(m, w) {
          w * m
        },
        map[i_label],
        nb_sample[i_label]
      )
    ) /
      sum(nb_sample[i_label])

    # Find pixel below threshold (i.e., -1) in any of the elev and apply to the combined map of
    # mse
    tmp2 <- Reduce(`|`, lapply(map[i_label], function(x) x[[1]] == -1))
    tmp[[1]][tmp2] <- -1

    # Convert the maps to matrix
    mse[[i_stap]] <- terra::as.matrix(tmp[[1]], wide = TRUE)
    # convert MSE from Pa to hPa
    mse[[i_stap]][mse[[i_stap]] > 0] <- mse[[i_stap]][mse[[i_stap]] > 0] /
      100 /
      100
    if (keep_mask) {
      mask[[i_stap]] <- terra::as.matrix(tmp[[2]], wide = TRUE)
    }
  }

  # Warn about staps where no pixel passes the mask of all their levels: likelihood zero everywhere
  stap_empty <- unique(labels_stap)[
    vapply(unique(labels_stap), \(s) !any(mse[[s]] >= 0, na.rm = TRUE), logical(1))
  ]
  if (length(stap_empty) > 0) {
    masked <- stapelev$n_valid == 0 & stapelev$stap_id %in% stap_empty
    stap_combined <- setdiff(stap_empty, stapelev$stap_id[masked])
    # With the mask split, say whether the masked levels sit mostly above or below the ground
    direction <- with(
      stapelev[masked, ],
      ifelse(
        is.na(frac_above),
        "",
        ifelse(frac_above >= frac_below, " (mostly above the ground)", " (mostly below the ground)")
      )
    )
    masked_txt <- glue::glue_collapse(glue::glue("{stapelev$stapelev[masked]}{direction}"), ", ")
    cli::cli_warn(c(
      "!" = "No pixel passes the mask of the pressure map of stap {.val {stap_empty}}.",
      "*" = if (any(masked)) {
        "Stapelev masked everywhere on its own: {masked_txt}."
      },
      "*" = if (length(stap_combined) > 0) {
        "Stap {.val {stap_combined}}: each level passes somewhere, but no pixel passes all levels."
      },
      ">" = "Check the labels of these stapelevs (flight, take-off or altitude change left \\
             unlabelled, wrong {.val elev_n}), or the extent of {.fun tag_set_map}."
    ))
  }

  # Add attribute
  # Delete the downloaded GeoTIFFs and trace files now that the maps are in memory
  unlink(unlist(files))

  tag$map_pressure_mse <- map_create(
    data = mse,
    extent = tag$param$tag_set_map$extent,
    scale = tag$param$tag_set_map$scale,
    stap = tag$stap,
    id = tag$param$id,
    type = "pressure_mse"
  )
  tag$map_pressure_mse$stapelev <- stapelev

  if (keep_mask) {
    tag$map_pressure_mask <- map_create(
      data = mask,
      extent = tag$param$tag_set_map$extent,
      scale = tag$param$tag_set_map$scale,
      stap = tag$stap,
      id = tag$param$id,
      type = "pressure_mask"
    )
  }

  # keep parameters used
  tag$param$geopressure_map <- list(
    max_sample = max_sample,
    margin = margin,
    keep_mask = keep_mask,
    keep_mask_split = keep_mask_split,
    thr_mask = thr_mask,
    timeout = timeout,
    workers = workers,
    era5_dataset = era5_dataset,
    compute_known = compute_known
  )
  tag$param$provenance$geopressure_map <- provenance_create(
    "GeoPressureAPI",
    era5_dataset = era5_dataset,
    api = if (!is.null(resp_json$provenance)) list(resp_json$provenance)
  )

  # return the pressure_mismatch in the same order as requested
  return(tag)
}
