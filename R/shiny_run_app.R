# Resolve launch.browser handler to the system browser.
shiny_launch_browser <- function(launch_browser) {
  if (isTRUE(launch_browser)) {
    function(url) utils::browseURL(url)
  } else {
    FALSE
  }
}

# Run the app in the current session.
shiny_run_app_fg <- function(app_dir, shiny_opts, launch_browser = TRUE) {
  do.call(shiny::shinyOptions, shiny_opts)
  shiny::runApp(
    app_dir,
    launch.browser = shiny_launch_browser(launch_browser)
  )
}

# Run the app in a background R process and optionally open a browser.
shiny_run_app_bg <- function(
  app_dir,
  shiny_opts,
  launch_browser = TRUE,
  proc_option = NULL,
  proc_id = NULL,
  app_label = "app"
) {
  p <- callr::r_bg(
    func = function(app_dir, shiny_opts) {
      loadNamespace("GeoPressureR")
      do.call(shiny::shinyOptions, shiny_opts)
      shiny::runApp(app_dir)
    },
    args = list(
      app_dir = app_dir,
      shiny_opts = shiny_opts
    )
  )

  port <- NA
  while (p$is_alive()) {
    p$poll_io(1000) # wait up to 1s for new output
    err <- p$read_error()
    out <- p$read_output()
    txt <- glue::glue("{err}\n{out}")

    if (grepl("Listening on http://127\\.0\\.0\\.1:[0-9]+", txt)) {
      port <- sub(".*127\\.0\\.0\\.1:([0-9]+).*", "\\1", txt)
      url <- glue::glue("http://127.0.0.1:{port}")

      if (!is.null(proc_option) && !is.null(proc_id)) {
        procs <- getOption(proc_option, default = list())
        procs <- Filter(function(x) inherits(x, "process") && isTRUE(x$is_alive()), procs)
        procs[[proc_id]] <- p
        options(structure(list(procs), names = proc_option))
      }

      if (isTRUE(launch_browser)) {
        cli::cli_alert_success("Opening {app_label} app at {.url {url}}")
        utils::browseURL(url)
      } else {
        cli::cli_alert_success("{app_label} app running at {.url {url}}")
      }
      break
    }
  }
  invisible(p)
}
