#' @rdname geopressuretemplate
#' @family geopressuretemplate
#' @export
geopressuretemplate_config <- function(
  id,
  config = config::get(config = id),
  assert_tag = TRUE,
  assert_graph = FALSE,
  ...
) {
  # 1. Create the config from the default value
  c <- param_create(id, default = TRUE)

  # Warn when config.yml has no entry for id, as config::get() then silently returns `default`
  config_file <- Sys.getenv("R_CONFIG_FILE", "config.yml")
  if (
    file.exists(config_file) &&
      !any(grepl(glue::glue("^[\"']?{id}[\"']?\\s*:"), readLines(config_file, warn = FALSE)))
  ) {
    cli::cli_warn(c(
      "!" = "{.val {id}} has no entry in {.file {config_file}}: only {.field default} values are used.",
      ">" = "Add a {.code {id}:} section to {.file {config_file}} or check the spelling of {.arg id}."
    ))
  }

  # Warn on function arguments placed directly under the tag id instead of under their function
  wrong_level <- intersect(names(config), unlist(lapply(c[vapply(c, is.list, logical(1))], names)))
  if (length(wrong_level) > 0) {
    cli::cli_warn(c(
      "!" = "In the config of {.val {id}}, {.field {wrong_level}} {?is/are} ignored because {?it is/they are} not nested under a function name.",
      ">" = "Move {cli::qty(wrong_level)}{?it/them} under the corresponding function, e.g. {.code tag_create: crop_start: ...}."
    ))
  }

  # 2. Overwrite with config file provided as input of the function
  if (!is.null(config)) {
    c <- merge_params(c, config)
  }

  # 3. Overwrite with any parameter provided as input directly
  c <- merge_params(c, list(...))

  # Perform check
  if (assert_tag) {
    if (length(c$geopressuretemplate$likelihood) <= 0) {
      cli::cli_abort(c(
        x = "{.var geopressuretemplate$likelihood} needs to be provided.",
        ">" = "Make sure {.var geopressuretemplate$likelihood} is a list in {.file config.yml} (e.g.
        {.code ['map_pressure', 'map_light']})."
      ))
    }
  }

  # Additional check to perform only if the workflow needs to run the graph model. This allows to
  # pick up on error in config from the beginning of the workflow.
  if (assert_graph) {
    # Validate that the likelihood is properly configured to include either
    # pressure or light mapping, or both
    if (
      !("map_pressure" %in%
        c$geopressuretemplate$likelihood ||
        "map_light" %in% c$geopressuretemplate$likelihood)
    ) {
      cli::cli_abort(c(
        x = "{.var geopressuretemplate$likelihood} needs to contain either {.field map_pressure} or
        {.field map_light} or both",
        ">" = "Make sure {.var geopressuretemplate$likelihood} is set in {.file config.yml}."
      ))
    }

    # Validate that the outputs include at least one of the possible output types
    possible_outputs <- c("marginal", "most_likely", "simulation")
    if (!any(possible_outputs %in% c$geopressuretemplate$outputs)) {
      cli::cli_abort(c(
        x = "{.var outputs} needs to be provided and contain at least one of
        {.val {possible_outputs}}",
        ">" = "Make sure {.var outputs} is set in {.file config.yml}."
      ))
    }

    # Validate that `nj` is specified if simulation is requested in outputs
    if (
      "simulation" %in%
        c$geopressuretemplate$outputs &&
        c$graph_simulation$nj <= 0
    ) {
      cli::cli_abort(c(
        x = "{.var nj} is required with {.val simulation} in {.var outputs}.",
        ">" = "Make sure {.var graph_simulation$nj} is set in {.file config.yml}."
      ))
    }
  }

  return(c)
}
