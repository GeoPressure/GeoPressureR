server <- function(input, output, session) {
  # Configuration parameters
  rv <- init(.tag = .tag, .stapath = .stapath, .twl = .twl, input = input)
  list2env(rv, environment())
  map_grid_value <- get0("map_grid", inherits = FALSE)

  if (isTRUE(shiny::getShinyOption("stop_on_session_end", TRUE))) {
    session$onSessionEnded(function() {
      stopApp()
    })
  }

  # Initialize calibration modal module
  show_calibration <- modal_calibration_server(
    "calibration_modal",
    twl = twl,
    stapath = stapath,
    twl_calib = twl_calib,
    map_light_twl = map_light_twl,
    tag = .tag,
    compute_known = compute_known,
    llp_param = llp_param
  )

  # Setup navigation and return update_stapath function
  nav_helpers <- setup_navigation_observers(
    input,
    output,
    session,
    stapath,
    show_calibration
  )
  update_stapath <- nav_helpers$update_stapath

  # Render outputs
  render_plotly_output(
    input,
    output,
    twl,
    stapath,
    drawing,
    is_modifying,
    zoom_state,
    .light_trace
  )
  render_map_output(
    output,
    has_map,
    extent,
    map_display,
    contour_display,
    stapath,
    known_positions,
    col,
    input
  )

  # Setup observers
  setup_drawing_observers(
    input,
    drawing,
    stapath,
    map_light_aggregate,
    map_grid_value,
    update_stapath,
    session
  )
  setup_labeling_observers(input, is_modifying, twl, zoom_state, session)

  if (has_map) {
    setup_position_observers(
      input,
      stapath,
      is_edit,
      map_likelihood,
      map_grid_value,
      session
    )
  }

  # Setup export handlers
  setup_export_handlers(output, twl, stapath, .tag)
}
