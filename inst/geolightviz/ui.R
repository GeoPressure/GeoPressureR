ui <- function() {
  shiny::bootstrapPage(
    theme = bslib::bs_theme(version = 5),
    shinyjs::useShinyjs(),
    shiny::tags$head(
      shiny::tags$link(
        rel = "shortcut icon",
        href = "https://geopressure.org/GeoPressureR/favicon-16x16.png"
      ),
      shiny::tags$link(
        href = "https://fonts.googleapis.com/css?family=Oswald",
        rel = "stylesheet"
      ),
      shiny::tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "style.css"
      ),
      shiny::tags$script(src = "keyboard_shortcuts.js")
    ),
    shiny::div(
      class = "container-fluid d-flex flex-column vh-100",
      shiny::fluidRow(
        class = "gpv-header text-center bg-black align-items-center",
        shiny::column(
          4,
          shiny::div(
            class = "d-flex align-items-center gap-2",
            shiny::tags$h2("GeoLightViz", class = "m-0"),
            shiny::htmlOutput("tag_id", class = "text-secondary m-0")
          ),
          shiny::fluidRow(
            id = "stapath_nav_container",
            class = "mt-2 d-flex justify-content-center",
            shiny::column(
              2,
              class = "p-0",
              bslib::tooltip(
                shiny::actionButton(
                  "previous_position",
                  "<",
                  class = "btn-nav btn-nav-prev"
                ),
                "Previous stationary period",
                placement = "bottom"
              )
            ),
            shiny::column(
              6,
              class = "p-0",
              shiny::selectInput(
                "stap_id",
                label = NULL,
                choices = "1",
                width = "100%"
              )
            ),
            shiny::column(
              2,
              class = "p-0",
              bslib::tooltip(
                shiny::actionButton(
                  "next_position",
                  ">",
                  class = "btn-nav btn-nav-next"
                ),
                "Next stationary period",
                placement = "bottom"
              )
            )
          )
        ),
        shiny::column(
          3,
          shiny::div(
            class = "stationary-box",
            shiny::tags$p(
              "Labeling:",
              class = "section-label"
            ),
            shiny::div(
              class = "gpv-action-row",
              shiny::actionButton(
                "label_twilight",
                shiny::tags$span("Edit", class = "btn-label"),
                class = "btn-primary btn-sm gpv-action-btn",
                icon = shiny::icon("pen"),
                title = "Start or stop twilight labeling"
              ),
              shiny::div(
                class = "btn-group",
                bslib::tooltip(
                  shiny::tags$span(
                    shiny::actionButton(
                      "undo_twilight_label",
                      NULL,
                      icon = shiny::icon("rotate-left"),
                      class = "btn-sm bg-secondary gpv-icon-btn",
                      disabled = TRUE
                    )
                  ),
                  "Undo label",
                  placement = "bottom"
                ),
                bslib::tooltip(
                  shiny::tags$span(
                    shiny::actionButton(
                      "redo_twilight_label",
                      NULL,
                      icon = shiny::icon("rotate-right"),
                      class = "btn-sm bg-secondary gpv-icon-btn",
                      disabled = TRUE
                    )
                  ),
                  "Redo label",
                  placement = "bottom"
                )
              ),
              shiny::actionButton(
                "save_twilight",
                shiny::tags$span("Save", class = "btn-label"),
                class = "btn-success btn-sm gpv-action-btn",
                icon = shiny::icon("save"),
                title = "Save twilight labels"
              ),
              shinyjs::hidden(
                shiny::downloadButton(
                  "export_twilight",
                  shiny::tags$span("Export", class = "btn-label"),
                  class = "btn-primary btn-sm gpv-action-btn",
                  title = "Export twilight labels"
                )
              )
            )
          )
        ),
        shiny::column(
          3,
          shiny::div(
            class = "stationary-box",
            shiny::tags$p(
              "Stationary period:",
              class = "section-label"
            ),
            shiny::div(
              class = "gpv-action-row",
              shiny::div(
                class = "btn-group",
                bslib::tooltip(
                  shiny::actionButton(
                    "add_stap",
                    NULL,
                    icon = shiny::icon("square-plus"),
                    class = "btn-sm bg-secondary gpv-icon-btn"
                  ),
                  "Add stationary period",
                  placement = "bottom"
                ),
                bslib::tooltip(
                  shiny::actionButton(
                    "remove_stap",
                    NULL,
                    icon = shiny::icon("square-minus"),
                    class = "btn-sm bg-secondary gpv-icon-btn"
                  ),
                  "Remove stationary period",
                  placement = "bottom"
                ),
                bslib::tooltip(
                  shiny::actionButton(
                    "change_range",
                    NULL,
                    icon = shiny::icon("pen"),
                    class = "btn-sm bg-secondary gpv-icon-btn"
                  ),
                  "Edit stationary-period range",
                  placement = "bottom"
                )
              ),
              shiny::actionButton(
                "save_stap",
                shiny::tags$span("Save", class = "btn-label"),
                class = "btn-success btn-sm gpv-action-btn",
                icon = shiny::icon("save"),
                title = "Save stationary periods"
              ),
              shinyjs::hidden(
                shiny::downloadButton(
                  "export_stap",
                  shiny::tags$span("Export", class = "btn-label"),
                  class = "btn-primary btn-sm gpv-action-btn",
                  title = "Export stationary periods"
                )
              )
            )
          )
        ),
        shiny::column(
          2,
          class = "p-0",
          shiny::actionButton(
            "show_twilight_histogram",
            shiny::tags$span("Likelihood Settings", class = "btn-label"),
            icon = shiny::icon("sliders-h"),
            class = "bg-secondary gpv-action-btn",
            title = "Open likelihood settings"
          )
        )
      ),
      shiny::fluidRow(
        class = "d-flex flex-fill",
        shiny::column(
          7,
          id = "plot_container",
          class = "d-flex flex-column flex-fill bg-black",
          shiny::div(
            class = "d-flex flex-column flex-fill",
            height = "100%",
            plotly::plotlyOutput("plotly_div", width = "100%", height = "100%")
          )
        ),
        shiny::column(
          5,
          id = "map_container",
          class = "flex-fill p-0",
          leaflet::leafletOutput("map", width = "100%", height = "100%")
        )
      )
    )
  )
}
