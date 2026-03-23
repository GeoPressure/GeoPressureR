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
      )
    ),
    shiny::div(
      class = "container-fluid d-flex flex-column vh-100",
      shiny::fluidRow(
        class = "text-center bg-black align-items-center",
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
              shiny::actionButton(
                "previous_position",
                "<",
                class = "btn-nav btn-nav-prev"
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
              shiny::actionButton(
                "next_position",
                ">",
                class = "btn-nav btn-nav-next"
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
            shiny::actionButton(
              "label_twilight",
              "Edit",
              class = "btn-primary btn-sm",
              icon = shiny::icon("pen"),
              width = "70px"
            ),
            shiny::actionButton(
              "save_twilight",
              "Save",
              class = "btn-success btn-sm btn-inline-icon",
              icon = shiny::icon("save"),
              width = "70px"
            ),
            shinyjs::hidden(
              shiny::downloadButton(
                "export_twilight",
                "Export",
                class = "btn-primary btn-sm",
                width = "70px"
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
              class = "btn-group",
              shiny::actionButton(
                "add_stap",
                NULL,
                icon = shiny::icon("square-plus"),
                class = "btn-sm bg-secondary"
              ),
              shiny::actionButton(
                "remove_stap",
                NULL,
                icon = shiny::icon("square-minus"),
                class = "btn-sm bg-secondary"
              ),
              shiny::actionButton(
                "change_range",
                NULL,
                icon = shiny::icon("pen"),
                class = "btn-sm bg-secondary"
              )
            ),
            shiny::actionButton(
              "save_stap",
              "Save",
              class = "btn-success btn-sm btn-inline-icon",
              icon = shiny::icon("save"),
              width = "70px"
            ),
            shinyjs::hidden(
              shiny::downloadButton(
                "export_stap",
                "Export",
                class = "btn-primary btn-sm",
                width = "70px"
              )
            )
          )
        ),
        shiny::column(
          2,
          class = "p-0",
          shiny::actionButton(
            "show_twilight_histogram",
            "Likelihood Settings",
            icon = shiny::icon("sliders-h"),
            class = "bg-secondary"
          )
        ),
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
            plotly::plotlyOutput("plotly_div", width = "100%", height = "100%"),
          )
        ),
        shiny::column(
          5,
          id = "map_container",
          class = "flex-fill p-0",
          leaflet::leafletOutput("map", width = "100%", height = "100%"),
        ),
      ),
    )
  )
}
