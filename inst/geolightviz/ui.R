ui <- function() {
  shiny::bootstrapPage(
    theme = bslib::bs_theme(version = 5),
    shinyjs::useShinyjs(),
    shiny::tags$head(
      shiny::tags$link(
        rel = "shortcut icon",
        href = "https://raphaelnussbaumer.com/GeoPressureR/favicon-16x16.png"
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
    div(
      class = "container-fluid d-flex flex-column vh-100",
      fluidRow(
        class = "text-center bg-black align-items-center",
        column(
          4,
          div(
            class = "d-flex align-items-center gap-2",
            shiny::tags$h2("GeoLightViz", class = "m-0"),
            htmlOutput("tag_id", class = "text-secondary m-0")
          ),
          fluidRow(
            id = "stapath_nav_container",
            class = "mt-2 d-flex justify-content-center",
            column(
              2,
              class = "p-0",
              actionButton(
                "previous_position",
                "<",
                class = "btn-nav btn-nav-prev"
              )
            ),
            column(
              6,
              class = "p-0",
              selectInput(
                "stap_id",
                label = NULL,
                choices = "1",
                width = "100%"
              )
            ),
            column(
              2,
              class = "p-0",
              actionButton(
                "next_position",
                ">",
                class = "btn-nav btn-nav-next"
              )
            )
          )
        ),
        column(
          3,
          div(
            class = "stationary-box",
            tags$p(
              "Labeling:",
              class = "section-label"
            ),
            actionButton(
              "label_twilight",
              "Edit",
              class = "btn-primary btn-sm",
              icon = icon("pen"),
              width = "70px"
            ),
            actionButton(
              "save_twilight",
              "Save",
              class = "btn-success btn-sm",
              icon = icon("save"),
              width = "70px"
            ),
            shinyjs::hidden(
              downloadButton(
                "export_twilight",
                "Export",
                class = "btn-primary btn-sm",
                width = "70px"
              )
            )
          )
        ),
        column(
          3,
          div(
            class = "stationary-box",
            tags$p(
              "Stationary period:",
              class = "section-label"
            ),
            div(
              class = "btn-group",
              actionButton(
                "add_stap",
                NULL,
                icon = icon("square-plus"),
                class = "btn-sm bg-secondary"
              ),
              actionButton(
                "remove_stap",
                NULL,
                icon = icon("square-minus"),
                class = "btn-sm bg-secondary"
              ),
              actionButton(
                "change_range",
                NULL,
                icon = icon("pen"),
                class = "btn-sm bg-secondary"
              )
            ),
            actionButton(
              "save_stap",
              "Save",
              class = "btn-success btn-sm",
              icon = icon("save"),
              width = "70px"
            ),
            shinyjs::hidden(
              downloadButton(
                "export_stap",
                "Export",
                class = "btn-primary btn-sm",
                width = "70px"
              )
            )
          )
        ),
        column(
          2,
          class = "p-0",
          actionButton(
            "show_twilight_histogram",
            "Likelihood Settings",
            icon = icon("sliders-h"),
            class = "bg-secondary"
          )
        ),
      ),
      fluidRow(
        class = "d-flex flex-fill",
        column(
          7,
          id = "plot_container",
          class = "d-flex flex-column flex-fill bg-black",
          div(
            class = "d-flex flex-column flex-fill",
            height = "100%",
            plotly::plotlyOutput("plotly_div", width = "100%", height = "100%"),
          )
        ),
        column(
          5,
          id = "map_container",
          class = "flex-fill p-0",
          leaflet::leafletOutput("map", width = "100%", height = "100%"),
        ),
      ),
    )
  )
}
