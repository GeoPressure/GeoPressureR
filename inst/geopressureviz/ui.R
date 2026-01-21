ui <- shiny::bootstrapPage(
  title = "GeoPressureViz",
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
    shiny::tags$style(
      type = "text/css",
      "html, body {width:100%;height:100%; font-family: Oswald,
               sans-serif;}.primary{background-color:#007bff; color: #fff;}.js-plotly-plot
               .plotly .modebar{left: 0}"
    ),
    shiny::tags$script(shiny::HTML(
      "
      Shiny.addCustomMessageHandler('updateTitle', function(title) {
        document.title = title;
      });
    "
    ))
  ),
  leaflet::leafletOutput("map", width = "100%", height = "100%"),
  shiny::absolutePanel(
    top = 0,
    left = 0,
    draggable = FALSE,
    width = "200px",
    style = "z-index:500; min-width: 300px;padding-left: 50px",
    shiny::tags$h2("GeoPressureViz", style = "color:white;"),
    shiny::tags$a(
      "About GeoPressureR",
      href = "https://raphaelnussbaumer.com/GeoPressureR/",
      style = "display: block;padding-bottom:20px;"
    ),
  ),
  shiny::absolutePanel(
    top = 0,
    right = 0,
    draggable = FALSE,
    width = "200px",
    style = "z-index:500; min-width: 300px;padding: 5px 10px;background-color:white;",
    shiny::fluidPage(
      shiny::fluidRow(
        shiny::column(3, shiny::htmlOutput("tag_id")),
        shiny::column(
          9,
          shiny::div(
            style = "text-align: center;",
            "Full Track",
            shinyWidgets::switchInput(
              "full_track",
              value = TRUE,
              inline = TRUE,
              size = "small"
            )
          )
        )
      )
    ),
    shiny::fluidPage(
      id = "track_info_view",
      shiny::fluidRow(
        shiny::column(
          8,
          shiny::tags$p(
            "Minimum duration [days]",
            style = "font-weight:bold; line-height: 34px;text-align: right;"
          )
        ),
        shiny::column(
          4,
          style = "padding:0px;",
          shiny::numericInput(
            "min_dur_stap",
            NULL,
            min = 0,
            max = 50,
            value = 0,
            step = 0.5
          )
        )
      ),
      shiny::fluidRow(
        shiny::actionButton(
          "export_path",
          "Export path to interim",
          style = "background-color: #28a745; color: white; width: 100%;"
        )
      )
    ),
    shiny::div(
      id = "stap_info_view",
      shiny::fluidPage(
        shiny::tags$p(
          "Choose a stationary period",
          style = "font-weight:bold;"
        ),
        shiny::fluidRow(
          shiny::column(
            2,
            style = "padding:0px;",
            shiny::actionButton("previous_position", "<", width = "100%")
          ),
          shiny::column(
            8,
            style = "padding:0px;",
            shiny::selectInput("stap_id", label = NULL, choices = "1")
          ),
          shiny::column(
            2,
            style = "padding:0px;",
            shiny::actionButton("next_position", ">", width = "100%")
          )
        )
      ),
      shiny::fluidRow(
        shiny::column(6, shiny::htmlOutput("flight_prev_info")),
        shiny::column(6, shiny::htmlOutput("flight_next_info"))
      ),
      shiny::tags$hr(),
      shiny::sliderInput(
        "speed",
        "Groundspeed limit [km/h]",
        min = 0,
        max = 150,
        value = 40,
        step = 10
      ),
      shiny::div(
        shiny::radioButtons(
          "map_source",
          label = "Probability map to display",
          inline = TRUE,
          choices = names(maps),
          selected = tail(names(maps), 1)
        ),
        shiny::tags$hr(),
        shiny::tags$p(
          "Change position by clicking on the map and update the pressure time series."
        ),
        shiny::fluidPage(
          id = "edit_query_position_id",
          shiny::fluidRow(
            shiny::column(6, shiny::actionButton("edit_position", "Edit position")),
            shiny::column(6, shiny::actionButton("query_position", "Query pressure"))
          )
        ),
        shiny::checkboxInput(
          "edit_position_interpolate",
          label = "Interpolate positions between stap",
          value = FALSE
        )
      )
    )
  ),
  shiny::fixedPanel(
    bottom = 0,
    left = 0,
    width = "100%",
    height = "300px",
    plotly::plotlyOutput("pressure_plot")
  )
)
