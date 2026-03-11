stap_color_map_js <- glue::glue_collapse(
  glue::glue("'{tag$stap$stap_id}':'{tag$stap$col}'"),
  sep = ","
)

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
    shiny::tags$link(
      href = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.3/font/bootstrap-icons.min.css",
      rel = "stylesheet"
    ),
    shiny::tags$style(
      type = "text/css",
      "html, body {width:100%; height:100%; font-family: Oswald, sans-serif;}
      .primary{background-color:#007bff; color:#fff;}
      .js-plotly-plot .plotly .modebar{left:0;}
      .gpv-side-panel{
        z-index:500; min-width:300px; padding:10px 12px; background-color:#fff;
        border-left:1px solid #e5e5e5; max-height:100vh; overflow-y:auto;
      }
      .gpv-panel-header, .gpv-toggle, .gpv-field-row, .gpv-stap-select .gpv-stap-option,
      .gpv-stap-select .gpv-stap-item, .gpv-flight-title, .gpv-flight-metric{
        display:flex; align-items:center;
      }
      .gpv-toggle .form-group, .gpv-field-row .form-group,
      .gpv-stap-nav .gpv-stap-select .form-group, .gpv-stap-nav .gpv-stap-select .selectize-control{
        margin-bottom:0;
      }
      .gpv-panel-header{justify-content:space-between; gap:10px; margin-bottom:6px; padding-bottom:4px;}
      .gpv-toggle{gap:8px;}
      .gpv-toggle-label{font-weight:700; margin:0; white-space:nowrap;}
      .gpv-help-text{margin:0 0 8px 0; font-size:1rem; font-weight:400; color:#495057; line-height:1.3;}
      .gpv-field-row{gap:8px; margin-bottom:8px;}
      .gpv-field-label{font-weight:700; flex:1;}
      .gpv-field-row .form-control{height:34px;}
      .gpv-stap-nav{display:flex; align-items:stretch; margin-bottom:8px;}
      .gpv-stap-nav .gpv-nav-left, .gpv-stap-nav .gpv-nav-right{flex:0 0 36px;}
      .gpv-stap-nav .gpv-stap-select{flex:1;}
      .gpv-stap-nav .gpv-nav-left .btn, .gpv-stap-nav .gpv-nav-right .btn{
        width:100%; height:34px; padding:6px 8px; border-radius:0;
      }
      .gpv-stap-nav .gpv-nav-left .btn{border-top-left-radius:4px; border-bottom-left-radius:4px;}
      .gpv-stap-nav .gpv-nav-right .btn{border-top-right-radius:4px; border-bottom-right-radius:4px;}
      .gpv-stap-nav .gpv-stap-select .selectize-input{
        border-radius:0; min-height:34px; padding:6px 8px; display:flex; align-items:center;
        flex-wrap:nowrap; overflow:hidden;
      }
      .gpv-stap-nav .gpv-stap-select .selectize-input > .item{
        max-width:100%; overflow:hidden; text-overflow:ellipsis; white-space:nowrap;
      }
      .gpv-stap-nav .gpv-stap-select .selectize-input > input{
        width:0 !important; min-width:0 !important; max-width:0 !important;
        padding:0 !important; margin:0 !important; opacity:0 !important; border:0 !important;
      }
      .gpv-stap-select .gpv-stap-option, .gpv-stap-select .gpv-stap-item{gap:6px;}
      .gpv-flight-row{display:grid; grid-template-columns:1fr 1fr; gap:8px; margin-bottom:8px;}
      .gpv-flight-card{
        padding:6px; background-color:#f8f9fa; border:2px solid #e5e5e5;
        border-radius:6px; min-height:72px;
      }
      .gpv-flight-title{gap:6px; margin-bottom:4px; font-weight:700; font-size:1.12rem;}
      .gpv-flight-metrics{display:grid; grid-template-columns:1fr 1fr; gap:5px;}
      .gpv-flight-metric{
        gap:6px; line-height:1.1; font-size:1.05rem; background-color:#fff;
        border:1px solid #dee2e6; border-radius:4px; padding:2px 3px;
      }
      .gpv-flight-metric .bi{font-size:1.2rem; color:#6c757d;}
      .gpv-flight-metric-main{display:flex; align-items:baseline; gap:3px; white-space:nowrap;}
      .gpv-flight-metric-value{font-weight:700;}
      .gpv-flight-metric-unit{font-size:0.78em; color:#6c757d; font-weight:500;}
      .gpv-color-dot{
        display:inline-block; width:12px; height:12px; border-radius:50%;
        border:1px solid rgba(0,0,0,0.35);
      }"
    ),
    shiny::tags$script(shiny::HTML(
      "
      Shiny.addCustomMessageHandler('updateTitle', function(title) {
        document.title = title;
      });
    "
    )),
    shiny::tags$script(shiny::HTML(
      glue::glue("window.gpvStapColors = {{{stap_color_map_js}}};")
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
  ),
  shiny::absolutePanel(
    top = 0,
    right = 0,
    draggable = FALSE,
    width = "320px",
    style = "padding:0;",
    shiny::div(
      class = "gpv-side-panel",
      shiny::div(
        class = "gpv-panel-header",
        shiny::htmlOutput("tag_id"),
        shiny::div(
          class = "gpv-toggle",
          shiny::tags$span("Full Track", class = "gpv-toggle-label"),
          shinyWidgets::switchInput(
            "full_track",
            label = NULL,
            value = TRUE,
            inline = TRUE,
            size = "small"
          )
        )
      ),
      shiny::div(
        id = "track_info_view",
        shiny::div(
          class = "gpv-field-row",
          shiny::tags$span("Minimum duration [days]", class = "gpv-field-label"),
          shiny::numericInput(
            "min_dur_stap",
            NULL,
            min = 0,
            max = 50,
            value = 0,
            step = 0.5,
            width = "90px"
          )
        ),
        shiny::downloadButton(
          "export_path",
          "Export current path (CSV)",
          style = "background-color: #28a745; color: white; width: 100%;"
        )
      ),
      shiny::div(
        id = "stap_info_view",
        shiny::div("Stationary period"),
        shiny::div(
          class = "gpv-stap-nav",
          shiny::div(
            class = "gpv-nav-left",
            shiny::actionButton("previous_position", "<")
          ),
          shiny::div(
            class = "gpv-stap-select",
            shiny::selectizeInput(
              "stap_id",
              label = NULL,
              choices = c("1" = "1"),
              width = "100%",
              options = list(
                onInitialize = I(
                  "function() {
                    this.$control_input.prop('readonly', true);
                  }"
                ),
                render = I(
                  "{
                    option: function(item, escape) {
                      var col = (window.gpvStapColors && window.gpvStapColors[item.value]) || '#999999';
                      var txt = item.text || item.label || item.value || '';
                      var dot = '<span class=\"gpv-color-dot\" style=\"background-color:' + col + ';\"></span>';
                      return '<div class=\"gpv-stap-option\">' + dot + '<span>' + escape(txt) + '</span></div>';
                    },
                    item: function(item, escape) {
                      var col = (window.gpvStapColors && window.gpvStapColors[item.value]) || '#999999';
                      var txt = item.text || item.label || item.value || '';
                      var dot = '<span class=\"gpv-color-dot\" style=\"background-color:' + col + ';\"></span>';
                      return '<div class=\"gpv-stap-item\">' + dot + '<span>' + escape(txt) + '</span></div>';
                    }
                  }"
                )
              )
            )
          ),
          shiny::div(
            class = "gpv-nav-right",
            shiny::actionButton("next_position", ">")
          )
        ),
        shiny::div(
          class = "gpv-flight-row",
          shiny::htmlOutput("flight_prev_info"),
          shiny::htmlOutput("flight_next_info")
        ),
        shiny::sliderInput(
          "speed",
          "Groundspeed threshold [km/h]",
          min = 0,
          max = 150,
          value = 40,
          step = 10
        ),
        shiny::radioButtons(
          "map_source",
          label = "Probability map",
          inline = TRUE,
          choices = names(maps),
          selected = tail(names(maps), 1)
        ),
        shiny::div("Position update"),
        shiny::fluidPage(
          id = "edit_query_position_id",
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::actionButton("edit_position", "Edit position", width = "100%")
            ),
            shiny::column(
              6,
              shiny::actionButton("query_position", "Query pressure", width = "100%")
            )
          )
        ),
        shiny::tags$p(
          class = "gpv-help-text",
          "Click the map to move the selected stationary period, then click Query pressure to compare the time series."
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
