ui <- shiny::fluidPage(
  shinyjs::useShinyjs(), # Enable shinyjs for UI control
  title = "Trainset", # This will be updated dynamically by server
  class = "px-0", # Bootstrap class to remove left and right padding
  theme = bslib::bs_theme(
    bootswatch = "flatly",
    base_font = bslib::font_google("Inter")
  ),
  shiny::tags$head(
    shiny::tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "styles.css"
    ),
    shiny::tags$link(
      rel = "shortcut icon",
      href = "https://geopressure.org/GeoPressureR/favicon-16x16.png"
    ),
    shiny::tags$script(src = "plot_controls.js"),
    shiny::tags$script(src = "plotly_events.js"),
    shiny::tags$script(shiny::HTML(
      "
      Shiny.addCustomMessageHandler('updateTitle', function(title) {
        document.title = title;
      });
    "
    ))
  ),

  # Custom header with Bootstrap styling
  shiny::div(
    class = "d-flex justify-content-between align-items-center p-3 bg-primary text-white mb-0",
    shiny::div(
      class = "d-flex align-items-center",
      shiny::div(class = "h3 mb-0 me-3", shiny::uiOutput("header_title")),
      shiny::actionButton(
        "shortcuts_help",
        label = NULL,
        icon = shiny::icon("info-circle"),
        class = "btn btn-outline-light btn-sm",
        title = "Keyboard & mouse shortcuts"
      )
    ),
    shiny::div(
      class = "d-flex gap-3 align-items-center",
      # Stap ID selector - only show if stap data exists
      shiny::conditionalPanel(
        condition = "output.stap_data_available == true",
        shiny::div(
          class = "d-flex flex-column",
          shiny::tags$label(class = "form-label text-white small mb-1", "Stap ID:"),
          shiny::div(
            class = "input-group align-items-center",
            shiny::actionButton("stap_id_prev", "<", class = "form-group btn btn-outline-light"),
            shiny::selectInput(
              "stap_id",
              NULL,
              choices = c("None" = ""), # Will be updated by server
              selected = "",
              width = "150px"
            ),
            shiny::actionButton("stap_id_next", ">", class = "form-group btn btn-outline-light"),
            shinyjs::hidden(
              shiny::actionButton(
                "compute_stap_btn",
                NULL,
                icon = shiny::icon("arrow-rotate-right"),
                class = "form-group btn btn-warning"
              )
            )
          )
        )
      ),

      # Active Series selector - only show if acceleration data exists
      shiny::conditionalPanel(
        condition = "output.acceleration_data_available == true",
        shiny::div(
          class = "d-flex flex-column",
          shiny::tags$label(class = "form-label text-white small mb-1", "Active Series:"),
          shiny::selectInput(
            "active_series",
            NULL,
            choices = c("Pressure" = "pressure", "Acceleration" = "acceleration"),
            selected = "pressure",
            width = "140px"
          )
        )
      ),

      # Label selector with add button
      shiny::div(
        class = "d-flex flex-column",
        shiny::tags$label(class = "form-label text-white small mb-1", "Label:"),
        shiny::div(
          class = "input-group",
          id = "div-group-label",
          shiny::selectizeInput(
            "label_select",
            NULL,
            choices = c("discard", "flight", "elev_1"),
            selected = "elev_1",
            width = "120px",
            options = list(
              render = I(
                "{
                  option: function(item, escape) {
                    function labelColor(lbl) {
                      if (!lbl) return 'black';
                      if (lbl === 'flight') return 'red';
                      if (lbl === 'discard') return 'grey';
                      var m = lbl.match(/^elev_(\\d+)$/);
                      if (m) {
                        var n = parseInt(m[1], 10);
                        var pal = ['#1f77b4','#ff7f0e','#2ca02c','#9467bd','#8c564b','#e377c2','#bcbd22','#17becf'];
                        return pal[(n - 1) % pal.length];
                      }
                      return 'black';
                    }
                    var txt = item.text || item.label || item.value || '';
                    var lbl = item.value || txt;
                    var dot = '<span class=\"label-dot\" style=\"background-color:' + labelColor(lbl) + '\"></span>';
                    return '<div>' + dot + escape(txt) + '</div>';
                  },
                  item: function(item, escape) {
                    function labelColor(lbl) {
                      if (!lbl) return 'black';
                      if (lbl === 'flight') return 'red';
                      if (lbl === 'discard') return 'grey';
                      var m = lbl.match(/^elev_(\\d+)$/);
                      if (m) {
                        var n = parseInt(m[1], 10);
                        var pal = ['#1f77b4','#ff7f0e','#2ca02c','#9467bd','#8c564b','#e377c2','#bcbd22','#17becf'];
                        return pal[(n - 1) % pal.length];
                      }
                      return 'black';
                    }
                    var txt = item.text || item.label || item.value || '';
                    var lbl = item.value || txt;
                    var dot = '<span class=\"label-dot\" style=\"background-color:' + labelColor(lbl) + '\"></span>';
                    return '<div class=\"item\">' + dot + escape(txt) + '</div>';
                  }
                }"
              )
            )
          ),
          shiny::actionButton("add_label_btn", "+", class = "form-group btn btn-outline-light")
        )
      ),

      shiny::div(
        class = "d-flex flex-column justify-content-end mt-2",
        shiny::actionButton(
          "check_btn",
          "Check",
          class = "btn btn-outline-light btn-sm",
          icon = shiny::icon("check")
        )
      ),

      # Save button (with hidden download fallback)
      shiny::div(
        class = "d-flex flex-column justify-content-end mt-2",
        shiny::tagList(
          shiny::actionButton(
            "save_btn",
            "Save",
            class = "btn btn-success",
            icon = shiny::icon("save")
          ),
          shinyjs::hidden(
            shiny::downloadButton(
              "export_btn",
              "Download",
              class = "btn btn-outline-light",
              icon = shiny::icon("download")
            )
          )
        )
      )
    )
  ),

  # Plot area with Bootstrap styling
  shiny::div(
    class = "position-relative",
    style = "height: calc(100vh - 111px);",
    plotly::plotlyOutput("ts_plot", width = "100%", height = "100%")
  )
)
