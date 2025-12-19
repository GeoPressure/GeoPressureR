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
      href = "https://raphaelnussbaumer.com/GeoPressureR/favicon-16x16.png"
    ),
    shiny::tags$script(src = "plot_controls.js"),
    shiny::tags$script(src = "plotly_events.js"),
    shiny::tags$script(HTML(
      "
      Shiny.addCustomMessageHandler('updateTitle', function(title) {
        document.title = title;
      });
    "
    ))
  ),

  # Custom header with Bootstrap styling
  div(
    class = "d-flex justify-content-between align-items-center p-3 bg-primary text-white mb-0",
    div(
      class = "d-flex align-items-center",
      div(class = "h3 mb-0 me-3", uiOutput("header_title")),
      actionButton(
        "shortcuts_help",
        label = NULL,
        icon = icon("info-circle"),
        class = "btn btn-outline-light btn-sm",
        title = "Keyboard & mouse shortcuts"
      )
    ),
    div(
      class = "d-flex gap-3 align-items-center",
      # Stap ID selector - only show if stap data exists
      conditionalPanel(
        condition = "output.stap_data_available == true",
        div(
          class = "d-flex flex-column",
          shiny::tags$label(class = "form-label text-white small mb-1", "Stap ID:"),
          div(
            class = "input-group align-items-center",
            actionButton("stap_id_prev", "<", class = "form-group btn btn-outline-light"),
            selectInput(
              "stap_id",
              NULL,
              choices = c("None" = ""), # Will be updated by server
              selected = "",
              width = "150px"
            ),
            actionButton("stap_id_next", ">", class = "form-group btn btn-outline-light"),
            shinyjs::hidden(
              actionButton(
                "compute_stap_btn",
                NULL,
                icon = icon("arrow-rotate-right"),
                class = "form-group btn btn-warning"
              )
            )
          )
        )
      ),

      # Active Series selector - only show if acceleration data exists
      conditionalPanel(
        condition = "output.acceleration_data_available == true",
        div(
          class = "d-flex flex-column",
          shiny::tags$label(class = "form-label text-white small mb-1", "Active Series:"),
          selectInput(
            "active_series",
            NULL,
            choices = c("Pressure" = "pressure", "Acceleration" = "acceleration"),
            selected = "pressure",
            width = "140px"
          )
        )
      ),

      # Label selector with add button
      div(
        class = "d-flex flex-column",
        shiny::tags$label(class = "form-label text-white small mb-1", "Label:"),
        div(
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
                    return '<div>' + dot + escape(txt) + '</div>';
                  }
                }"
              )
            )
          ),
          actionButton("add_label_btn", "+", class = "form-group btn btn-outline-light")
        )
      ),

      # Save button (with hidden download fallback)
      div(
        class = "d-flex flex-column justify-content-end mt-2",
        tagList(
          actionButton("save_btn", "Save", class = "btn btn-success", icon = icon("save")),
          shinyjs::hidden(
            downloadButton(
              "export_btn",
              "Download",
              class = "btn btn-outline-light",
              icon = icon("download")
            )
          )
        )
      )
    )
  ),

  # Plot area with Bootstrap styling
  div(
    class = "position-relative",
    style = "height: calc(100vh - 111px);",
    plotly::plotlyOutput("ts_plot", width = "100%", height = "100%")
  )
)
