# Shortcuts/help modal. Sourced inside `server()`.

shiny::observeEvent(input$shortcuts_help, {
  shiny::showModal(
    shiny::modalDialog(
      title = "Keyboard and mouse shortcuts",
      easyClose = TRUE,
      size = "l",
      footer = shiny::modalButton("Close"),
      shiny::tagList(
        shiny::h4("Labeling"),
        shiny::tags$ul(
          shiny::tags$li("Click or drag on the plot to apply the selected label."),
          shiny::tags$li(shiny::tagList(
            "Press ",
            shiny::tags$kbd("1"),
            " to ",
            shiny::tags$kbd("9"),
            " to select a label by its order in the label dropdown."
          )),
          shiny::tags$li(
            shiny::tagList(
              shiny::tags$kbd("Ctrl"),
              " (Windows / Linux) or ",
              shiny::tags$kbd("Cmd"),
              " (macOS) + mouse click/selection: clear labels instead of applying a new one."
            )
          )
        ),
        shiny::h4("Time navigation (x-axis)"),
        shiny::tags$ul(
          shiny::tags$li(shiny::tagList(
            shiny::tags$kbd("Mouse wheel"),
            " over plot area: zoom in/out in time."
          )),
          shiny::tags$li(shiny::tagList(
            shiny::tags$kbd("\u2191"),
            " / ",
            shiny::tags$kbd("\u2193"),
            ": zoom in / out around the current view."
          )),
          shiny::tags$li(shiny::tagList(
            shiny::tags$kbd("\u2190"),
            " / ",
            shiny::tags$kbd("\u2192"),
            ": pan left / right in time."
          )),
          shiny::tags$li(shiny::tagList(
            shiny::tags$kbd("Shift"),
            " + ",
            shiny::tags$kbd("\u2190"),
            " / ",
            shiny::tags$kbd("\u2192"),
            ": pan by a full window width."
          )),
          shiny::tags$li(shiny::tagList(
            shiny::tags$kbd("A"),
            ": autoscale x/y axes."
          ))
        ),
        shiny::h4("Y-axis zoom"),
        shiny::tags$ul(
          shiny::tags$li(shiny::tagList(
            shiny::tags$kbd("Mouse wheel"),
            " over left y-axis: zoom pressure axis."
          )),
          shiny::tags$li(shiny::tagList(
            shiny::tags$kbd("Mouse wheel"),
            " over right y-axis: zoom acceleration axis (when available)."
          ))
        ),
        shiny::h4("STAP navigation"),
        shiny::tags$ul(
          shiny::tags$li(shiny::tagList(
            "Use the ",
            shiny::tags$kbd("<"),
            " and ",
            shiny::tags$kbd(">"),
            " buttons next to \"Stap ID\" to move between stationary periods."
          ))
        ),
        shiny::h4("Auto-save and files"),
        shiny::tags$ul(
          shiny::tags$li(
            "If an automatic label folder is configured (e.g. data/tag-label), clicking \"Save\" writes the main label CSV for this tag (e.g. {tag-id}-labeled.csv)."
          ),
          shiny::tags$li(
            "While you are editing labels and auto-save is enabled, a backup CSV is also written automatically about once per minute in the same folder (file named {tag-id}-labeled-backup.csv)."
          ),
          shiny::tags$li(
            "If no auto-save folder is available, use the \"Download\" button to manually save a CSV of the current labels."
          )
        ),
        shiny::h4("More information"),
        shiny::tags$p(
          "For detailed labelling instructions, see the ",
          shiny::tags$a(
            href = "https://geopressure.org/GeoPressureManual/labelling-tracks.html",
            target = "_blank",
            "GeoPressure Manual: labelling tracks"
          ),
          "."
        )
      )
    )
  )
})
