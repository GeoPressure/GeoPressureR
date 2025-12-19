# Shortcuts/help modal. Sourced inside `server()`.

observeEvent(input$shortcuts_help, {
  showModal(
    modalDialog(
      title = "Keyboard and mouse shortcuts",
      easyClose = TRUE,
      size = "l",
      footer = modalButton("Close"),
      tagList(
        h4("Labeling"),
        tags$ul(
          tags$li("Click or drag on the plot to apply the selected label."),
          tags$li(
            tagList(
              tags$kbd("Ctrl"),
              " (Windows / Linux) or ",
              tags$kbd("Cmd"),
              " (macOS) + mouse click/selection: clear labels instead of applying a new one."
            )
          )
        ),
        h4("Time navigation (x-axis)"),
        tags$ul(
          tags$li(tagList(tags$kbd("Mouse wheel"), " over plot area: zoom in/out in time.")),
          tags$li(tagList(
            tags$kbd("\u2191"),
            " / ",
            tags$kbd("\u2193"),
            ": zoom in / out around the current view."
          )),
          tags$li(tagList(
            tags$kbd("\u2190"),
            " / ",
            tags$kbd("\u2192"),
            ": pan left / right in time."
          )),
          tags$li(tagList(
            tags$kbd("Shift"),
            " + ",
            tags$kbd("\u2190"),
            " / ",
            tags$kbd("\u2192"),
            ": pan by a full window width."
          ))
        ),
        h4("Y-axis zoom"),
        tags$ul(
          tags$li(tagList(tags$kbd("Mouse wheel"), " over left y-axis: zoom pressure axis.")),
          tags$li(tagList(
            tags$kbd("Mouse wheel"),
            " over right y-axis: zoom acceleration axis (when available)."
          ))
        ),
        h4("STAP navigation"),
        tags$ul(
          tags$li(tagList(
            "Use the ",
            tags$kbd("<"),
            " and ",
            tags$kbd(">"),
            " buttons next to \"Stap ID\" to move between stationary periods."
          ))
        ),
        h4("Auto-save and files"),
        tags$ul(
          tags$li(
            "If an automatic label folder is configured (e.g. data/tag-label), clicking \"Save\" writes the main label CSV for this tag (e.g. {tag-id}-labeled.csv)."
          ),
          tags$li(
            "While you are editing labels and auto-save is enabled, a backup CSV is also written automatically about once per minute in the same folder (file named {tag-id}-labeled-backup.csv)."
          ),
          tags$li(
            "If no auto-save folder is available, use the \"Download\" button to manually save a CSV of the current labels."
          )
        ),
        h4("More information"),
        tags$p(
          "For detailed labelling instructions, see the ",
          tags$a(
            href = "https://raphaelnussbaumer.com/GeoPressureManual/labelling-tracks.html",
            target = "_blank",
            "GeoPressure Manual: labelling tracks"
          ),
          "."
        )
      )
    )
  )
})
