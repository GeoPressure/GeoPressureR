# Download handlers for exporting data

# Setup export handlers
setup_export_handlers <- function(output, input, session, twl, stapath, .tag) {
  label_dir <- shiny::getShinyOption("label_dir")
  stap_dir <- shiny::getShinyOption("stap_dir")
  auto_label_save <- !is.null(label_dir) && dir.exists(label_dir)
  auto_stap_save <- !is.null(stap_dir) && dir.exists(stap_dir)

  session$onFlushed(
    function() {
      if (isTRUE(auto_label_save)) {
        shinyjs::show("save_twilight")
        shinyjs::hide("export_twilight")
      } else {
        shinyjs::hide("save_twilight")
        shinyjs::show("export_twilight")
      }
      if (isTRUE(auto_stap_save)) {
        shinyjs::show("save_stap")
        shinyjs::hide("export_stap")
      } else {
        shinyjs::hide("save_stap")
        shinyjs::show("export_stap")
      }
    },
    once = TRUE
  )

  observeEvent(input$save_twilight, {
    if (!isTRUE(auto_label_save)) {
      showNotification(
        "Automatic save is disabled for this session. Use the download dialog instead.",
        duration = 10,
        type = "error"
      )
      return()
    }

    target_file <- file.path(label_dir, glue::glue("{.tag$param$id}-labeled.csv"))
    tryCatch(
      {
        tag <- .tag
        tag$twilight <- twl()
        twilight_label_write(tag, file = target_file, quiet = TRUE)
        showNotification(
          glue::glue("Labels saved to {target_file}"),
          duration = 5,
          type = "message"
        )
      },
      error = function(e) {
        showNotification(
          glue::glue("Save failed: {e$message}. Using manual download instead."),
          duration = 10,
          type = "warning"
        )
        shinyjs::click("export_twilight")
      }
    )
  })

  observeEvent(input$save_stap, {
    if (!isTRUE(auto_stap_save)) {
      showNotification(
        "Automatic save is disabled for this session. Use the download dialog instead.",
        duration = 10,
        type = "error"
      )
      return()
    }

    target_file <- file.path(stap_dir, glue::glue("{.tag$param$id}.csv"))
    tryCatch(
      {
        utils::write.csv(stapath(), target_file, row.names = FALSE)
        showNotification(
          glue::glue("Stap saved to {target_file}"),
          duration = 5,
          type = "message"
        )
      },
      error = function(e) {
        showNotification(
          glue::glue("Save failed: {e$message}. Using manual download instead."),
          duration = 10,
          type = "warning"
        )
        shinyjs::click("export_stap")
      }
    )
  })

  # Export twilight data
  output$export_twilight <- shiny::downloadHandler(
    filename = function() {
      glue::glue("{.tag$param$id}-labeled.csv")
    },
    content = function(file) {
      tag <- .tag
      tag$twilight <- twl()
      twilight_label_write(tag, file = file, quiet = TRUE)
    }
  )

  # Export stapath data
  output$export_stap <- shiny::downloadHandler(
    filename = function() {
      glue::glue("{.tag$param$id}.csv")
    },
    content = function(file) {
      utils::write.csv(stapath(), file, row.names = FALSE)
    }
  )
}
