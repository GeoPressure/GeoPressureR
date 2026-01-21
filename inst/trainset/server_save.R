# Save/export + periodic backups. Sourced inside `server()`.

backup_timer <- reactiveTimer(60000, session) # every 60 seconds

shiny::observe({
  backup_timer()

  if (!isTRUE(auto_save_enabled)) {
    return()
  }

  if (!isTRUE(state$labels_dirty)) {
    return()
  }

  base_name <- tools::file_path_sans_ext(state$tag$param$id)
  backup_file <- file.path(label_dir, glue::glue("{base_name}-labeled-backup.csv"))

  try(
    {
      tmp <- tempfile(
        pattern = paste0(base_name, "-labeled-backup-"),
        fileext = ".csv",
        tmpdir = label_dir
      )
      write_labels_csv(tmp)

      # Atomic-ish replace: rename over the previous backup.
      if (file.exists(backup_file)) {
        file.remove(backup_file)
      }
      ok <- file.rename(tmp, backup_file)
      if (!isTRUE(ok)) {
        file.copy(tmp, backup_file, overwrite = TRUE)
        unlink(tmp)
      }
    },
    silent = TRUE
  )
})

shiny::observeEvent(input$save_btn, {
  if (!isTRUE(auto_save_enabled)) {
    shiny::showNotification(
      "Automatic save is disabled for this session. Use the download dialog instead.",
      duration = 10,
      type = "error"
    )
    return()
  }

  base_name <- tools::file_path_sans_ext(state$tag$param$id)
  target_file <- file.path(label_dir, glue::glue("{base_name}-labeled.csv"))

  tryCatch(
    {
      write_labels_csv(target_file)
      shiny::showNotification(
        glue::glue("Labels saved to {target_file}"),
        duration = 5,
        type = "message"
      )
    },
    error = function(e) {
      shiny::showNotification(
        glue::glue("Save failed: {e$message}. Using manual download instead."),
        duration = 10,
        type = "warning"
      )
      shinyjs::click("export_btn")
    }
  )
})

output$export_btn <- shiny::downloadHandler(
  filename = function() {
    base_name <- tools::file_path_sans_ext(state$tag$param$id)
    glue::glue("{base_name}-labeled.csv")
  },
  content = function(file) {
    tryCatch(
      {
        write_labels_csv(file)
      },
      error = function(e) {
        shiny::showNotification(
          glue::glue("Download failed: {e$message}"),
          duration = 10,
          type = "error"
        )
      }
    )
  },
  contentType = "text/csv"
)
