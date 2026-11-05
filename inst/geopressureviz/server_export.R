# CSV export of the edited path. Sourced inside `server()`.

output$export_path <- shiny::downloadHandler(
  filename = function() {
    glue::glue("{tag$param$id}-path-geopressureviz.csv")
  },
  content = function(file) {
    utils::write.csv(
      reactVal$path,
      file = file,
      row.names = FALSE
    )
  }
)
