# Download handlers for exporting data

# Setup export handlers
setup_export_handlers <- function(output, twl, stapath, .tag) {
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
