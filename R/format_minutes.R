#' @noRd
format_minutes <- function(mins) {
  mins <- round(mins)
  years <- mins %/% (365 * 24 * 60)
  days <- (mins %% (365 * 24 * 60)) %/% (24 * 60)
  hours <- (mins %% (24 * 60)) %/% 60
  minutes <- mins %% 60

  glue::glue(
    "{if (years > 0) glue::glue('{years}y ') else ''}",
    "{if (days > 0) glue::glue('{days}d ') else ''}",
    "{if (hours > 0 || days > 0 || years > 0) glue::glue('{hours}h ') else ''}",
    "{minutes}min"
  )
}
