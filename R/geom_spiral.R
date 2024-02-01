#' @title geom_spiral
#' @description geom_spiral laver et spiral plot.
#' @details details here
#' @param mapping beskrivelse
#' @param data beskrivelse
#' @param geom skal nok væk
#' @param na.rm skal nok væk
#' @param show.legend viser legend
#' @param n Antal omdrejninger på spiralen.
#' @param ... de der ekstra ting der kan sendes med
#' @rdname geom_spiral
#' @importFrom ggplot2 layer
#' @export
geom_spiral <- function(mapping = NULL, data = NULL, geom = "path",
 na.rm = FALSE, show.legend = NA,
 n = 12, ...) {
  position = "identity"
  inherit.aes = TRUE
  layer(
    stat = StatSpiral, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, n = n, ...)
  )
}
