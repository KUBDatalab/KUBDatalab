#' @title geom_spiral
#' @description geom_spiral laver et spiral plot.
#' @details details here
#' @param mapping beskrivelse
#' @param data beskrivelse
#' @param geom skal nok væk
#' @param na.rm skal nok væk
#' @param show.legend viser legend
#' @rdname geom_spiral
#' @importFrom ggplot2 layer
#' @export
geom_spiral <- function(mapping = NULL, data = NULL, geom = "path",
position = "identity", na.rm = FALSE, show.legend = NA,
inherit.aes = TRUE, n = 12, ...) {
  layer(
    stat = StatSpiral, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, n = n, ...)
  )
}
