#' @title geom_spiral
#' @description geom_spiral laver et spiral plot.
#' @details details here
#' @param param 1
#' @rdname geom_spiral
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
