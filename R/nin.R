#' @title Negated %in%
#' @description Negated %in%
#' @details details here
#' @param param 1
#' @param param 2
#' @examples
#' library(ggplot2)
#'
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_color_ucph() +
#'      theme_kubdatalab()
#' @rdname nin
#' @export

`%nin%` <- function (x, table) {
  match(x, table, nomatch = 0L) == 0L
}
