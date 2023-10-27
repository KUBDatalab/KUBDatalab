#' @title Mode
#' @description stat_mode returns the mode (most common value) in a vector
#' @details details here
#' @param x den vektor hvor vi vil have typetallet
#' @rdname stat_mode
#' @export
stat_mode <- function(x) {
  uniq <- unique(x)
  uniq[which.max(tabulate(match(x, uniq)))]
}
