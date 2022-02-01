#' @title Negated %in%
#' @description Negated %in%
#' @details details here
#' @param param 1
#' @param param 2
#' @rdname `%nin%`
#' @export
`%nin%` <- function (x, table) {
  match(x, table, nomatch = 0L) == 0L
}
