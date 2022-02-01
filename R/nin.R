#' @title Negated %in%
#' @description %nin% returns a logical vector indicating if there is a match or not for its left operand
#' @details details here
#' @param param 1
#' @param param 2
#' @rdname nin
#' @export
`%nin%` <- function (x, table) {
  match(x, table, nomatch = 0L) == 0L
}



