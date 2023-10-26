#' @title Negated in
#' @description nin returns a logical vector indicating if there is a match or not for its left operand
#' @details details here
#' @param x A vector of values to be matched.
#' @param table A vector of values to be matched against.
#' @return A logical vector indicating if there is a match or not for its left operand
#' @rdname nin
#' @export
`%nin%` <- function (x, table) {
  match(x, table, nomatch = 0L) == 0L
}
