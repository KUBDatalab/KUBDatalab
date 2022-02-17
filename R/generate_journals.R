#' @title generate_journals()
#' @description generate_journals returns a tibble with fictional names of scientific journals
#' @details Based on an idea (and data collected) by Lloyd James a table of
#' scientific adjectives is combined with a table of scientific disciplines. These
#' are glued together with common patterns of scientific journal titles.
#'
#' Currently there are no checks for duplicated journal names, and the upper
#' limit of journal names that can be returned is 698.
#'
#' The assignment of subjects to the journals are controlled by the adjectives,
#' and any instances of internal consistency between adjective, discipline and
#' subject are purely coincidental.
#'
#' @param n number of journals returned
#' @param subjects logical value indicating wether the subject of the journal should be returned
#' @references L. James, ESG, (2021), GitHub repository, https://github.com/ltjames/ESG
#' @rdname generate_journals
#' @export

generate_journals <- function(n=10, subjects=T){
  # testing input
  if (is.numeric(n) == FALSE) {
    stop(sprintf("Error - n must be a number between 1 and %s", nrow(sciences)*nrow(adjectives)*nrow(journal_patterns)))
  }
  if (!between(n, 1, nrow(sciences)*nrow(adjectives))){
    stop(sprintf("Error - n must be an number between 1 and %s", nrow(sciences)*nrow(adjectives)*nrow(journal_patterns)))
  }
  # generate array indeces for an array with dimensions nrow(sciences) x nrow(adjectives)
  # A sample of integers between 1 and nrow(sciences)*nrow(adjectives) is taken.
  # Each integer points to one cell in the array.
  arr <- arrayInd(sample(nrow(sciences)*nrow(adjectives), n), c(nrow(sciences),nrow(adjectives), nrow(journal_patterns)))

  # The array indeces are used to subset rows from adjective, sciences and
  # journal patterns. - they are collected to a tibble, adjective and
  # discipline glued into the journal pattern
  # The journal title is returned in title case

  resultat <- tibble(adjective = adjectives[arr[,2],2],
                     subject = adjectives[arr[,2],1],
                     discipline = sciences[arr[,1],],
                     pattern = journal_patterns[arr[,3],]) %>%
    rowwise() %>%
    mutate(resultat = glue::glue(pattern)) %>%
    ungroup() %>%
    mutate(journal_title = str_to_title(resultat)) %>%
    select(journal_title, subject)
  if(!subjects){
    resultat <- select(resultat, journal_title)
  }
  return(resultat)
}
