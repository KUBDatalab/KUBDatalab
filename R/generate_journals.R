#' @title generate_journals()
#' @description generate_journals returns a tibble with fictional names of scientific journals
#' @details details here - vi skal huske at kreditere lloyd
#' @param n number of journals returned
#' @param subjects logical value indicating wether the subject of the journal should be returned
#' @references L. James, ESG, (2021), GitHub repository, https://github.com/ltjames/ESG
#' @rdname generate_journals
#' @export

generate_journals <- function(n=10, subjects=T){
  resultat <- cbind(slice_sample(adjectives, n=n),
                slice_sample(sciences, n=n),
                slice_sample(journal_patterns, n=n, replace = T)
  ) %>%
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

# nrow(sciences)*nrow(adjectives)*nrow(journal_patterns)
