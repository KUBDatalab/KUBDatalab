#' @title generate_sciences()
#' @description generate_sciences returns a tibble with fictional scientific disciplines
#' @details details here - vi skal huske at kreditere lloyd
#' @param n number of scientific disciplines returned
#' @param subjects logical value indicating wether the major subject of the discipline should be returned
#' @references L. James, ESG, (2021), GitHub repository, https://github.com/ltjames/ESG
#' @rdname generate_sciences
#' @export

generate_sciences <- function(n=10, subjects=T){
  resultat <- cbind(slice_sample(adjectives, n=n),
                    slice_sample(sciences, n=n)
  ) %>%
    mutate(discipline = str_c(adjective, discipline, sep=" ")) %>%
    mutate(discipline = str_to_title(discipline)) %>%
    select(discipline, subject)
  if(!subjects){
    resultat <- select(resultat, discipline)
  }
  return(resultat)
}

# nrow(sciences)*nrow(adjectives)

