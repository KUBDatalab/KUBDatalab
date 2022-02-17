#' @title generate_sciences()
#' @description generate_sciences returns a tibble with fictional scientific disciplines
#' @details details here - vi skal huske at kreditere lloyd
#' @param n number of scientific disciplines returned
#' @param subjects logical value indicating wether the major subject of the discipline should be returned
#' @references L. James, ESG, (2021), GitHub repository, https://github.com/ltjames/ESG
#' @rdname generate_sciences
#' @export

# A list of sciences and a list of scientific adjectives (with information) on
# parent scientific field, are combined, and a tibble of random, usually
# none-sensical scientific disciplines are returned.

generate_sciences <- function(n=10L, subjects=T){
  if (is.numeric(n) == FALSE) {
    stop(sprintf("Error - n must be a number between 1 and %s", nrow(sciences)*nrow(adjectives) ))
  }
  if (!between(n, 1, nrow(sciences)*nrow(adjectives))){
    stop(sprintf("Error - n must be an number between 1 and %s", nrow(sciences)*nrow(adjectives) ))
  }

  # generate array indeces for an array with dimensions nrow(sciences) x nrow(adjectives)
  # A sample of integers between 1 and nrow(sciences)*nrow(adjectives) is taken.
  # Each integer points to one cell in the array.
  arr <- arrayInd(sample(nrow(sciences)*nrow(adjectives), n), c(nrow(sciences),nrow(adjectives)))

  # The array indeces are used to subset rows from adjective and sciences - they
  # are collected to a tibble, adjective and discipline concatenated
  # The discipline is returned in title case
  resultat <- tibble(adjective = adjectives[arr[,2],2],
         subject = adjectives[arr[,2],1],
         discipline = sciences[arr[,1],] ) %>%
    mutate(discipline = str_c(adjective, discipline, sep=" ")) %>%
    mutate(discipline = str_to_title(discipline)) %>%
    select(discipline, subject)

  # If subjects = F, scientific subject is removed
  if(!subjects){
    resultat <- select(resultat, discipline)
  }
  return(resultat)
}
