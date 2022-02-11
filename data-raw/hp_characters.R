# Code to prepare Potter-data

library(tidyverse)
library(tidywikidatar)


jkr_df_query <- tibble::tribble(
  ~p, ~q,
  "P170", "Q34660",
  "P31", "Q3658341"

)

jkr_df <- tw_query(query = jkr_df_query)

# Så henter vi - næsten - hele svineriet på dem

jkr_all_properties <- tw_get(id = jkr_df)

sex <- jkr_all_properties %>%
  filter(property=="P21")

# Instance (P31)
# Slytherin:
# Q33297169
# gryffindor
# Q33179355
# ravenclaw:
#   Q33295822
#
# hufflepuff
# Q33296715

houses <- c("Q33296715", "Q33295822", "Q33179355", "Q33297169")

huse <- jkr_all_properties %>%
  filter(property == "P31") %>%
  filter(value %in% houses)

samlet <- jkr_df %>%
  left_join(sex) %>%
  select(-property, -rank) %>%
  rename(sex = value) %>%
  left_join(huse) %>%
  select(-property, -rank) %>%
  rename(house = value) %>%
  filter(!is.na(hus))


samlet <- samlet %>%
  mutate(sex = recode(sex,
                      "Q6581072" = "female",
                      "Q6581097" = "male")) %>%
  mutate(hus = recode(hus,
                      "Q33297169" = "Slytherin",
                      "Q33179355" = "gryffindor",
                      "Q33295822"  = "ravenclaw",
                      "Q33296715" = "hufflepuff"
  ))
hp_characters <-samlet %>%
  select(-id, -description) %>%
  rename(name = label)

usethis::use_data(hp_characters, overwrite = TRUE)
#' @name hp_characters
#' @export
"hp_characters"

