load("data-raw/sciences.rds")
load("data-raw/journal_patterns.rds")
load("data-raw/adjectives.rds")

usethis::use_data(adjectives, journal_patterns, sciences, internal = T, overwrite = T)
