# code to prepare internal data for generation of sciences and journaltitles

# Sciences - scientific journalnames
library(tidyverse)

# Adjectives and sciences
adjectives <- read.csv("data-raw/adjectives.csv")
usethis::use_data(adjectives, adjectives, internal = TRUE)



sciences <- read.csv("data-raw/sciences.csv")
usethis::use_data(sciences, sciences, internal = TRUE)





# tidsskriftmønstre:

journal_patterns <- tribble(~pattern,
  "the {adjective} {discipline} journal",
  "The journal of {adjective} {discipline}",
  "{adjective} {discipline} magazine",
  "{adjective} {discipline} notes",
  "the journal of {adjective} {discipline} studies",
  "journal of {adjective} {discipline}",
  "journal of {adjective} {discipline} research",
  "The review of {adjective} {discipline}",
  "The {adjective} {discipline} quarterly",
  "{adjective} {discipline} quarterly",
  "{adjective} {discipline} studies",
  "quarterly reviews of {adjective} {discipline}",
  "{adjective} {discipline} news"
)
usethis::use_data(journal_patterns, journal_patterns, internal = TRUE)
