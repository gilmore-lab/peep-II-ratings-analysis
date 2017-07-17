# check_gender_age
#
# Extracts the relevant participant metadata from a shared
# Google Sheet used for the project. Loads a second participant
# metadata file with age at test info. Compares them and saves
# an audit file data/csv/gender-age-checks.csv.
#
# Args:
#
# Returns:
#
# -------------------------------------------------------------

library(googlesheets)
library(tidyverse)

dropped_subs <- "007"

peep2 <- gs_title("PEEP II Data Management")

# Only need some of Demographics data (sheet = 1)
peep2 %>%
  gs_read(ws = 1) %>%
  filter(`Completed/Scheduled Visit 2` == "completed",
         `Completed/Scheduled EAR Day` == "completed",
         `Completed/Scheduled Visit 3` == "completed",
         !(`ID Number` %in% dropped_subs)) %>%
  select(`ID Number`, `Two Parent Family?`, 
         `Mother Gender`,`Mother Race`, `Mother Latina?`,
         `Other Parent Gender`, `Other Parent Race`,
         `Other Parent Latino?`,
         `Target Child DOB`, `Target Child Gender`,
         `Target Child Race`, `Target Child Latino?`) ->
  peep2_demo

# Data file Pan sent & Rick trimmed
peep2_age <- read_csv("data/csv/peep2-age.csv")

# peep2_demo$`ID Number` is char, peep2_age$fam_id is numeric
peep2_demo$`ID Number` <- as.numeric(peep2_demo$`ID Number`)

peep2_demo_age <- merge(peep2_demo, peep2_age, 
                        by.x = "ID Number", 
                        by.y = "fam_id")

peep2_demo_age %>%
  select(`ID Number`, `Target Child Gender`, gender )

peep2_demo_age$gender <- tolower(peep2_demo_age$gender)
peep2_demo_age$`Target Child Gender` <- tolower(peep2_demo_age$`Target Child Gender`)

peep2_demo_age %>%
  select(`ID Number`, `Target Child Gender`, gender, age_yrs ) %>%
  mutate( gender_matches = `Target Child Gender` == gender) %>%
  mutate( missing_age = is.na(age_yrs)) ->
  gender_age_checks

write_csv(gender_age_checks, "data/csv/gender-age-checks.csv")




