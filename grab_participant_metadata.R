# grab_participant_metadata.R
#
# Extracts the relevant participant metadata from a shared
# Google Sheet used for the project.
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

write_csv(peep2_demo, path = "data/csv/peep2-demo.csv")
