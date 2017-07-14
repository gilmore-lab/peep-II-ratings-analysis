# merge_peep2_ratings_demo.R
# 
# Merges the ratings data and the demographic data
#
# Args:
#
# Returns:
#
#---------------------------------------------------------------

require(tidyverse)

ratings <- read_csv("data/csv/peep2-ratings.csv")
demo <- read_csv("data/csv/peep2-demo.csv")

# demo contains `ID Number` as char
demo %>%
  mutate(fam_id = as.numeric(`ID Number`)) ->
  demo

ratings_demo <- merge(ratings, demo, by="fam_id")

write_csv(ratings_demo, "data/csv/peep2-ratings-demo.csv")