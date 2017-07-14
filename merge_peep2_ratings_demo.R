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

try(
  ratings <- read_csv("data/csv/peep2-ratings.csv"),
  stop("Problem reading peep2-ratings.csv")
  )

try(
  demo <- read_csv("data/csv/peep2-demo.csv"),
  stop("Problem reading peep2-demo.csv")
)  

# demo contains `ID Number` as char; ratings has fam_id as numeric
demo %>%
  mutate(fam_id = as.numeric(`ID Number`)) ->
  demo

ratings_demo <- merge(ratings, demo, by="fam_id")

write_csv(ratings_demo, "data/csv/peep2-ratings-demo.csv")