# save_merged_ratings_data.R
#
# Creates a merged ratings data file and exports it.
#
# Args:
#
# Returns:
#
# -------------------------------------------------------------

library(tidyverse)

peep2_data_dir <- "~/Box\ Sync/b-peep-project\ Shared/PEEP2\ data/PEEP2\ Home\ visit\ behavioural\ data/"

# List files and open one for inspection
peep2_flist <- list.files(path = peep2_data_dir, 
                          pattern = "\\.csv$", 
                          full.names = TRUE)
peep2_df_list <- lapply(peep2_flist, read.csv)
peep2_df <- Reduce(function(x,y) merge(x,y, all =TRUE), peep2_df_list)

peep_2_df <- peep2_df %>%
  rename(scared_rating = scared.rating)

write_csv(peep_2_df, path = "data/csv/peep-II-ratings.csv")
