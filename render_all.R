# render_all.R
#
# Runs all analyses for all participants based on
# the by-participant.Rmd file. Use `source("render_all.R")`
#
# Args:
#
# Returns:
#
#--------------------------------------------------------------
library(stringr)
library(tidyverse)

#--------------------------------------------------------------
# Define function to render individual files
render_peep_participant <- function(fam_id) {
    require(stringr) # for str_pad()
    fam_id_pad <- str_pad(fam_id, 2, pad = "0")
    rmarkdown::render("by-participant.Rmd",
                      params = list(this_fam = as.character(fam_id)),
                      output_file = paste0("participant_", fam_id_pad, ".md"),
                      output_dir = "indiv_rpts")
}

#--------------------------------------------------------------
# Load data
peep2_data_dir <- "~/Box\ Sync/b-peep-project\ Shared/PEEP2\ data/PEEP2\ Home\ visit\ behavioural\ data/"
peep2_flist <- list.files(path = peep2_data_dir, 
                          pattern = "\\.csv$", 
                          full.names = TRUE)
peep2_df_list <- lapply(peep2_flist, read.csv)
peep2_df <- Reduce(function(x,y) merge(x,y, all =TRUE),
                   peep2_df_list)

#--------------------------------------------------------------
# Get unique participants before running render function
fam_ids <- unique(peep2_df$fam_id)
lapply(fam_ids, render_peep_participant)