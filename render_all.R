render_peep_participant <- function(fam_id) {
    rmarkdown::render("by-participant.Rmd",
                      params = list(this_fam = as.character(fam_id)),
                      output_file = paste0("participant_", fam_id, ".md"),
                      output_dir = "indiv_rpts")
}

# Load data
peep2.data.dir <- "~/Box\ Sync/b-peep-project\ Shared/PEEP2\ data/PEEP2\ Home\ visit\ behavioural\ data/"

# List files and open one for inspection
peep2.flist <- list.files(path = peep2.data.dir, 
                          pattern = "\\.csv$", 
                          full.names = TRUE)
peep2.df.list <- lapply(peep2.flist, read.csv)
peep2.df <- Reduce(function(x,y) merge(x,y, all =TRUE),
                   peep2.df.list)

fam_ids <- unique(peep2.df$fam_id)
lapply(fam_ids, render_peep_participant)