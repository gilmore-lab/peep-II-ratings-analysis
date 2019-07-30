# gather_clean_ratings.R
# 
# Functions to gather and clean the PEEP II data

update_peep_II_ratings <- function() {
  save_peep_II_ratings(
    clean_peep_II_ratings(
      gather_peep_II_ratings()))
}

gather_peep_II_ratings <- function() {
  data_dir <- "~/Box\ Sync/b-peep-project\ Shared/PEEP2\ data/PEEP2\ Home\ visit\ behavioural\ data/"
  
  # List files and open one for inspection
  flist <- list.files(path = data_dir, 
                      pattern = "\\.csv$", 
                      full.names = TRUE)
  ratings_list <- lapply(flist, readr::read_csv)
  ratings <- Reduce(function(x,y) merge(x,y, all =TRUE), ratings_list)
}

clean_peep_II_ratings <- function(df) {
  df <- rename_scared_rating(df)
  df <- extract_data_from_snd_file(df)
  df <- create_spkr_fam(df)
  df <- recode_how_feel(df)
  df <- make_ids_numeric(df)
  df
}

save_peep_II_ratings <- function(df, fn = "data/csv/peep-II-ratings.csv") {
  readr::write_csv(df, fn)
  message(paste0("New ratings file written to ", "'", fn, "'"))
}

rename_scared_rating <- function(df) {
  if ("scared-rating" %in% names(df)) {
    df <- rename(df, scared_rating = `scared-rating`)
  }
  df
}

extract_data_from_snd_file <- function(df) {
  snd_file_regex <- "([0-9]+)-([a-z]+)-([a-z]+)-([ab])"
  snd_file_extract <- stringr::str_match(df$snd_file, snd_file_regex)
  
  df <- dplyr::mutate(df, 
                      speaker = as.numeric(snd_file_extract[,2]),
                      prosody = snd_file_extract[,3],
                      script  = snd_file_extract[,4],
                      order   = snd_file_extract[,5])
  df
}

create_spkr_fam <- function(df) {
  df <- dplyr::mutate(df,
               familiar_bool = (as.numeric(fam_id) == as.numeric(speaker)),
               spkr_fam = if_else(familiar_bool, 'fam', 'nov'))
  df <- dplyr::select(df, -familiar_bool)
}

recode_how_feel <- function(df) {
  f <- factor(df$how_feel)
  
  # There are some missing ratings that have 0 values; recode to NA
  levels(f) <- c(NA, 'neu', 'hap', 'ang', 'sad', 'sca')
  df$how_feel <- f
  df
}

make_ids_numeric <- function(df) {
  df <- mutate(df, fam_id = as.numeric(fam_id),
               nov_id = as.numeric(nov_id))
  df
}
