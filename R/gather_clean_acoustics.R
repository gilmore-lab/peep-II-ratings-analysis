# gather_clean_acoustics.R
# 
# Gather and clean the acoustics data

extract_acoustics_data_from_matlab <- function(fn) {
  ac_mat <- R.matlab::readMat(fn)
  
  # Convert to data frame
  ac_dat <- as.data.frame(ac_mat$data)
  
  # Convert data to 1D array
  v <- NULL
  for (i in 1:16) {
    v <- c(v, ac_dat[,i])
  }
  
  # Define values for nominal variables
  scr <- rep(rep(c("chk-a", "chk-b", "din-a", "din-b", 
                   "hlp-a", "hlp-b", "tlk-a", "tlk-b"), times = 4), times = 4)
  emo <- rep(rep(c("ang", "hap", "neu", "sad"), each = 8), times = 4)
  utt <- rep(1:4, each = 32)
  
  # Assemble data frame and return
  df <- data.frame(data.frame(measure = v, script = scr, 
                              prosody = emo, utterance = utt))
  
  message(paste0("Data extracted from '", fn, "'"))
  dplyr::arrange(df, script, prosody, utterance)
}

clean_acoustics_data_names <- function(df, measure_nm = "measure") {
  # Rename measure (hacky)
  df[measure_nm] <- df$measure
  df <- dplyr::select(df, -measure)
  
  # Create separate script and order variables
  df <- tidyr::separate(df, script, into = c("script", "order"))
  df
}

test_gather_clean_acoustics <- function(this_fam = "001", 
                                        this_measure_nm = "F0_mean") {
  fn <- construct_acoustics_file_path(this_fam, this_measure_nm)
  df <- extract_acoustics_data_from_matlab(fn)
  df <- clean_acoustics_data_names(df, measure_nm = this_measure_nm)
  df
}

construct_acoustics_file_path <- function(fam_id = "001", this_measure = "F0_mean") {
  data_fn <- paste0("~/Box Sync/b-peep-project Shared/PEEP2 acoustics/", fam_id, "/data")
  
  path2file = NULL
  
  # This could be less hacky, but at least it's clear
  if (this_measure == "F0_mean") {
    path2file <- "/F0/F0/MEAN/F0.mat"
  } else if (this_measure == "F0_contours") {
    path2file <- "/F0/F0/CONTOURS/F0.mat"
  } else if (this_measure == "HNR_mean") {
    path2file <- "/HNR/HNR/MEAN/HNR.mat"
  } else if (this_measure == "HNR_contours") {
    path2file <- "/HNR/HNR/CONTOURS/HNR.mat"
  }
  
  # Stoop et al. ms use F0_mean, F0_sd, and speech rate
  
  if (is.null(path2file)) {
    stop("Unknown measure name.")
  }
  paste0(data_fn, path2file)
}
