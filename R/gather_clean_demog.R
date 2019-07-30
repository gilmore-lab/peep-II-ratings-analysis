# gather_clean_demog.R
#
# Gather and clean the demographic data

update_peep_II_demog <- function() {
  save_peep_II_demog(
    clean_peep_II_demog(
      gather_peep_II_demo()))
}

gather_peep_II_demo <- function(dropped_subs = "007") {
  gs <- googlesheets::gs_title("PEEP II Data Management")
  
  df <- googlesheets::gs_read(ss = gs, ws = 1)
  
  df <- dplyr::filter(df, `Completed/Scheduled Visit 2` == "completed",
                      `Completed/Scheduled EAR Day` == "completed",
                      `Completed/Scheduled Visit 3` == "completed",
                      !(`ID Number` %in% dropped_subs))
  
  df <- dplyr::select(df, `ID Number`, `Two Parent Family?`, 
                      `Mother Gender`,`Mother Race`, `Mother Latina?`,
                      `Other Parent Gender`, `Other Parent Race`,
                      `Other Parent Latino?`,
                      `Target Child DOB`, `Target Child Gender`,
                      `Target Child Race`, `Target Child Latino?`)
  df
}

clean_peep_II_demog <- function(df) {
  df <- select_demog_subset(df)
  df <- rename_demog_vars(df)
  df <- harmonize_demog_vars(df)
  df
}

save_peep_II_demog <- function(df, fn = "data/csv/peep-II-demog.csv") {
  readr::write_csv(df, fn)
  message(paste0("New demographics file written to ", "'", fn, "'"))
}

select_demog_subset <- function(df) {
  df <- dplyr::select(df,
                      `ID Number`,
                      `Two Parent Family?`,
                      `Mother Gender`,
                      `Mother Race`,
                      `Mother Latina?`,
                      `Other Parent Gender`,
                      `Other Parent Race`,
                      `Other Parent Latino?`,
                      `Target Child DOB`,
                      `Target Child Gender`,
                      `Target Child Race`,
                      `Target Child Latino?`)
  df
}

rename_demog_vars <- function(df) {
  df <- dplyr::rename(df,
                      fam_id = `ID Number`,
                      two_parent_fam = `Two Parent Family?`,
                      child_dob = `Target Child DOB`,
                      child_gender = `Target Child Gender`,
                      child_race = `Target Child Race`,
                      child_latinx = `Target Child Latino?`,
                      mother_gender = `Mother Gender`,
                      mother_race = `Mother Race`,
                      mother_latinx = `Mother Latina?`,
                      other_gender = `Other Parent Gender`,
                      other_race = `Other Parent Race`,
                      other_latinx = `Other Parent Latino?`)
  df
}

harmonize_demog_vars <- function(df) {
  df <- dplyr::mutate(df, fam_id = as.numeric(fam_id),
                      two_parent_fam = tolower(two_parent_fam),
                      mother_gender = tolower(mother_gender),
                      mother_race = tolower(mother_race),
                      mother_latinx = tolower(mother_latinx),
                      other_gender = tolower(other_gender),
                      other_race = tolower(other_race),
                      other_latinx = tolower(other_latinx),
                      child_dob = lubridate::mdy(child_dob),
                      child_gender = tolower(child_gender),
                      child_race = tolower(child_race),
                      child_latinx = tolower(child_latinx))
  df
}

