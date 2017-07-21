export-ratings-time-series-for-fmri
================
Rick Gilmore
2017-07-21 13:51:00

Aims
----

This document describes the process of exporting a set of TR by TR (volume by volume) time series of the participants ratings of each script. The goal is to create a file that can be used to analyze the post hoc ratings as a continuous regressor in the BOLD fMRI time series data. There will either be one file with all ratings--angry, happy, sad, scared--for each participant and run or separate files for each rating type, participant, and run.

Inputs and outputs
------------------

The ratings data files in the project Box folder are the inputs. Other code in this repo can be used to import and manipulate these files.

The outputs need to be in an `*.sdm` format used by Brain Voyager. Based on the `001_O4R1_3DMC.sdm` sample file provided by Pan Liu, it looks like we could produce one, tab-delimited output file with a header row containing R G B vectors for the plot color, a second header row containing the variable names enclosed in double quotations, and then rows for each volume in the MR time series.

The design of the study is as follows. Each sound played for 10 s (5 x 2s/TR), followed by a 6s (3 x 2s TR) silent period. There were 33 sounds played per run. There were 2 discarded data acquisition volumes (DISDAQs) before the first sound file began with volume 3. The last volume index was 266. Thirty-three sounds x (10 s + 6 s)/sound divided by 2 s/volume is 264 volumes. 264 + the 2 initial discards gives 266 volumes.

I suggest we use `<fam_id>-<run0x>-ratings-timeseries.sdm` or `<fam_id>-<run0x>-<rating-type>-timeseries.sdm` as the output file naming format. I also suggest we use `angry-rating`, `happy-rating`, `sad-rating`, and `scared-rating` as the variable names. All ratings fall on a \[1,4\] scale, so we will assign 0 to the volumes where no sound was played.

Import ratings data for one participant
---------------------------------------

``` r
peep2_data_dir <- "~/Box\ Sync/b-peep-project\ Shared/PEEP2\ data/PEEP2\ Home\ visit\ behavioural\ data/"

import_peep2_ratings <- function(fn) {
  try(
    read.csv(fn),
    stop(sprintf("Cannot read %\n", fn))
  )
}

convert_ratings_to_ts <- function(rating) {
  # Takes [1,33] vector of ratings and
  # converts to [1,266] vector with duplicates and 0s
  # as appropriate
  
  z <- rep(0, 2*33)
  zz <- z 
  zz[seq(1,66, by=2)] <- t(rating)
  t <- rep(zz, rep(c(5,3), 33))
  ts <- rep(0, 266)
  ts[3:266] <- t
  ts
}

# Test for one file in list
fl <- list.files(peep2_data_dir, "\\.csv$", full.names = TRUE)
ratings <- import_peep2_ratings(fl[1])
convert_ratings_to_ts(ratings['angry_rating'])
```

    ##   [1] 0 0 2 2 2 2 2 0 0 0 1 1 1 1 1 0 0 0 4 4 4 4 4 0 0 0 2 2 2 2 2 0 0 0 2
    ##  [36] 2 2 2 2 0 0 0 4 4 4 4 4 0 0 0 3 3 3 3 3 0 0 0 2 2 2 2 2 0 0 0 2 2 2 2
    ##  [71] 2 0 0 0 2 2 2 2 2 0 0 0 2 2 2 2 2 0 0 0 3 3 3 3 3 0 0 0 2 2 2 2 2 0 0
    ## [106] 0 4 4 4 4 4 0 0 0 2 2 2 2 2 0 0 0 1 1 1 1 1 0 0 0 2 2 2 2 2 0 0 0 2 2
    ## [141] 2 2 2 0 0 0 3 3 3 3 3 0 0 0 3 3 3 3 3 0 0 0 3 3 3 3 3 0 0 0 1 1 1 1 1
    ## [176] 0 0 0 3 3 3 3 3 0 0 0 3 3 3 3 3 0 0 0 2 2 2 2 2 0 0 0 2 2 2 2 2 0 0 0
    ## [211] 3 3 3 3 3 0 0 0 3 3 3 3 3 0 0 0 2 2 2 2 2 0 0 0 1 1 1 1 1 0 0 0 3 3 3
    ## [246] 3 3 0 0 0 1 1 1 1 1 0 0 0 2 2 2 2 2 0 0 0

Next, let's create the export file for a single rating.

``` r
outfn <- "angry-rating.tsv"
ar <- convert_ratings_to_ts(ratings['angry_rating'])
cat("255 0 0\n", file = outfn)
cat(sprintf("%s\n", 'angry_rating'), file = outfn, append = TRUE)
for (i in 1:266) {
  cat(sprintf("%d\n", ar[i]), file = outfn, append = TRUE)
}
```

Export file for all four ratings.

``` r
outfn <- "all-ratings.tsv"

ratings_in_file <- (names(ratings) %in% c("happy_rating",
                                          "angry_rating",
                                          "sad_rating",
                                          "scared_rating"))
ratings_to_write <- ratings[ratings_in_file]

line_colors <- c(red = "255 0 0",
                 green = "0 255 0",
                 blue = "0 0 255",
                 black = "0 0 0")

# create file anew with this line
for (c in 1:dim(ratings_to_write)[2]) {
  cat(sprintf("%s\t", line_colors[c]), 
      file = outfn, append = TRUE)
}
cat(sprintf("\n"), file = outfn, append = TRUE)

for (c in 1:dim(ratings_to_write)[2]) {
  cat(sprintf("%s\t", names(ratings_to_write)[c]),
      file = outfn, append = TRUE)
}
cat(sprintf("\n"), file = outfn, append = TRUE)

for (i in 1:266) {
  for (c in 1:dim(ratings_to_write)[2]) {
    cat(sprintf("%s\t",
                convert_ratings_to_ts(ratings_to_write[c])[i]),
        file = outfn, append = TRUE)
  }
  cat(sprintf("\n"), file = outfn, append = TRUE)
} # for 
```

Now, let's turn this into a function.

``` r
make_outfn_name <- function(infn) {
  try(
    this_file <- read.csv(infn),
    stop(sprintf("Cannot read %\n", infn))
  )
  if ("fam_id" %in% names(this_file)) {
    out_fn <- paste0(sprintf("%03d", unique(this_file$fam_id)),
                     "-",
                     sprintf("run%02d", unique(this_file$run)),
                     "-ratings-timeseries.sdm")
  } else stop(sprintf("Can't read fam_id in %s", fn))
  out_fn
}

make_ratings_matrix <- function(out_fn, these_ratings) {
  ratings_in_file <- (names(ratings) %in% c("happy_rating",
                                            "angry_rating",
                                            "sad_rating",
                                            "scared_rating"))
  ratings_to_write <- ratings[ratings_in_file]
  
  line_colors <- c(red = "255 0 0",
                   green = "0 255 0",
                   blue = "0 0 255",
                   black = "0 0 0")
  
  # create file anew with this line
  cat("", file = out_fn)
  
  for (c in 1:dim(ratings_to_write)[2]) {
    cat(sprintf("%s\t", line_colors[c]), 
        file = out_fn, append = TRUE)
  }
  cat(sprintf("\n"), file = out_fn, append = TRUE)
  
  for (c in 1:dim(ratings_to_write)[2]) {
    cat(sprintf("%s\t", names(ratings_to_write)[c]),
        file = out_fn, append = TRUE)
  }
  cat(sprintf("\n"), file = out_fn, append = TRUE)
  
  for (i in 1:266) {
    for (c in 1:dim(ratings_to_write)[2]) {
      cat(sprintf("%s\t",
                convert_ratings_to_ts(ratings_to_write[c])[i]),
          file = out_fn, append = TRUE)
    }
    cat(sprintf("\n"), file = out_fn, append = TRUE)
  }   
}

make_ratings_file <- function(fn) {
  out_fn <- make_outfn_name(fn)
  these_ratings <- import_peep2_ratings(fn)
  make_ratings_matrix(out_fn, these_ratings)
}
```

Create test file to send to Pan.

``` r
make_ratings_file(fl[1])
```
