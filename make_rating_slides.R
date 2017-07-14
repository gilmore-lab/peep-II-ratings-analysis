# make_rating_slides.R
#
# Makes an *_presentation with all of the participants
# and most of the figures. We create a .R file first with
# special #' commenting that `rmarkdown::render` uses to convert
# lines to Markdown. This then creates a file we can use to 
# make our *_presentation.
#
# Args:
#
# Returns:
#
#--------------------------------------------------------------
library(stringr)

# Open file
fn <- "peep-II-individual-data-summaries.R"
file.create(fn)

# Write header matter
cat("#' ---\n", file = fn, append = TRUE)
cat("#' title: 'PEEP-II Ratings'\n", file = fn, append = TRUE)
cat("#' author: 'Rick O. Gilmore'\n", file = fn, append = TRUE)
cat("#' date: '`r Sys.time()`'\n", file = fn, append = TRUE)
cat("#' output: github_document\n", file = fn, append = TRUE)
cat("#' ---\n", file = fn, append = TRUE)

# Extract list of participants or directories
dl <- list.dirs("indiv_rpts", recursive = FALSE)

#--------------------------------------------------------------
# Create function to make slide for given participant and fig
make_slide <- function(this.dir, this.fig.name, fn) {
  this.participant <- str_extract(this.dir, "[0-9]+")
  
  cat("\n", file = fn, append = TRUE)
  cat("#' ## Participant ", file = fn, append = TRUE)
  cat(this.participant, file = fn, append = TRUE)
  cat("\n", file = fn, append = TRUE)
  
  cat("#' ", file = fn, append = TRUE)
  cat('<div class="centered">\n', file = fn, append = TRUE)
  
  cat("#' ", file = fn, append = TRUE)
  cat('<img src=', file = fn, append = TRUE)
  cat('"', file = fn, append = TRUE)
  path.2.img <- paste0(this.dir, "/", "figure-markdown_github/", this.fig.name)
  cat(path.2.img, file = fn, append = TRUE)
  cat('"/>\n', file = fn, append = TRUE)
  
  cat("#' ", file = fn, append = TRUE)
  cat('</div>\n\n', file = fn, append = TRUE)  
}

#--------------------------------------------------------------
# Create slides for each figure type in sequence.
# If we used the revealjs_presentation format, the H1 formatted
# slides would appear for horizontal navigation and the H2
# formatted slides would appear for vertical navigation.

# Time series figures
cat("#' # Time series of intensity ratings\n", file = fn, append = TRUE)
this.fig.name <- "time-series-rated-intensity-1.png"
lapply(dl, make_slide, this.fig.name, fn)

# Angry ratings
cat("#' # Ratings of angry prosodies\n", file = fn, append = TRUE)
this.fig.name <- "angry-ratings-1.png"
lapply(dl, make_slide, this.fig.name, fn)

# Happy ratings
cat("#' # Ratings of happy prosodies\n", file = fn, append = TRUE)
this.fig.name <- "happy-ratings-1.png"
lapply(dl, make_slide, this.fig.name, fn)

# Sad ratings
cat("#' # Ratings of sad prosodies\n", file = fn, append = TRUE)
this.fig.name <- "sad-ratings-1.png"
lapply(dl, make_slide, this.fig.name, fn)

# Neutral ratings
cat("#' # Ratings of neutral prosodies\n", file = fn, append = TRUE)
this.fig.name <- "neutral-ratings-1.png"
lapply(dl, make_slide, this.fig.name, fn)

# How feel
cat("#' # How feel by target prosody\n", file = fn, append = TRUE)
this.fig.name <- "how-feel-by-target-prosody-1.png"
lapply(dl, make_slide, this.fig.name, fn)

# Turn .R output into *_presentation
rmarkdown::render(fn, output_format = "revealjs::revealjs_presentation")