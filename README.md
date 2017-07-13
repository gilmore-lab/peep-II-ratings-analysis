# PEEP-II-ratings-analysis

This repository contains files for the PEEP II project.
It focuses on visualizations and analyses of the behavioral ratings.
The [Databrary](http://databrary.org) volume for this study can be found at <https://nyu.databrary.org/volume/339>.
The MATLAB code for presenting audio recordings both in the magnet and outside it, where behavioral ratings were taken, can be found at <https://github.com/gilmore-lab/peep-II>.

## Contents

- Images shown to the participants to make intensity ratings in [HTML](ratings-facial-exemplars.nb.html), [Rmd](ratings-facial-exemplars.Rmd), and [Markdown](ratings-facial-exemplars.md) formats. To regenerate, run `rmarkdown::render("ratings-facial-exemplars.Rmd")`.
- Summary plots of ratings across participants in [HTML](analysis-summary-plots.nb.html), [Rmd](analysis-summary-plots.Rmd), and [Markdown](analysis-summary-plots.md) formats. To regenerate, run `rmarkdown::render("analysis-summary-plots.Rmd")`. Additional image files generated by this script can be found in [`analysis-summary-plots_files/`](analysis-summary-plots_files/).
- Preliminary exploratory work on a cluster analysis in [HTML](cluster-analysis.nb.html), [Rmd](cluster-analysis.Rmd), and [Markdown](cluster-analysis.md) formats. To regenerate, run `rmarkdown::render("cluster-analysis.Rmd")`. Additional images from this script can be found in [`cluster-analysis_files/`](cluster-analysis_files/).
- Files that generate a series of summary analyses on a participant by participant basis that can then be grouped into an HTML presentation:
    - [`render_all.R`](render_all.R): Script that creates individual analyses of all participants' rating data stored in the shared Box folder. Outputs from this script are written to the [`indiv_rpts/`](indiv_rpts/) directory.
    - [`make_ioslides.R`](make_ioslides.R): Script creates an HTML formatted set of slides of all outputs in `indiv_rpts/`. The output file is [`peep-II-individual-data-summaries.html`](peep-II-individual-data-summaries.html). To view the presentation in your browser, use [this](https://rawgit.com/gilmore-lab/peep-II-ratings-analysis/master/peep-II-individual-data-summaries.html) link to a version that uses [RawGit](http://rawgit.com). [`peep-II-individual-data-summaries.R`](peep-II-individual-data-summaries.R) is generated as a by-product.