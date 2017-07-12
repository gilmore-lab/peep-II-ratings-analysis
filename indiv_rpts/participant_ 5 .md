by-participant
================
Rick Gilmore
2017-07-12 07:40:10

-   [Preliminaries](#preliminaries)
-   [Time series: Family 5](#time-series-family-5)
    -   [Rated intensity](#rated-intensity)
    -   [How feel?](#how-feel)
-   [Ratings by target prosody type](#ratings-by-target-prosody-type)
    -   [Angry prosody](#angry-prosody)
    -   [Happy prosody](#happy-prosody)
    -   [Sad prosody](#sad-prosody)
    -   [Neutral prosody](#neutral-prosody)
    -   [How feel by target prosody & speaker](#how-feel-by-target-prosody-speaker)

Preliminaries
=============

``` r
peep2.data.dir <- "~/Box\ Sync/b-peep-project\ Shared/PEEP2\ data/PEEP2\ Home\ visit\ behavioural\ data/"

# List files and open one for inspection
peep2.flist <- list.files(path = peep2.data.dir, 
                          pattern = "\\.csv$", 
                          full.names = TRUE)
peep2.df.list <- lapply(peep2.flist, read.csv)
peep2.df <- Reduce(function(x,y) merge(x,y, all =TRUE),
                   peep2.df.list)

# Pick character ranges by hand
peep2.df$target_prosody <- str_sub(peep2.df$snd_file, 18, 20)
peep2.df$script_name <- str_sub(peep2.df$snd_file, 22, 24)
peep2.df$script_variation <- str_sub(peep2.df$snd_file, 
                                     26, 26)

# Create speaker_type
peep2.df$speaker_id <- as.numeric(str_sub(peep2.df$snd_file,
                                          14, 16))
peep2.df$speaker_type <- rep("???", times=dim(peep2.df)[1])
peep2.df$speaker_type[peep2.df$fam_id == peep2.df$speaker_id] = "mom"
peep2.df$speaker_type[peep2.df$nov_id == peep2.df$speaker_id] = "unf"

# Rename scared.rating for consistency with other ratings
peep2.df %>% 
  rename(scared_rating = scared.rating) ->
  peep2.df
```

``` r
peep2.df %>%
  filter(fam_id == as.numeric(params$this_fam)) ->
  this_fam.df
```

``` r
# Combine intensity ratings
this_fam_gathered.df <- gather(this_fam.df, 
                              `angry_rating`, 
                              `happy_rating`, 
                              `sad_rating`, 
                              `scared_rating`, 
                              key = "rating_type", 
                              value = "intensity")
this_fam_gathered.df$rating_type <- factor(this_fam_gathered.df$rating_type, labels = c("ang", "hap", "sad", "sca"))
```

Time series: Family 5
=====================

Rated intensity
---------------

``` r
this_fam_gathered.df$how_feel <- factor(this_fam_gathered.df$how_feel, labels = c("neu", "hap", "ang", "sad", "sca"))

this_fam_gathered.df %>%
  ggplot() +
  aes(x=sound_index, y=intensity) +
  geom_step(color = 'black') +
  geom_point(size = 1, aes(color=target_prosody)) +
  facet_grid(rating_type ~ run)
```

![](/Users/rick/github/gilmore-lab/peep-II-ratings-analysis/indiv_rpts/participant_%205%20_files/figure-markdown_github/time-series-rated-intensity-1.png)

How feel?
---------

``` r
this_fam_gathered.df %>%
  ggplot() +
  aes(x=sound_index, y=how_feel, shape=speaker_type) +
  geom_point(size = 1, aes(color=target_prosody)) +
  facet_grid(. ~ run)
```

![](/Users/rick/github/gilmore-lab/peep-II-ratings-analysis/indiv_rpts/participant_%205%20_files/figure-markdown_github/time-series-how-feel-1.png)

Ratings by target prosody type
==============================

Angry prosody
-------------

``` r
plot_ratings <- function(df = this_fam_gathered.df, prosody = "ang") {
  df %>%
  filter(target_prosody == prosody) %>%
  ggplot() +
  aes(x=rating_type, y=intensity, color=rating_type) +
  geom_violin() +
  facet_grid(. ~ speaker_type) +
  geom_jitter(width=0.3, height=0) ->
  p
  p + theme(legend.position = "none")
}

p <- plot_ratings(prosody = "ang")
p 
```

![](/Users/rick/github/gilmore-lab/peep-II-ratings-analysis/indiv_rpts/participant_%205%20_files/figure-markdown_github/angry-ratings-1.png)

Happy prosody
-------------

``` r
p <- plot_ratings(prosody = "hap")
p
```

![](/Users/rick/github/gilmore-lab/peep-II-ratings-analysis/indiv_rpts/participant_%205%20_files/figure-markdown_github/happy-ratings-1.png)

Sad prosody
-----------

``` r
p <- plot_ratings(prosody = "sad")
p
```

![](/Users/rick/github/gilmore-lab/peep-II-ratings-analysis/indiv_rpts/participant_%205%20_files/figure-markdown_github/sad-ratings-1.png)

Neutral prosody
---------------

``` r
p <- plot_ratings(prosody = "neu")
p
```

![](/Users/rick/github/gilmore-lab/peep-II-ratings-analysis/indiv_rpts/participant_%205%20_files/figure-markdown_github/neutral-ratings-1.png)

How feel by target prosody & speaker
------------------------------------

``` r
this_fam_gathered.df %>%
  ggplot() +
  aes(x=rating_type, y=intensity) +
  geom_point(size = 1, aes(color=target_prosody)) +
  facet_grid(speaker_type ~ how_feel) +
  theme(legend.position = "bottom")
```

![](/Users/rick/github/gilmore-lab/peep-II-ratings-analysis/indiv_rpts/participant_%205%20_files/figure-markdown_github/how-feel-by-target-prosody-1.png)
