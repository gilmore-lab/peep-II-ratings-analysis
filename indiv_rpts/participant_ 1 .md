by-participant
================
Rick Gilmore
2017-07-12 08:05:43

-   [Preliminaries](#preliminaries)
-   [Time series: Family 1](#time-series-family-1)
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

Time series: Family 1
=====================

Rated intensity
---------------

``` r
this_fam_gathered.df %>%
  ggplot() +
  aes(x=sound_index, y=intensity) +
  geom_step(color = 'black') +
  geom_point(size = 1, aes(color=target_prosody)) +
  facet_grid(rating_type ~ run)
```

    ## Warning: Removed 66 rows containing missing values (geom_path).

    ## Warning: Removed 66 rows containing missing values (geom_point).

![](/Users/rick/github/gilmore-lab/peep-II-ratings-analysis/indiv_rpts/participant_%201%20_files/figure-markdown_github/time-series-rated-intensity-1.png)

How feel?
---------

``` r
# this_fam_gathered.df$how_feel <- factor(this_fam_gathered.df$how_feel, labels = c("neu", "hap", "ang", "sad", "sca"))

this_fam_gathered.df %>%
  ggplot() +
  aes(x=sound_index, y=how_feel, shape=speaker_type) +
  geom_point(size = 1, aes(color=target_prosody)) +
  facet_grid(. ~ run)
```

![](/Users/rick/github/gilmore-lab/peep-II-ratings-analysis/indiv_rpts/participant_%201%20_files/figure-markdown_github/time-series-how-feel-1.png)

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

    ## Warning: Removed 16 rows containing non-finite values (stat_ydensity).

    ## Warning: Removed 16 rows containing missing values (geom_point).

![](/Users/rick/github/gilmore-lab/peep-II-ratings-analysis/indiv_rpts/participant_%201%20_files/figure-markdown_github/angry-ratings-1.png)

Happy prosody
-------------

``` r
p <- plot_ratings(prosody = "hap")
p
```

    ## Warning: Removed 16 rows containing non-finite values (stat_ydensity).

    ## Warning: Removed 16 rows containing missing values (geom_point).

![](/Users/rick/github/gilmore-lab/peep-II-ratings-analysis/indiv_rpts/participant_%201%20_files/figure-markdown_github/happy-ratings-1.png)

Sad prosody
-----------

``` r
p <- plot_ratings(prosody = "sad")
p
```

    ## Warning: Removed 16 rows containing non-finite values (stat_ydensity).

    ## Warning: Removed 16 rows containing missing values (geom_point).

![](/Users/rick/github/gilmore-lab/peep-II-ratings-analysis/indiv_rpts/participant_%201%20_files/figure-markdown_github/sad-ratings-1.png)

Neutral prosody
---------------

``` r
p <- plot_ratings(prosody = "neu")
p
```

    ## Warning: Removed 18 rows containing non-finite values (stat_ydensity).

    ## Warning: Removed 18 rows containing missing values (geom_point).

![](/Users/rick/github/gilmore-lab/peep-II-ratings-analysis/indiv_rpts/participant_%201%20_files/figure-markdown_github/neutral-ratings-1.png)

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

    ## Warning: Removed 66 rows containing missing values (geom_point).

![](/Users/rick/github/gilmore-lab/peep-II-ratings-analysis/indiv_rpts/participant_%201%20_files/figure-markdown_github/how-feel-by-target-prosody-1.png)
