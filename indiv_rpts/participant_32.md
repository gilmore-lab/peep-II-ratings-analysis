by-participant
================
Rick Gilmore
2017-07-12 14:05:14

-   [Family 32](#family-32)
-   [Time series](#time-series)
    -   [Rated intensity](#rated-intensity)
    -   [How feel?](#how-feel)
-   [Ratings by target prosody type](#ratings-by-target-prosody-type)
    -   [Angry prosody](#angry-prosody)
    -   [Happy prosody](#happy-prosody)
    -   [Sad prosody](#sad-prosody)
    -   [Neutral prosody](#neutral-prosody)
    -   [How feel by target prosody & speaker](#how-feel-by-target-prosody-speaker)
-   [Cluster analysis](#cluster-analysis)
    -   [Hierarchical clusters](#hierarchical-clusters)
    -   [Multidimensional scaling](#multidimensional-scaling)
    -   [K-means](#k-means)

Family 32
=========

Time series
===========

Rated intensity
---------------

``` r
this_fam_gathered.df %>%
  ggplot() +
  aes(x=sound_index, y=intensity) +
  geom_step(color = 'black') +
  geom_point(size = 1, aes(color=target_prosody)) +
  facet_grid(rating_type ~ run) +
  theme(legend.position = "bottom")
```

![](/Users/rick/github/gilmore-lab/peep-II-ratings-analysis/indiv_rpts/participant_32_files/figure-markdown_github/time-series-rated-intensity-1.png)

How feel?
---------

``` r
this_fam_gathered.df %>%
  ggplot() +
  aes(x=sound_index, y=how_feel, shape=speaker_type) +
  geom_point(size = 1, aes(color=target_prosody)) +
  facet_grid(. ~ run) +
  theme(legend.position = "bottom")
```

![](/Users/rick/github/gilmore-lab/peep-II-ratings-analysis/indiv_rpts/participant_32_files/figure-markdown_github/time-series-how-feel-1.png)

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

![](/Users/rick/github/gilmore-lab/peep-II-ratings-analysis/indiv_rpts/participant_32_files/figure-markdown_github/angry-ratings-1.png)

Happy prosody
-------------

``` r
p <- plot_ratings(prosody = "hap")
p
```

![](/Users/rick/github/gilmore-lab/peep-II-ratings-analysis/indiv_rpts/participant_32_files/figure-markdown_github/happy-ratings-1.png)

Sad prosody
-----------

``` r
p <- plot_ratings(prosody = "sad")
p
```

![](/Users/rick/github/gilmore-lab/peep-II-ratings-analysis/indiv_rpts/participant_32_files/figure-markdown_github/sad-ratings-1.png)

Neutral prosody
---------------

``` r
p <- plot_ratings(prosody = "neu")
p
```

![](/Users/rick/github/gilmore-lab/peep-II-ratings-analysis/indiv_rpts/participant_32_files/figure-markdown_github/neutral-ratings-1.png)

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

![](/Users/rick/github/gilmore-lab/peep-II-ratings-analysis/indiv_rpts/participant_32_files/figure-markdown_github/how-feel-by-target-prosody-1.png)

Cluster analysis
================

Hierarchical clusters
---------------------

See <https://stackoverflow.com/questions/20343398/how-to-use-hclust-as-function-call-in-r>.

``` r
hclustfunc <- function(x, method = "complete", dmeth = "euclidean") {    
    hclust(dist(x, method = dmeth), method = method)
}
```

``` r
# Drop scared because it wasn't reported for all participants
this_fam.df %>%
  select(sad.r, hap.r, ang.r, how_feel) %>%
  hclustfunc() -> fit
```

    ## Warning in dist(x, method = dmeth): NAs introduced by coercion

``` r
clust_lbl <- with(this_fam.df, 
                  paste0(target_prosody, "-", speaker_type, "-", script_name))

plot(fit, labels = clust_lbl)
```

![](/Users/rick/github/gilmore-lab/peep-II-ratings-analysis/indiv_rpts/participant_32_files/figure-markdown_github/hclust-no-scared-1.png)

Multidimensional scaling
------------------------

``` r
# Drop scared because it wasn't reported for all participants, assume k=2 dimensions

d <- dist(this_fam.df[,7:10])
```

    ## Warning in dist(this_fam.df[, 7:10]): NAs introduced by coercion

``` r
mds_fit <- cmdscale(d, eig=TRUE, k=2)

# New data frame for plotting
mds.df <- data.frame(x = mds_fit$points[,1],
                     y = mds_fit$points[,2],
                     target_prosody = this_fam.df$target_prosody,
                     script_name <- this_fam.df$script_name,
    script_variation = this_fam.df$script_variation)

mds.df %>%
  ggplot() +
  aes(x = x, y = y, color = target_prosody) +
  geom_point(alpha = .5) +
  theme(legend.position = "bottom")
```

![](/Users/rick/github/gilmore-lab/peep-II-ratings-analysis/indiv_rpts/participant_32_files/figure-markdown_github/mds-no-scared-1.png)

K-means
-------

``` r
set.seed(20)
d <- dist(this_fam.df[,c('hap.r',
                    'ang.r',
                    'sad.r')])

ratings_cluster <- kmeans(d, centers = 4, nstart = 20)
table(ratings_cluster$cluster, this_fam.df$target_prosody)
```

    ##    
    ##     ang hap neu sad
    ##   1  10   5   5   0
    ##   2   2   0   1   8
    ##   3   0  10   0   0
    ##   4   4   0  11   8
