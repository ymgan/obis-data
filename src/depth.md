Check depth in OBIS data
================
Yi-Ming Gan
2023-02-23

## Check records with depth \< 0 meters

### end depth \< 0

Number of records with end depth = -1

``` r
library(tidyverse)
library(lubridate)
library(httr)
library(robis)

url <- "https://api.obis.org/"
r = GET(url, path = "v3/occurrence", query = list(enddepth = -1))
content(r)$total
```

    ## [1] 197060

``` r
# variables
end_depth = 0  # in meters
max_end_depth = -100000  # minimum depth in current obis-qc checks

# initialize tibble with end_depth at 100m interval
end_depth_occ_df <- tibble(end_depth = seq(0, max_end_depth, -100))

# function to get occurrence record count per end_depth
get_occ_count_with_end_depth <- function(end_depth){
  url <- "https://api.obis.org/"
  r <- GET(url, path = "v3/occurrence", query = list(enddepth = end_depth))
  count <- content(r)$total  # get the count of records from query content
  return(count)
}

# create columns of record count for each of end_depth
end_depth_occ_df <- end_depth_occ_df %>%
  rowwise() %>%
  mutate(count = get_occ_count_with_end_depth(end_depth))

end_depth_occ_df 
```

    ## # A tibble: 1,001 × 2
    ## # Rowwise: 
    ##    end_depth   count
    ##        <dbl>   <int>
    ##  1         0 8772541
    ##  2      -100   16900
    ##  3      -200   11297
    ##  4      -300    8216
    ##  5      -400    6527
    ##  6      -500    5129
    ##  7      -600    3734
    ##  8      -700    2803
    ##  9      -800    2321
    ## 10      -900    1978
    ## # … with 991 more rows

``` r
# plot them into bar chart
ggplot(end_depth_occ_df, aes(x = end_depth, y = count)) + 
  geom_bar(stat = "identity") +
  scale_y_continuous(trans = "log10")
```

![](depth_files/figure-gfm/end%20depth%20occ%20count-1.png)<!-- -->

### start depth \< 0

skipping start depth analysis because it took forever

## Facets

### minimumDepthInMeters

Where are the negative depth with enddepth \< 0 then?

``` r
url <- "https://api.obis.org/"
r <- GET(url, path = "v3/facet", query = list(facets = "minimumDepthInMeters"))
res <- content(r)$results$minimumDepthInMeters

min_depth <- tibble(
  minimumDepthInMeters = map_int(res, "key"),
  count = map_int(res, "records")
)

ggplot(min_depth, aes(x = minimumDepthInMeters, y = count)) +
  geom_bar(stat = "identity")
```

![](depth_files/figure-gfm/min%20depth-1.png)<!-- -->

### maximumDepthInMeters

``` r
url <- "https://api.obis.org/"
r <- GET(url, path = "v3/facet", query = list(facets = "maximumDepthInMeters"))
res <- content(r)$results$maximumDepthInMeters

max_depth <- tibble(
  maximumDepthInMeters = map_int(res, "key"),
  count = map_int(res, "records")
)

ggplot(max_depth, aes(x = maximumDepthInMeters, y = count)) +
  geom_bar(stat = "identity")
```

![](depth_files/figure-gfm/max%20depth-1.png)<!-- -->

### verbatimDepth

``` r
url <- "https://api.obis.org/"
r <- GET(url, path = "v3/facet", query = list(facets = "verbatimDepth"))
res <- content(r)$results$verbatimDepth

verbatim_depth <- tibble(
  verbatimDepth = map_chr(res, "key"),
  count = map_int(res, "records")
)

ggplot(verbatim_depth, aes(x = verbatimDepth, y = count)) +
  geom_bar(stat = "identity") +
  coord_flip()
```

![](depth_files/figure-gfm/verbatim%20depth-1.png)<!-- -->

I don’t know what are these, I don’t understand it but I tried. Please
take it with a grain of salt, bye :3
