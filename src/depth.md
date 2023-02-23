Check depth in OBIS data
================
Yi-Ming Gan
2023-02-23

## Check records with depth \<= 0 meters

``` r
library(tidyverse)
library(lubridate)
library(httr)
library(robis)

url <- "https://api.obis.org/"
r = GET(url, path = "v3/occurrence", query = list(enddepth = 0))
content(r)$total
```

    ## [1] 8772541

There are many records with negative depth. How many are they?

``` r
# variables
end_depth = 0  # in meters
max_end_depth = -11000

# initialize tibble with end_depth at 100m interval
occ_df <- tibble(end_depth = seq(0, max_end_depth, -100))

# function to get occurrence record count per end_depth
get_occ_count_with_end_depth <- function(end_depth){
  url <- "https://api.obis.org/"
  r <- GET(url, path = "v3/occurrence", query = list(enddepth = end_depth))
  count <- content(r)$total  # get the count of records from query content
  return(count)
}

# create columns of record count for each of end_depth
occ_df <- occ_df %>%
  rowwise() %>%
  mutate(
    count = get_occ_count_with_end_depth(end_depth)
    )

occ_df
```

    ## # A tibble: 111 × 2
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
    ## # … with 101 more rows

``` r
# plot them into bar chart
ggplot(occ_df, aes(x = end_depth, y = count)) + 
  geom_bar(stat = "identity") +
  scale_y_continuous(trans="log10")
```

![](depth_files/figure-gfm/depth%20occ%20count-1.png)<!-- -->

## What are taxa of these records where depth \< 0?

Since the web service takes integer for `enddepth` and 0 is a valid
depth value. Attempt query for facets with taxon `class` with enddepth =
-1.

``` r
# GET request with parameters
url <- "https://api.obis.org/"
r <- GET(url, path = "v3/facet", query = list(facets = "class", enddepth = -1))
results <- content(r)$results$class

# create a tibble of each taxonomic class with the count
class_negative_depth <- tibble(
  class = map_chr(results, "key"),
  count = map_int(results, "records")
) %>% arrange(desc(count))

class_negative_depth
```

    ## # A tibble: 10 × 2
    ##    class          count
    ##    <chr>          <int>
    ##  1 Actinopteri    89360
    ##  2 Aves           44425
    ##  3 Gastropoda     25489
    ##  4 Asteroidea     14040
    ##  5 Bivalvia        7312
    ##  6 Elasmobranchii  5449
    ##  7 Malacostraca    2174
    ##  8 Polyplacophora  2120
    ##  9 Dinophyceae     1266
    ## 10 Polychaeta       884

``` r
ggplot(class_negative_depth, aes(x = count, y = class)) + 
  geom_col(stat = "identity")
```

![](depth_files/figure-gfm/taxon%20class%20negative%20depth-1.png)<!-- -->

Aside from birds (Aves) where the records should not use
`minimumDepthInMeters` and `maximumDepthInMeters` to document the
distance above surface, the rest of the records with negative depth
seems to be misunderstanding of the terms definition.
