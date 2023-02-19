Check date in OBIS data
================
Yi-Ming Gan
2023-02-19

Create a tibble of cut off date (end date) and a count of records with
that end date.

``` r
library(tidyverse)
library(lubridate)
library(httr)

url <- "https://api.obis.org/"
end_date <- ymd("0099-12-31")
earliest_valid_date = ymd("0100-01-01")
df <- as.tibble(data.frame(end_date = Date(), earliest_valid_date = Date(), count = numeric()))

while (end_date < today()) {
  r = GET(url, path = "v3/occurrence", query = list(enddate = end_date))
  count <- content(r)$total  # get the count of records from query content
  df <- df %>% add_row(end_date = end_date, earliest_valid_date = earliest_valid_date, count = count)
  end_date <- end_date + years(100)
  earliest_valid_date <- earliest_valid_date + years(100)
}

df
```

    ## # A tibble: 20 × 3
    ##    end_date   earliest_valid_date    count
    ##    <date>     <date>                 <dbl>
    ##  1 0099-12-31 0100-01-01                47
    ##  2 0199-12-31 0200-01-01                52
    ##  3 0299-12-31 0300-01-01                52
    ##  4 0399-12-31 0400-01-01                52
    ##  5 0499-12-31 0500-01-01                52
    ##  6 0599-12-31 0600-01-01                52
    ##  7 0699-12-31 0700-01-01                52
    ##  8 0799-12-31 0800-01-01                52
    ##  9 0899-12-31 0900-01-01                52
    ## 10 0999-12-31 1000-01-01                52
    ## 11 1099-12-31 1100-01-01                59
    ## 12 1199-12-31 1200-01-01                69
    ## 13 1299-12-31 1300-01-01                71
    ## 14 1399-12-31 1400-01-01                71
    ## 15 1499-12-31 1500-01-01                71
    ## 16 1599-12-31 1600-01-01                74
    ## 17 1699-12-31 1700-01-01                79
    ## 18 1799-12-31 1800-01-01             22110
    ## 19 1899-12-31 1900-01-01            184827
    ## 20 1999-12-31 2000-01-01          36262055

## Occurrences prior 1700

Seems like there are many dates with 0001! Let’s see what are the dates.

``` r
library(robis)

end_date <- ymd("1699-12-31")
max_end_date <- year(end_date + years(1))  # 1700

# get all occurrence with enddate = end_date
occ <- occurrence(enddate = end_date)

# list all eventDate prior end_date
sort(occ$eventDate)
```

    ##  [1] "0001-03-17"            "0001-04-08"            "0001-04-11"           
    ##  [4] "0001-04-11"            "0001-04-11"            "0001-04-11"           
    ##  [7] "0001-04-11"            "0001-04-11"            "0001-04-11"           
    ## [10] "0001-04-11"            "0001-04-11"            "0001-04-11"           
    ## [13] "0001-04-11"            "0001-04-11"            "0001-04-11"           
    ## [16] "0001-04-20"            "0001-05-17"            "0001-05-17"           
    ## [19] "0001-05-17"            "0001-05-17"            "0001-05-17"           
    ## [22] "0001-05-18"            "0001-05-18"            "0001-05-18"           
    ## [25] "0001-06-01"            "0001-06-01"            "0001-06-01"           
    ## [28] "0001-06-01"            "0001-06-05"            "0001-06-05"           
    ## [31] "0001-06-05"            "0001-06-06"            "0001-06-06"           
    ## [34] "0001-06-06"            "0001-06-06"            "0001-06-06"           
    ## [37] "0001-06-13"            "0001-06-14"            "0001-06-14"           
    ## [40] "0001-06-17"            "0001-06-17"            "0001-07-25"           
    ## [43] "0001-07-25"            "0001-07-30"            "0001-07-30"           
    ## [46] "0001-08-03"            "0001-08-07"            "0196-05-29"           
    ## [49] "0196-05-29"            "0196-05-29"            "0196-05-29"           
    ## [52] "0196-05-29"            "1064-08-11"            "1064-08-11"           
    ## [55] "1071-08-10"            "1071-08-10"            "1073-04-17"           
    ## [58] "1073-05-10"            "1076-04-09"            "1152-01-01"           
    ## [61] "1192/2001"             "1193-11-12"            "1199-08-05"           
    ## [64] "1199-08-05"            "1199-08-05"            "1199-08-05"           
    ## [67] "1200-02-27"            "1291-07-22"            "1520"                 
    ## [70] "1526-01-01"            "1648-08-20T12:00:00Z"  "1648-08-20T12:00:00Z" 
    ## [73] "1691-07-02T12:00:00Z"  "1691-07-02T12:00:00Z"  "1698-02-03"           
    ## [76] "2003-08-21/0205-01-06" "2006-09-22/0200-10-02" "2019-09-25/0219-11-07"
    ## [79] "2019-09-25/0219-11-07"

``` r
# plot histogram of the year
ggplot(occ, aes(date_year)) + 
  geom_histogram(bins=100) +
  scale_x_continuous(breaks = seq(0, max_end_date, 100))
```

![](obis-data_files/figure-gfm/unique%20years-1.png)<!-- -->

Out of curiousity, check what datasets that have those `date_year`

``` r
occ %>% 
  count(date_year, dataset_id) %>%
  rowwise() %>%
  mutate(datasetName = dataset(datasetid = dataset_id)$title)
```

    ## # A tibble: 20 × 4
    ## # Rowwise: 
    ##    date_year dataset_id                               n datasetName             
    ##        <int> <chr>                                <int> <chr>                   
    ##  1         1 c5687a17-e454-40f9-9a4b-d04b2c812d74    47 UF Invertebrate Zoology 
    ##  2       196 aa16d305-d413-4c4a-90be-b1ec3298d58d     5 CAS Invertebrate Zoolog…
    ##  3      1064 36fbc01b-72bd-42f0-af0a-e1701cddcf94     2 FishNet2 Marine Data    
    ##  4      1071 36fbc01b-72bd-42f0-af0a-e1701cddcf94     2 FishNet2 Marine Data    
    ##  5      1073 36fbc01b-72bd-42f0-af0a-e1701cddcf94     2 FishNet2 Marine Data    
    ##  6      1076 36fbc01b-72bd-42f0-af0a-e1701cddcf94     1 FishNet2 Marine Data    
    ##  7      1103 5e6ee2aa-8155-452b-97e3-14d2835c85a0     1 NuSEDS - New Salmon Esc…
    ##  8      1104 5e6ee2aa-8155-452b-97e3-14d2835c85a0     1 NuSEDS - New Salmon Esc…
    ##  9      1119 5e6ee2aa-8155-452b-97e3-14d2835c85a0     2 NuSEDS - New Salmon Esc…
    ## 10      1152 c5687a17-e454-40f9-9a4b-d04b2c812d74     1 UF Invertebrate Zoology 
    ## 11      1193 36fbc01b-72bd-42f0-af0a-e1701cddcf94     1 FishNet2 Marine Data    
    ## 12      1199 36fbc01b-72bd-42f0-af0a-e1701cddcf94     4 FishNet2 Marine Data    
    ## 13      1200 36fbc01b-72bd-42f0-af0a-e1701cddcf94     1 FishNet2 Marine Data    
    ## 14      1291 36fbc01b-72bd-42f0-af0a-e1701cddcf94     1 FishNet2 Marine Data    
    ## 15      1520 331444c9-952b-4365-8afa-c4bf38f6a2ef     1 Marine mammal records o…
    ## 16      1526 125415a6-c5b8-4c81-9050-324deb4a0e5a     1 Historical strandings o…
    ## 17      1596 6c6df99a-7670-40b4-8eaa-f2e374ed4da6     1 hab_region_11           
    ## 18      1648 e3ab9e10-7857-494a-867c-ff8d9e5b1111     2 SPF Marine Algae Collec…
    ## 19      1691 f4775120-afcc-4350-9e03-649c978f6070     2 SOMBASE/TOTAL - Biocons…
    ## 20      1698 d104db68-63cf-4f34-b92a-54bd55a07394     1 Museums Victoria Marine…
