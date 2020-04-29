Initial Steps to Network Analysis
================
Fadi Egho

Fifth Tribe, a digital agency serving non-profits and government
agencies, has scraped over 17,000 tweets from 107 ISIS-supporting
twitter users from September of 2015 till January of 2016 (roughly
corresponding to ISIS’ peak notoriety period) and uploaded them onto
[kaggle]('https://www.kaggle.com/fifthtribe/how-isis-uses-twitter') as a
public domain dataset \[1\]. Let’s start by loading some necessary
packages:

``` r
library(readxl)
library(tidyverse)
library(stringi)
```

We then import the data set from excel:

``` r
twtr <- read_excel('tweets.xlsx')
head(twtr)
```

    ## # A tibble: 6 x 8
    ##   name   username  description  location followers numberstatuses time  tweets  
    ##   <chr>  <chr>     <chr>        <chr>        <dbl>          <dbl> <chr> <chr>   
    ## 1 Gunsa~ GunsandC~ ENGLISH TRA~ <NA>           640             49 1/6/~ ENGLISH~
    ## 2 Gunsa~ GunsandC~ ENGLISH TRA~ <NA>           640             49 1/6/~ ENGLISH~
    ## 3 Gunsa~ GunsandC~ ENGLISH TRA~ <NA>           640             49 1/6/~ ENGLISH~
    ## 4 Gunsa~ GunsandC~ ENGLISH TRA~ <NA>           640             49 1/6/~ ENGLISH~
    ## 5 Gunsa~ GunsandC~ ENGLISH TRA~ <NA>           640             49 1/6/~ ENGLISH~
    ## 6 Gunsa~ GunsandC~ ENGLISH TRA~ <NA>           640             49 1/6/~ THE SEC~

``` r
glimpse(twtr)
```

    ## Rows: 17,410
    ## Columns: 8
    ## $ name           <chr> "GunsandCoffee", "GunsandCoffee", "GunsandCoffee", "...
    ## $ username       <chr> "GunsandCoffee70", "GunsandCoffee70", "GunsandCoffee...
    ## $ description    <chr> "ENGLISH TRANSLATIONS: http://t.co/QLdJ0ftews", "ENG...
    ## $ location       <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, ...
    ## $ followers      <dbl> 640, 640, 640, 640, 640, 640, 640, 640, 640, 640, 64...
    ## $ numberstatuses <dbl> 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, ...
    ## $ time           <chr> "1/6/2015 21:07", "1/6/2015 21:27", "1/6/2015 21:29"...
    ## $ tweets         <chr> "ENGLISH TRANSLATION: 'A MESSAGE TO THE TRUTHFUL IN ...

As noted above, the data set captures variables such as the *username*,
*followers*, *time*, and *tweets* (where the text in each tweet also
contains ‘mentions’ of other users).

1.  Fifth Tribe. (2016, May). How ISIS Uses Twitter, Version 2.
    Retrieved February 19, 2020 from
    ‘<https://www.kaggle.com/fifthtribe/how-isis-uses-twitter>’
