---
title: "Introduction to DataFrame"
date: "2015-11-24"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Introduction to DataFrame}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

[![Travis-CI Build Status](https://travis-ci.org/wahani/dat.svg?branch=master)](https://travis-ci.org/wahani/dat)
[![codecov.io](https://codecov.io/github/wahani/dat/coverage.svg?branch=master)](https://codecov.io/github/wahani/dat?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/dat)](http://cran.r-project.org/package=dat)
[![Downloads](http://cranlogs.r-pkg.org/badges/dat?color=brightgreen)](http://www.r-pkg.org/pkg/dat)

Provides a class and methods for working with data frames extending what can be found in the dplyr.

## Installation


```r
devtools::install_github("wahani/dat")
```

## Why should you care?

This package links to dplyr functions and does not cover everything in it, so
why should you care:

- You probably have to rewrite all your dplyr / data.table code once you put it 
inside a package. I.e. working around non standard evaluation or find another
way to apiece R CMD check. And you don't like that.
- We work together and you have to understand code I wrote.

I cannot think of other reasons why you should care, of course if you just like
the syntax that's fine too.


## Examples:

I took the examples from the introductory vignette of dplyr. Things that are not supported:

- no rename
- no distinct
- no transmute

But you still work with data frames. So you can go back or mix in dplyr features
when you need them.


```r
library(nycflights13)
library(dplyr)
library(dat)

dat <- do.call(DataFrame, flights)
str(dat)
```

```
## Classes 'DataFrame', 'tbl_df', 'tbl' and 'data.frame':	336776 obs. of  16 variables:
##  $ year     : int  2013 2013 2013 2013 2013 2013 2013 2013 2013 2013 ...
##  $ month    : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ day      : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ dep_time : int  517 533 542 544 554 554 555 557 557 558 ...
##  $ dep_delay: num  2 4 2 -1 -6 -4 -5 -3 -3 -2 ...
##  $ arr_time : int  830 850 923 1004 812 740 913 709 838 753 ...
##  $ arr_delay: num  11 20 33 -18 -25 12 19 -14 -8 8 ...
##  $ carrier  : chr  "UA" "UA" "AA" "B6" ...
##  $ tailnum  : chr  "N14228" "N24211" "N619AA" "N804JB" ...
##  $ flight   : int  1545 1714 1141 725 461 1696 507 5708 79 301 ...
##  $ origin   : chr  "EWR" "LGA" "JFK" "JFK" ...
##  $ dest     : chr  "IAH" "IAH" "MIA" "BQN" ...
##  $ air_time : num  227 227 160 183 116 150 158 53 140 138 ...
##  $ distance : num  1400 1416 1089 1576 762 ...
##  $ hour     : num  5 5 5 5 5 5 5 5 5 5 ...
##  $ minute   : num  17 33 42 44 54 54 55 57 57 58 ...
```

This function I use to compare if my code produces roughly (not the same class)
the same as dplyr code.


```r
myIdentical <- function(a, b) {
  l <- lapply(list(a, b), as.data.frame)
  do.call(identical, l)
}
```

### Filter rows


```r
myIdentical(
  filter(flights, month == 1, day == 1),
  dat[~ month == 1 & day == 1]
)
```

```
## [1] TRUE
```

```r
myIdentical(
  slice(flights, 1:10),
  dat[~1:10]
)
```

```
## [1] TRUE
```

```r
myIdentical(
  # It is truly amazing how many times I tried to subset a data frame with
  # dat[1:10] and meant rows. Thats why this is working:
  dat[~1:10],
  dat[1:10, ]
)
```

```
## [1] TRUE
```

### Sorting


```r
myIdentical(
  arrange(flights, year, month, day),
  dat[~order(year, month, day)]
)
```

```
## [1] TRUE
```


### Select cols

You can use characters and logicals to select cols of a *DataFrame*. Using numeric values is not supported; it is error prone and I have spent too many hours of my live debugging code where I relied on positions in a data frame.


```r
myIdentical(
  select(flights, year, month, day),
  dat[c("year", "month", "day")]
)
```

```
## [1] TRUE
```

```r
myIdentical(
  select(flights, year:day),
  dat["year:day"] # characters are passed into dplyr::select_
)
```

```
## [1] TRUE
```

You can also pass in a function wich checks if you want to select a column, e.g.
select all numeric cols:


```r
dat[is.numeric]
```

```
## Source: local data frame [336,776 x 12]
## 
##     year month   day dep_time dep_delay arr_time arr_delay flight air_time
##    (int) (int) (int)    (int)     (dbl)    (int)     (dbl)  (int)    (dbl)
## 1   2013     1     1      517         2      830        11   1545      227
## 2   2013     1     1      533         4      850        20   1714      227
## 3   2013     1     1      542         2      923        33   1141      160
## 4   2013     1     1      544        -1     1004       -18    725      183
## 5   2013     1     1      554        -6      812       -25    461      116
## 6   2013     1     1      554        -4      740        12   1696      150
## 7   2013     1     1      555        -5      913        19    507      158
## 8   2013     1     1      557        -3      709       -14   5708       53
## 9   2013     1     1      557        -3      838        -8     79      140
## 10  2013     1     1      558        -2      753         8    301      138
## ..   ...   ...   ...      ...       ...      ...       ...    ...      ...
## Variables not shown: distance (dbl), hour (dbl), minute (dbl)
```

Or select all cols with missing values in them:


```r
dat[function(x) any(is.na(x))]
```

```
## Source: local data frame [336,776 x 7]
## 
##    dep_time dep_delay arr_time arr_delay air_time  hour minute
##       (int)     (dbl)    (int)     (dbl)    (dbl) (dbl)  (dbl)
## 1       517         2      830        11      227     5     17
## 2       533         4      850        20      227     5     33
## 3       542         2      923        33      160     5     42
## 4       544        -1     1004       -18      183     5     44
## 5       554        -6      812       -25      116     5     54
## 6       554        -4      740        12      150     5     54
## 7       555        -5      913        19      158     5     55
## 8       557        -3      709       -14       53     5     57
## 9       557        -3      838        -8      140     5     57
## 10      558        -2      753         8      138     5     58
## ..      ...       ...      ...       ...      ...   ...    ...
```

### Mutate


```r
myIdentical(
  mutate(flights,
         gain = arr_delay - dep_delay,
         speed = distance / air_time * 60),
  dat[gain ~ arr_delay - dep_delay,
      speed ~ distance / air_time * 60]
)
```

```
## [1] TRUE
```

### Summarise


```r
myIdentical(
  group_by(flights, month) %>% 
    summarise(delay = mean(dep_delay, na.rm = TRUE)),
  dat[delay ~ mean(dep_delay, na.rm = TRUE), by = "month"]
)
```

```
## [1] TRUE
```

```r
# dplyr:
delay1 <- flights %>%
  group_by(tailnum) %>%
  summarise(
    count = n(),
    dist = mean(distance, na.rm = TRUE),
    delay = mean(arr_delay, na.rm = TRUE)
  ) %>%
  filter(count > 20, dist < 2000)

# dat:
delay2 <- dat[
  count ~ n(),
  dist ~ mean(distance, na.rm = TRUE),
  delay ~ mean(arr_delay, na.rm = TRUE),
  by = "tailnum"
  ] %>%
  .[~count > 20 & dist < 2000]

myIdentical(delay1, delay2)
```

```
## [1] TRUE
```

