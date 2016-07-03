---
title: "Tools for Data Manipulation"
date: "2016-07-03"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Tools for Data Manipulation}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

[![Travis-CI Build Status](https://travis-ci.org/wahani/dat.svg?branch=master)](https://travis-ci.org/wahani/dat)
[![codecov.io](https://codecov.io/github/wahani/dat/coverage.svg?branch=master)](https://codecov.io/github/wahani/dat?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/dat)](http://cran.r-project.org/package=dat)
[![Downloads](http://cranlogs.r-pkg.org/badges/dat?color=brightgreen)](http://www.r-pkg.org/pkg/dat)

An implementation of common higher order functions and a link to dplyr for common transformations on data frames to work around non standard evaluation by default.

## Installation


```r
devtools::install_github("wahani/dat")
```

## Why should you care?

- You probably have to rewrite all your dplyr / data.table code once you put it 
inside a package. I.e. working around non standard evaluation or find another
way to satisfy `R CMD check`. And you don't like that.
- `dplyr` is not respecting the class of the object it operates on; the class
attribute changes on-the-fly.
- Neither `dplyr` nor `data.table` is playing nice with S4, but you really,
really want a S4 *data.table* or *tbl_df*.
- You like currying as in `rlist` and `purrr`.


## A link to `dplyr`

The examples are from the introductory vignette of `dplyr`. Things that are not
supported:

- rename
- distinct
- transmute

But you still work with data frames: so you can simply mix in dplyr features 
whenever you need them. The functions `filtar`, `mutar` and `sumar` are `R CMD 
check` friendly replacements for the corresponding versions in `dplyr`. For 
`select` you can use `extract`. The function names are chosen so that they are 
similar but do not conflict with `dplyr`s function names - so `dplyr` can be
savely attached to the search path.


```r
library(nycflights13)
library(dat)
```

### Select rows

`filtar` can be used as a replacement for `filter` and `slice`. When you
reference a variable in the data itself, you can indicate this by using a one
sided formula.


```r
filtar(flights, ~ month == 1 & day == 1)  
filtar(flights, 1:10)
```

And for sorting:


```r
filtar(flights, ~order(year, month, day))
```

```
## Source: local data frame [336,776 x 19]
## 
##     year month   day dep_time sched_dep_time dep_delay arr_time
##    <int> <int> <int>    <int>          <int>     <dbl>    <int>
## 1   2013     1     1      517            515         2      830
## 2   2013     1     1      533            529         4      850
## 3   2013     1     1      542            540         2      923
## 4   2013     1     1      544            545        -1     1004
## 5   2013     1     1      554            600        -6      812
## 6   2013     1     1      554            558        -4      740
## 7   2013     1     1      555            600        -5      913
## 8   2013     1     1      557            600        -3      709
## 9   2013     1     1      557            600        -3      838
## 10  2013     1     1      558            600        -2      753
## ..   ...   ...   ...      ...            ...       ...      ...
## Variables not shown: sched_arr_time <int>, arr_delay <dbl>, carrier <chr>,
##   flight <int>, tailnum <chr>, origin <chr>, dest <chr>, air_time <dbl>,
##   distance <dbl>, hour <dbl>, minute <dbl>, time_hour <time>.
```


### Select cols

You can use characters, logicals, regular expressions and functions to select
columns. Regular expressions are indicated by a leading "^". Character are
simply passed to `dplyr::select_`.


```r
flights %>%
  extract(c("year", "month", "day")) %>%
  extract("year:day") %>%
  extract("^day$") %>%
  extract(is.numeric)
```


### Mutate


```r
mutar(
  flights,
  gain ~ arr_delay - dep_delay,
  speed ~ distance / air_time * 60
)
```

Grouping data is handled within `mutar`:


```r
mutar(flights, n ~ n(), by = "month") %>%
  extract("n") %>% 
  unique
```

```
## Source: local data frame [12 x 1]
## 
##        n
##    <int>
## 1  27004
## 2  28889
## 3  27268
## 4  28135
## 5  24951
## 6  28834
## 7  28330
## 8  28796
## 9  28243
## 10 29425
## 11 29327
## 12 27574
```


### Summarise


```r
sumar(flights, delay ~ mean(dep_delay, na.rm = TRUE), by = "month")
```


### Same operations on different columns


```r
sumar(
  flights, 
  FL(.n ~ mean(.n, na.rm = TRUE), 
     .n = c("arr_delay", "dep_delay")), 
  by = "month"
)
```

```
## Source: local data frame [12 x 3]
## 
##    month  arr_delay dep_delay
##    <int>      <dbl>     <dbl>
## 1      1  6.1299720 10.036665
## 2      2  5.6130194 10.816843
## 3      3  5.8075765 13.227076
## 4      4 11.1760630 13.938038
## 5      5  3.5215088 12.986859
## 6      6 16.4813296 20.846332
## 7      7 16.7113067 21.727787
## 8      8  6.0406524 12.611040
## 9      9 -4.0183636  6.722476
## 10    10 -0.1670627  6.243988
## 11    11  0.4613474  5.435362
## 12    12 14.8703553 16.576688
```


## A link to S4

Using this package you can create S4 classes to contain a data frame (or a 
data.table) and use the interface to `dplyr`. Both `dplyr` and `data.table` do 
not support integration with S4. The main function here is `mutar` which is
generic enough to link to subsetting of rows and cols as well as mutate and 
summarise. In the background `dplyr`s ability to work on a `data.table` is being
used.


```r
library(data.table)

setClass("DataTable", "data.table")

DataTable <- function(...) {
  new("DataTable", data.table::data.table(...))
}

setMethod("[", "DataTable", mutar)
```

```
## [1] "["
```

```r
dtflights <- do.call(DataTable, flights)

dtflights[1:10, "year:day"]
```

```
## Object of class "DataTable"
##     year month day
##  1: 2013     1   1
##  2: 2013     1   1
##  3: 2013     1   1
##  4: 2013     1   1
##  5: 2013     1   1
##  6: 2013     1   1
##  7: 2013     1   1
##  8: 2013     1   1
##  9: 2013     1   1
## 10: 2013     1   1
```

```r
dtflights[n ~ n(), by = "month"] %>% extract("n") %>% unique
```

```
## Object of class "DataTable"
##         n
##  1: 27004
##  2: 28889
##  3: 27268
##  4: 28135
##  5: 24951
##  6: 28834
##  7: 28330
##  8: 28796
##  9: 28243
## 10: 29425
## 11: 29327
## 12: 27574
```

```r
dtflights[n ~ n(), sby = "month"]
```

```
## Object of class "DataTable"
##     month     n
##  1:     1 27004
##  2:    10 28889
##  3:    11 27268
##  4:    12 28135
##  5:     2 24951
##  6:     3 28834
##  7:     4 28330
##  8:     5 28796
##  9:     6 28243
## 10:     7 29425
## 11:     8 29327
## 12:     9 27574
```

```r
dtflights %>%
  filtar(~month > 6) %>%
  mutar(n ~ n(), by = "month") %>%
  sumar(n ~ dplyr::first(n), by = "month")
```

```
## Object of class "DataTable"
##    month     n
## 1:    10 28889
## 2:    11 27268
## 3:    12 28135
## 4:     7 29425
## 5:     8 29327
## 6:     9 27574
```


## Working with vectors

Inspired by `rlist` and `purrr` some low level operations on vectors are
supported. The aim here is to integrate syntactic sugar for anonymous functions.
Furthermore the functions should support the use of pipes.

- `map` and `flatmap` as replacements for the apply functions
- `extract` for subsetting
- `replace` for replacing elements in a vector

What we can do with map:


```r
map(1:3, ~ .^2)
flatmap(1:3, ~ .^2)
map(1:3 ~ 11:13, c) # zip
dat <- data.frame(x = 1, y = "")
map(dat, x ~ x + 1, is.numeric)
```

What we can do with extract:


```r
extract(1:10, ~ . %% 2 == 0) %>% sum
extract(1:15, ~ 15 %% . == 0)
l <- list(aList = list(x = 1), aAtomic = "hi")
extract(l, "^aL")
extract(l, is.atomic)
```

What we can do with replace:


```r
replace(c(1, 2, NA), is.na, 0)
replace(c(1, 2, NA), rep(TRUE, 3), 0)
replace(c(1, 2, NA), 3, 0)
replace(list(x = 1, y = 2), "x", 0)
replace(list(x = 1, y = 2), "^x$", 0)
replace(list(x = 1, y = "a"), is.character, NULL)
```
