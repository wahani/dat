---
title: "Tools for Data Manipulation"
date: "2015-12-18"
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

An implementation of common higher order functions (map, extract and reduce) and a link to dplyr for common transformations on data frames to work around non standard evaluation by default.

## Installation


```r
devtools::install_github("wahani/dat")
```

## Why should you care?

- You probably have to rewrite all your dplyr / data.table code once you put it 
inside a package. I.e. working around non standard evaluation or find another
way to satisfy R CMD check. And you don't like that.
- dplyr is not respecting the class of the object it operates on; the class
attribute changes on-the-fly.
- Neither dplyr nor data.table is playing nice with S4, but you really, really
want a S4 *data.table* or *tbl_df*.
- You like currying as in rlist and purrr.
- You find it annoying that you constantly have to switch between lapply, vapply
and mapply (and other map functions).
- We work together and you have to understand code I wrote.

## map

What we can do with map:


```r
library(dat)
map(1:3, ~ .^2) # lapply
```

```
## [[1]]
## [1] 1
## 
## [[2]]
## [1] 4
## 
## [[3]]
## [1] 9
```

```r
map(1:3, numeric(1) : x ~ x^2) # vapply
```

```
## [1] 1 4 9
```

```r
map(ML(1:4, 4:1), integer : f(x, y) ~ rep(x, y)) # mapply + check return type
```

```
## [[1]]
## [1] 1 1 1 1
## 
## [[2]]
## [1] 2 2 2
## 
## [[3]]
## [1] 3 3
## 
## [[4]]
## [1] 4
```

```r
map(list(1:4, 4:1), 2:3) # subsetting on lists
```

```
## [[1]]
## [1] 2 3
## 
## [[2]]
## [1] 3 2
```

```r
map(ML(1:3, 11:13), c) # zip
```

```
## [[1]]
## [1]  1 11
## 
## [[2]]
## [1]  2 12
## 
## [[3]]
## [1]  3 13
```

```r
map(ML(1:3, 11:13), c) %>% 
  { map(do.call(ML, .), c) } # unzip
```

```
## [[1]]
## [1] 1 2 3
## 
## [[2]]
## [1] 11 12 13
```

```r
dat <- data.frame(x = 1, y = "")
map(dat, x ~ x + 1, is.numeric) # only operates on numeric cols
```

```
##   x y
## 1 2
```


## extract

Sum all even numbers from 1 to 10:


```r
is.even <- function(x) (x %% 2) == 0
sum((1:10)[is.even(1:10)])
```

```
## [1] 30
```

```r
1:10 %>% extract(~ . %% 2 == 0) %>% sum
```

```
## [1] 30
```

```r
1:10 %>% extract(is.even) %>% sum
```

```
## [1] 30
```

Find all factors of 15:


```r
factors <- function(a, b = 1:a) b[a %% b == 0]
factors(15)
```

```
## [1]  1  3  5 15
```

```r
extract(1:15, ~ 15 %% . == 0)
```

```
## [1]  1  3  5 15
```

Find all relative prime numbers:


```r
gcd <- function(a, b) {
  .gcd <- function(a, b) if (b == 0) a else Recall(b, a %% b)
  map(ML(a, b), .gcd) %>% unlist
}

extract(1:10, x ~ gcd(x, 10) == 1)
```

```
## [1] 1 3 7 9
```

Find real prime numbers:


```r
isPrime <- function(n) {
  .isPrime <- function(n) {
    iter <- function(i) {
      if (i * i > n) TRUE
      else if (n %% i == 0 || n %% (i + 2) == 0) FALSE
      else Recall(i + 6)
    }
    if (n <= 1) FALSE
    else if (n <= 3) TRUE
    else if (n %% 2 == 0 || n %% 3 == 0) FALSE
    else iter(5)
  }
  map(n, logical(1) : x ~ .isPrime(x))
}

extract(1:10, isPrime)
```

```
## [1] 2 3 5 7
```

## reduce

Let's define a sum function:


```r
newSum <- function(x) reduce(x, `+`)
newSum(1:10)
```

```
## [1] 55
```

Or bind some data frames together:


```r
dat <- data.frame(id = 1:2, y = 1:4)
split(dat, dat$id) %>%
  reduce(rbind) # and reverse the split
```

```
##   id y
## 1  1 1
## 3  1 3
## 2  2 2
## 4  2 4
```


## Data Frame

I took the examples from the introductory vignette of dplyr. Things that are not
supported:

- rename
- distinct
- transmute

But you still work with data frames. So you can go back or mix in dplyr features
when you need them.


```r
library(nycflights13)
library(dplyr)

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

### Filter


```r
filter(flights, month == 1, day == 1) # dplyr
dat[~ month == 1 & day == 1]          # #1
mutar(dat, ~ month == 1 & day == 1)   # #2

slice(flights, 1:10)                  # dplyr
dat[~1:10]                            # #1
dat %>% mutar(~1:10)                  # #2
```


```r
dat[1:10, ]                           # standard things
```

```
## Source: local data frame [10 x 16]
## 
##     year month   day dep_time dep_delay arr_time arr_delay carrier tailnum
##    (int) (int) (int)    (int)     (dbl)    (int)     (dbl)   (chr)   (chr)
## 1   2013     1     1      517         2      830        11      UA  N14228
## 2   2013     1     1      533         4      850        20      UA  N24211
## 3   2013     1     1      542         2      923        33      AA  N619AA
## 4   2013     1     1      544        -1     1004       -18      B6  N804JB
## 5   2013     1     1      554        -6      812       -25      DL  N668DN
## 6   2013     1     1      554        -4      740        12      UA  N39463
## 7   2013     1     1      555        -5      913        19      B6  N516JB
## 8   2013     1     1      557        -3      709       -14      EV  N829AS
## 9   2013     1     1      557        -3      838        -8      B6  N593JB
## 10  2013     1     1      558        -2      753         8      AA  N3ALAA
## Variables not shown: flight (int), origin (chr), dest (chr), air_time
##   (dbl), distance (dbl), hour (dbl), minute (dbl)
```

### Sorting


```r
arrange(flights, year, month, day)    # dplyr
dat[~order(year, month, day)]         # #1
```


```r
dat %>% mutar(~order(year, month, day)) # #2
```

```
## Source: local data frame [336,776 x 16]
## 
##     year month   day dep_time dep_delay arr_time arr_delay carrier tailnum
##    (int) (int) (int)    (int)     (dbl)    (int)     (dbl)   (chr)   (chr)
## 1   2013     1     1      517         2      830        11      UA  N14228
## 2   2013     1     1      533         4      850        20      UA  N24211
## 3   2013     1     1      542         2      923        33      AA  N619AA
## 4   2013     1     1      544        -1     1004       -18      B6  N804JB
## 5   2013     1     1      554        -6      812       -25      DL  N668DN
## 6   2013     1     1      554        -4      740        12      UA  N39463
## 7   2013     1     1      555        -5      913        19      B6  N516JB
## 8   2013     1     1      557        -3      709       -14      EV  N829AS
## 9   2013     1     1      557        -3      838        -8      B6  N593JB
## 10  2013     1     1      558        -2      753         8      AA  N3ALAA
## ..   ...   ...   ...      ...       ...      ...       ...     ...     ...
## Variables not shown: flight (int), origin (chr), dest (chr), air_time
##   (dbl), distance (dbl), hour (dbl), minute (dbl)
```

### Select cols

You can use characters and logicals to select cols of a *DataFrame*. Using
numeric values is not supported; it is error prone and I have spent too many
hours of my life debugging code where I relied on positions in a data frame.


```r
select(flights, year, month, day)       # dplyr 
select(flights, year:day)               # dplyr

# characters are passed into dplyr::select_:
dat %>%
  mutar(c("year", "month", "day")) %>%
  mutar("year:day")

dat[c("year", "month", "day")]["year:day"]
```

You can also pass in a function which checks if you want to select a column, e.g.
select all numeric cols:


```r
dat[is.numeric] # or mutar(dat, is.numeric)
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

### Mutate


```r
flights %>% 
  mutate(gain = arr_delay - dep_delay,
         speed = distance / air_time * 60)

dat[gain ~ arr_delay - dep_delay,
    speed ~ distance / air_time * 60]
```


```r
dat %>%
  mutar(gain ~ arr_delay - dep_delay,
        speed ~ distance / air_time * 60) %>%
  mutar("^gain|speed$") # I assume a regex if the character begins with '^'
```

```
## Source: local data frame [336,776 x 2]
## 
##     gain    speed
##    (dbl)    (dbl)
## 1      9 370.0441
## 2     16 374.2731
## 3     31 408.3750
## 4    -17 516.7213
## 5    -19 394.1379
## 6     16 287.6000
## 7     24 404.4304
## 8    -11 259.2453
## 9     -5 404.5714
## 10    10 318.6957
## ..   ...      ...
```

### Summarise


```r
group_by(flights, month) %>% 
    summarise(delay = mean(dep_delay, na.rm = TRUE))

dat[delay ~ mean(dep_delay, na.rm = TRUE), 
    by = "month"]
```


```r
mutar(dat, 
      delay ~ mean(dep_delay, na.rm = TRUE), 
      by = "month")
```

```
## Source: local data frame [12 x 2]
## 
##    month     delay
##    (int)     (dbl)
## 1      1 10.036665
## 2      2 10.816843
## 3      3 13.227076
## 4      4 13.938038
## 5      5 12.986859
## 6      6 20.846332
## 7      7 21.727787
## 8      8 12.611040
## 9      9  6.722476
## 10    10  6.243988
## 11    11  5.435362
## 12    12 16.576688
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

# dat #1:
delay2 <- dat[
  count ~ n(),
  dist ~ mean(distance, na.rm = TRUE),
  delay ~ mean(arr_delay, na.rm = TRUE),
  by = "tailnum"
  ] %>%
  .[~count > 20 & dist < 2000]
```


```r
# dat #2:
dat %>%
  mutar(count ~ n(),
        dist ~ mean(distance, na.rm = TRUE),
        delay ~ mean(arr_delay, na.rm = TRUE),
        by = "tailnum") %>%
  mutar(~count > 20 & dist < 2000)
```

```
## Source: local data frame [2,962 x 4]
## 
##    tailnum count     dist      delay
##      (chr) (int)    (dbl)      (dbl)
## 1           2512 710.2576        NaN
## 2   N0EGMQ   371 676.1887  9.9829545
## 3   N10156   153 757.9477 12.7172414
## 4   N102UW    48 535.8750  2.9375000
## 5   N103US    46 535.1957 -6.9347826
## 6   N104UW    47 535.2553  1.8043478
## 7   N10575   289 519.7024 20.6914498
## 8   N105UW    45 524.8444 -0.2666667
## 9   N107US    41 528.7073 -5.7317073
## 10  N108UW    60 534.5000 -1.2500000
## ..     ...   ...      ...        ...
```

### Split-apply-combine


```r
# The naive split-apply-combine
map(dat, mutar, By("month"), count ~ n()) %>% 
  mutar(27003:27006, j = c("month", "count"))
```

```
## Source: local data frame [4 x 2]
## 
##   month count
##   (int) (int)
## 1     1 27004
## 2     1 27004
## 3     2 24951
## 4     2 24951
```


### Mixing dplyr and mutar


```r
dat %>%
  group_by(tailnum) %>%
  summarise(
    count = n(),
    dist = mean(distance, na.rm = TRUE),
    delay = mean(arr_delay, na.rm = TRUE)
  ) %>%
  mutar(~count > 20 & dist < 2000)
```

```
## Source: local data frame [2,962 x 4]
## 
##    tailnum count     dist      delay
##      (chr) (int)    (dbl)      (dbl)
## 1           2512 710.2576        NaN
## 2   N0EGMQ   371 676.1887  9.9829545
## 3   N10156   153 757.9477 12.7172414
## 4   N102UW    48 535.8750  2.9375000
## 5   N103US    46 535.1957 -6.9347826
## 6   N104UW    47 535.2553  1.8043478
## 7   N10575   289 519.7024 20.6914498
## 8   N105UW    45 524.8444 -0.2666667
## 9   N107US    41 528.7073 -5.7317073
## 10  N108UW    60 534.5000 -1.2500000
## ..     ...   ...      ...        ...
```

### Example with S4 and data.table

Let's define a S4 class inheriting from data.table and do something with it.


```r
library(data.table)
setOldClass(c("data.table", "data.frame"))

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
set.seed(1)
dat <- DataTable(id = rep(letters[1:2], each = 10), x = 101:120, y = rnorm(20))
dat[1:2, ]
```

```
## Object of class "DataTable"
##    id   x          y
## 1:  a 101 -0.6264538
## 2:  a 102  0.1836433
```

```r
dat %>% 
  mutar(~1:2, y ~ runif(2))
```

```
## Object of class "DataTable"
##    id   x         y
## 1:  a 101 0.8209463
## 2:  a 102 0.6470602
```

```r
dat %>%
  mutar(sumOfX ~ sum(x), by = "id")
```

```
## Object of class "DataTable"
##    id sumOfX
## 1:  a   1055
## 2:  b   1155
```

```r
map(dat, mutar, By("id"), sumOfX ~ sum(x))[~1:2]
```

```
## Object of class "DataTable"
##    id   x          y sumOfX
## 1:  a 101 -0.6264538   1055
## 2:  a 102  0.1836433   1055
```

```r
map(dat, dt ~ DataTable(sumOfX = sum(dt$x)), By("id"))
```

```
## Object of class "DataTable"
##    sumOfX
## 1:   1055
## 2:   1155
```

