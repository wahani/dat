[![Travis-CI Build Status](https://travis-ci.org/wahani/dat.svg?branch=master)](https://travis-ci.org/wahani/dat)
[![codecov.io](https://codecov.io/github/wahani/dat/coverage.svg?branch=master)](https://codecov.io/github/wahani/dat?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/dat)](http://cran.r-project.org/package=dat)
[![Downloads](http://cranlogs.r-pkg.org/badges/dat?color=brightgreen)](http://www.r-pkg.org/pkg/dat)

An implementation of common higher order functions with syntactic
sugar for anonymous function. Provides also a link to 'dplyr' and
'data.table' for common transformations on data frames to work around non
standard evaluation by default.

## Installation

### From GitHub


```r
remotes::install_github("wahani/dat")
```

### From CRAN


```r
install.packages("dat")
```


## Why should you care?

- You probably have to rewrite all your dplyr / data.table code once you put it 
inside a package. I.e. working around non standard evaluation or find another
way to satisfy `R CMD check`. And you don't like that.
- `dplyr` is not respecting the class of the object it operates on; the class
attribute changes on-the-fly.
- Neither `dplyr` nor `data.table` are playing nice with S4, but you really,
really want a S4 *data.table* or *tbl_df*.
- You like currying as in `rlist` and `purrr`.


## Tools for data manipulation

The examples are from the introductory vignette of `dplyr`. You still work with
data frames: so you can simply mix in dplyr features whenever you need them.


```r
library("nycflights13")
library("dat")
```

### Select rows

We can use `mutar` to select rows. When you
reference a variable in the data frame, you can indicate this by using a one
sided formula.


```r
mutar(flights, ~ month == 1 & day == 1)
mutar(flights, ~ 1:10)
```

And for sorting:


```r
mutar(flights, ~ order(year, month, day))
```

```
## # A tibble: 336,776 x 19
##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
##  1  2013     1     1      517            515         2      830            819
##  2  2013     1     1      533            529         4      850            830
##  3  2013     1     1      542            540         2      923            850
##  4  2013     1     1      544            545        -1     1004           1022
##  5  2013     1     1      554            600        -6      812            837
##  6  2013     1     1      554            558        -4      740            728
##  7  2013     1     1      555            600        -5      913            854
##  8  2013     1     1      557            600        -3      709            723
##  9  2013     1     1      557            600        -3      838            846
## 10  2013     1     1      558            600        -2      753            745
## # … with 336,766 more rows, and 11 more variables: arr_delay <dbl>,
## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>
```


### Select cols

You can use characters, logicals, regular expressions and functions to select
columns. Regular expressions are indicated by a leading "^".


```r
flights %>%
  extract(c("year", "month", "day")) %>%
  extract("^day$") %>%
  extract(is.numeric)
```


### Operations on columns

The main difference between `dplyr::mutate` and `mutar` is that you use a `~`
instead of `=`.
    

```r
mutar(
  flights,
  gain ~ arr_delay - dep_delay,
  speed ~ distance / air_time * 60
)
```

Grouping data is handled within `mutar`:


```r
mutar(flights, n ~ .N, by = "month")
```


```r
mutar(flights, delay ~ mean(dep_delay, na.rm = TRUE), by = "month")
```

You can also provide additional arguments to a formula. This is especially
helpful when you want to pass arguments from a function to such expressions. The
additional augmentation can be anything which you can use to select columns
(character, regular expression, function) or a named list where each element is
a character.
    

```r
mutar(
  flights,
  .n ~ mean(.n, na.rm = TRUE) | "^.*delay$",
  .x ~ mean(.x, na.rm = TRUE) | list(.x = "arr_time"),
  by = "month"
)
```

```
## # A tibble: 336,776 x 19
##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
##    <int> <int> <int>    <int>          <int>     <dbl>    <dbl>          <int>
##  1  2013     1     1      517            515      10.0    1523.            819
##  2  2013     1     1      533            529      10.0    1523.            830
##  3  2013     1     1      542            540      10.0    1523.            850
##  4  2013     1     1      544            545      10.0    1523.           1022
##  5  2013     1     1      554            600      10.0    1523.            837
##  6  2013     1     1      554            558      10.0    1523.            728
##  7  2013     1     1      555            600      10.0    1523.            854
##  8  2013     1     1      557            600      10.0    1523.            723
##  9  2013     1     1      557            600      10.0    1523.            846
## 10  2013     1     1      558            600      10.0    1523.            745
## # … with 336,766 more rows, and 11 more variables: arr_delay <dbl>,
## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>
```


## A link to S4

Using this package you can create S4 classes to contain a data frame (or a
data.table) and use the interface to `dplyr`. Both `dplyr` and `data.table` do
not support integration with S4. The main function here is `mutar` which is
generic enough to link to subsetting of rows and cols as well as mutate and
summarise. In the background `dplyr`s ability to work on a `data.table` is being
used.


```r
library("data.table")

setClass("DataTable", "data.table")

DataTable <- function(...) {
  new("DataTable", data.table::data.table(...))
}

setMethod("[", "DataTable", mutar)

dtflights <- do.call(DataTable, nycflights13::flights)

dtflights[1:10, c("year", "month", "day")]
dtflights[n ~ .N, by = "month"]
dtflights[n ~ .N, sby = "month"]

dtflights %>%
  filtar(~month > 6) %>%
  mutar(n ~ .N, by = "month") %>%
  sumar(n ~ data.table::first(n), by = "month")
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
