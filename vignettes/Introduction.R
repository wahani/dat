## ---- results='asis', echo=FALSE-----------------------------------------
cat(gsub("\\n   ", "", packageDescription("dat", fields = "Description")))

## ----eval=FALSE----------------------------------------------------------
#  devtools::install_github("wahani/dat")

## ------------------------------------------------------------------------
library(dat)
map(1:3, ~ .^2) # lapply
flatmap(1:3, ~ .^2) # lapply + unlist
flatmap(1:3, numeric(1) : x ~ x^2) # lapply + unlist + type check
map(ML(1:4, 4:1), integer : f(x, y) ~ rep(x, y)) # mapply + check return type
map(list(1:4, 4:1), 2:3) # subsetting on lists
map(ML(1:3, 11:13), c) # zip
map(ML(1:3, 11:13), c) %>% 
  { map(do.call(ML, .), c) } # unzip
dat <- data.frame(x = 1, y = "")
map(dat, x ~ x + 1, is.numeric) # only operates on numeric cols

## ------------------------------------------------------------------------
is.even <- function(x) (x %% 2) == 0
sum((1:10)[is.even(1:10)])
1:10 %>% extract(~ . %% 2 == 0) %>% sum
1:10 %>% extract(is.even) %>% sum

## ------------------------------------------------------------------------
factors <- function(a, b = 1:a) b[a %% b == 0]
factors(15)
extract(1:15, ~ 15 %% . == 0)

## ------------------------------------------------------------------------
gcd <- function(a, b) {
  .gcd <- function(a, b) if (b == 0) a else Recall(b, a %% b)
  map(ML(a, b), .gcd) %>% unlist
}

extract(1:10, x ~ gcd(x, 10) == 1)

## ------------------------------------------------------------------------
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
  flatmap(n, logical(1) : x ~ .isPrime(x))
}

extract(1:10, isPrime)

## ------------------------------------------------------------------------
library(nycflights13)
library(dplyr)

dat <- do.call(DataFrame, flights)
str(dat)

## ----results='hide'------------------------------------------------------
filter(flights, month == 1, day == 1) # dplyr
dat[~ month == 1 & day == 1]          # #1
mutar(dat, ~ month == 1 & day == 1)   # #2

slice(flights, 1:10)                  # dplyr
dat[~1:10]                            # #1
dat %>% mutar(~1:10)                  # #2

## ------------------------------------------------------------------------
dat[1:10, ]                           # standard things

## ----results='hide'------------------------------------------------------
arrange(flights, year, month, day)    # dplyr
dat[~order(year, month, day)]         # #1

## ------------------------------------------------------------------------
dat %>% mutar(~order(year, month, day)) # #2

## ----results='hide'------------------------------------------------------
select(flights, year, month, day)       # dplyr 
select(flights, year:day)               # dplyr

# characters are passed into dplyr::select_:
dat %>%
  mutar(c("year", "month", "day")) %>%
  mutar("year:day")

dat[c("year", "month", "day")]["year:day"]

## ------------------------------------------------------------------------
dat[is.numeric] # or mutar(dat, is.numeric)

## ----results='hide'------------------------------------------------------
dat[function(x) any(is.na(x))]

## ----results='hide'------------------------------------------------------
flights %>% 
  mutate(gain = arr_delay - dep_delay,
         speed = distance / air_time * 60)

dat[gain ~ arr_delay - dep_delay,
    speed ~ distance / air_time * 60]

## ------------------------------------------------------------------------
dat %>%
  mutar(gain ~ arr_delay - dep_delay,
        speed ~ distance / air_time * 60) %>%
  mutar("^gain|speed$") # I assume a regex if the character begins with '^'

## ----results='hide'------------------------------------------------------
group_by(flights, month) %>% 
    summarise(delay = mean(dep_delay, na.rm = TRUE))

dat[delay ~ mean(dep_delay, na.rm = TRUE), 
    by = "month"]

## ------------------------------------------------------------------------
mutar(dat, 
      delay ~ mean(dep_delay, na.rm = TRUE), 
      by = "month")

## ------------------------------------------------------------------------
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

## ------------------------------------------------------------------------
# dat #2:
dat %>%
  mutar(count ~ n(),
        dist ~ mean(distance, na.rm = TRUE),
        delay ~ mean(arr_delay, na.rm = TRUE),
        by = "tailnum") %>%
  mutar(~count > 20 & dist < 2000)

## ------------------------------------------------------------------------
# The naive split-apply-combine
map(dat, mutar, By("month"), count ~ n()) %>% 
  mutar(27003:27006, j = c("month", "count"))

## ------------------------------------------------------------------------
dat %>%
  group_by(tailnum) %>%
  summarise(
    count = n(),
    dist = mean(distance, na.rm = TRUE),
    delay = mean(arr_delay, na.rm = TRUE)
  ) %>%
  mutar(~count > 20 & dist < 2000)

## ------------------------------------------------------------------------
library(data.table)
setOldClass(c("data.table", "data.frame"))

setClass("DataTable", "data.table")

DataTable <- function(...) {
  new("DataTable", data.table::data.table(...))
}

setMethod("[", "DataTable", mutar)

set.seed(1)
dat <- DataTable(id = rep(letters[1:2], each = 10), x = 101:120, y = rnorm(20))
dat[1:2, ]
dat %>% 
  mutar(~1:2, y ~ runif(2))

dat %>%
  mutar(sumOfX ~ sum(x), by = "id")

map(dat, mutar, By("id"), sumOfX ~ sum(x))[~1:2]
map(dat, dt ~ DataTable(sumOfX = sum(dt$x)), By("id"))

