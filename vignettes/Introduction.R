## ---- results='asis', echo=FALSE-----------------------------------------
cat(gsub("\\n   ", "", packageDescription("dat", fields = "Description")))

## ----eval=FALSE----------------------------------------------------------
#  devtools::install_github("wahani/dat")

## ------------------------------------------------------------------------
library(dat)
map(1:3, ~ .^2) # lapply
map(1:3, numeric(1) : x ~ x^2) # vapply
map(L(1:4, 4:1), integer : f(x, y) ~ rep(x, y)) # mapply + check return type
map(L(1:3, 11:13), c) # zip
map(L(1:3, 11:13), c) %>% 
  { map(do.call(L, .), c) } # unzip
dat <- DataFrame(x = 1, y = "")
map(dat, x ~ x + 1, is.numeric) # only operates on numeric cols

## ------------------------------------------------------------------------
library(nycflights13)
library(dplyr)

dat <- do.call(DataFrame, flights)
str(dat)

## ----results='hide'------------------------------------------------------
filter(flights, month == 1, day == 1)
dat[~ month == 1 & day == 1]
mutar(dat, ~ month == 1 & day == 1)

slice(flights, 1:10)
dat[~1:10]
dat %>% mutar(~1:10)

# It is truly amazing how many times I tried to subset a data frame with
# dat[1:10] and meant rows. Thats why this is working:
dat[~1:10]

## ------------------------------------------------------------------------
dat[1:10, ]

## ----results='hide'------------------------------------------------------
arrange(flights, year, month, day)
dat[~order(year, month, day)]

## ------------------------------------------------------------------------
dat %>% mutar(~order(year, month, day))

## ----results='hide'------------------------------------------------------
select(flights, year, month, day)
select(flights, year:day)

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
  mutar("^gain|speed$") # I assume a regex if the character begin with '^'

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
dat %>%
  group_by(tailnum) %>%
  summarise(
    count = n(),
    dist = mean(distance, na.rm = TRUE),
    delay = mean(arr_delay, na.rm = TRUE)
  ) %>%
  mutar(~count > 20 & dist < 2000)

