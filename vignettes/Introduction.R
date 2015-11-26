## ---- results='asis', echo=FALSE-----------------------------------------
cat(gsub("\\n   ", "", packageDescription("dat", fields = "Description")))

## ----eval=FALSE----------------------------------------------------------
#  devtools::install_github("wahani/dat")

## ------------------------------------------------------------------------
library(dat)
dat <- DataFrame(x = 1, y = c(2, NA))
map(dat, x ~ x + 1, ~ !any(is.na(.)))
map(1:3, . ~ .^2) # lapply
map(1:3, numeric(1) : x ~ x^2) # vapply
map(L(1:4, 4:1), f(x, y) ~ rep(x, y)) # mapply
map(L(1:10, 11:20), c) # zip
map(L(1:10, 11:20), c) %>% 
  { map(do.call(L, .), c) } # unzip

## ------------------------------------------------------------------------
library(nycflights13)
library(dplyr)

dat <- do.call(DataFrame, flights)
str(dat)

## ------------------------------------------------------------------------
myIdentical <- function(a, b) {
  l <- lapply(list(a, b), as.data.frame)
  do.call(identical, l)
}

## ------------------------------------------------------------------------
dat %>%
  mutar(~1:10)

myIdentical(
  filter(flights, month == 1, day == 1),
  dat[~ month == 1 & day == 1]
)

myIdentical(
  slice(flights, 1:10),
  dat[~1:10]
)

myIdentical(
  # It is truly amazing how many times I tried to subset a data frame with
  # dat[1:10] and meant rows. Thats why this is working:
  dat[~1:10],
  dat[1:10, ]
)

## ------------------------------------------------------------------------
dat %>%
  mutar(~order(year, month, day))

myIdentical(
  arrange(flights, year, month, day),
  dat[~order(year, month, day)]
)

## ------------------------------------------------------------------------
dat %>%
  mutar(c("year", "month", "day")) %>%
  mutar("year:day")

myIdentical(
  select(flights, year, month, day),
  dat[c("year", "month", "day")]
)

myIdentical(
  select(flights, year:day),
  dat["year:day"] # characters are passed into dplyr::select_
)

## ------------------------------------------------------------------------
dat[is.numeric] # or mutar(dat, is.numeric)

## ------------------------------------------------------------------------
dat[function(x) any(is.na(x))]

## ------------------------------------------------------------------------
dat %>%
  mutar(gain ~ arr_delay - dep_delay,
        speed ~ distance / air_time * 60) %>%
  mutar("^gain|speed$")

myIdentical(
  mutate(flights,
         gain = arr_delay - dep_delay,
         speed = distance / air_time * 60),
  dat[gain ~ arr_delay - dep_delay,
      speed ~ distance / air_time * 60]
)

## ------------------------------------------------------------------------
dat %>%
  mutar(delay ~ mean(dep_delay, na.rm = TRUE), by = "month")

myIdentical(
  group_by(flights, month) %>% 
    summarise(delay = mean(dep_delay, na.rm = TRUE)),
  dat[delay ~ mean(dep_delay, na.rm = TRUE), by = "month"]
)

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
dat %>%
  mutar(count ~ n(),
        dist ~ mean(distance, na.rm = TRUE),
        delay ~ mean(arr_delay, na.rm = TRUE),
        by = "tailnum") %>%
  mutar(~count > 20 & dist < 2000)

# dat #2:
delay2 <- dat[
  count ~ n(),
  dist ~ mean(distance, na.rm = TRUE),
  delay ~ mean(arr_delay, na.rm = TRUE),
  by = "tailnum"
  ] %>%
  .[~count > 20 & dist < 2000]

myIdentical(delay1, delay2)

## ------------------------------------------------------------------------
dat %>%
  group_by(tailnum) %>%
  summarise(
    count = n(),
    dist = mean(distance, na.rm = TRUE),
    delay = mean(arr_delay, na.rm = TRUE)
  ) %>%
  mutar(~count > 20 & dist < 2000)

