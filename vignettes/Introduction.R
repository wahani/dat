## ---- results='asis', echo=FALSE-----------------------------------------
cat(gsub("\\n   ", "", packageDescription("dat", fields = "Description")))

## ------------------------------------------------------------------------
library(nycflights13)
library(dplyr)
library(dat)

dat <- do.call(DataFrame, flights)
str(dat)

myIdentical <- function(a, b) {
  l <- lapply(list(a, b), as.data.frame)
  do.call(identical, l)
}

myIdentical(
  filter(flights, month == 1, day == 1),
  dat[~ month == 1 & day == 1]
)

myIdentical(
  slice(flights, 1:10),
  dat[~1:10]
)

myIdentical(
  dat[~1:10],
  dat[1:10, ]
)

myIdentical(
  arrange(flights, year, month, day),
  dat[~order(year, month, day)]
)

myIdentical(
  select(flights, year, month, day),
  dat[c("year", "month", "day")]
)

myIdentical(
  select(flights, year:day),
  dat["year:day"]
)

dat[is.numeric]
dat[function(x) any(is.na(x))]

# there is no rename
# no distinct

myIdentical(
  mutate(flights,
         gain = arr_delay - dep_delay,
         speed = distance / air_time * 60),
  dat[gain ~ arr_delay - dep_delay,
      speed ~ distance / air_time * 60]
)

# no transmute

myIdentical(
  summarise(group_by(flights, month), delay = mean(dep_delay, na.rm = TRUE)),
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

# dat:
delay2 <- dat[
  count ~ n(),
  dist ~ mean(distance, na.rm = TRUE),
  delay ~ mean(arr_delay, na.rm = TRUE),
  by = "tailnum"
  ] %>%
  .[~count > 20 & dist < 2000]

myIdentical(delay1, delay2)



