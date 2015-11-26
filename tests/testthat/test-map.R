test_that("map", {

  equals <- function(x, y) {
    testthat::expect_equal(x, y)
  }

  dat <- data.frame(y = 1:10, z = 2)

  # data frames
  map(dat, x ~ x + 1) %>% equals(data.frame(y = 2:11, z = 3))
  map(dat %>% mutar(x ~ "x"), x ~ x + 1, is.numeric) %>%
    equals(data.frame(y = 2:11, z = 3, x = "x", stringsAsFactors = FALSE))
  map(dat, x ~ x + 1, x ~ all(x == 2)) %>% equals(data.frame(y = 1:10, z = 3))

  # vectors
  map(1, x ~ x) %>% equals(list(1))

  # subsetting
  map(list(1:2, 3:4), 2) %>% equals(list(2, 4))
  map(list(1:3, 2:5), 2:3) %>% equals(list(2:3, 3:4))
  map(list(1:3, 2:5), c(TRUE, FALSE, TRUE)) %>% equals(list(c(1, 3), c(2, 4, 5)))

  # multivariate map
  map(L(1:2, 3:4), f(x, y) ~ x + y) %>% equals(list(4, 6))
  map(L(1:2, 3:4), f(x, y) ~ x + y, simplify = TRUE) %>% equals(c(4, 6))
  map(List(1:2, 3:4), f(x, y, z) ~ x + y + z, z = 1) %>% equals(list(5, 7))

  # check return types
  testthat::expect_error(map(1:2, numeric : x ~ x))
  map(as.numeric(1:2), numeric : x ~ x) %>% equals(as.list(1:2))
  map(1:2, integer(1) : x ~ x) %>% equals(1:2)
  map(1:2, numeric(1) : x ~ x + 0.5) %>% equals(c(1.5, 2.5))

})

