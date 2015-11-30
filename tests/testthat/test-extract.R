test_that("subset with extract", {

  isEqual <- function(a, b) {
    testthat::expect_equal(a, b)
  }

  is.even <- function(x) (x %% 2) == 0

  isEqual(extract(1:10, is.even), c(2, 4, 6, 8, 10))
  isEqual(extract(1:10, ~ . %% 2 == 0), c(2, 4, 6, 8, 10))
  isEqual(extract(1:10, c(FALSE, TRUE)), c(2, 4, 6, 8, 10))
  isEqual(extract(1:10, 1:2), 1:2)

  isEqual(extract(data.frame(x = 1, y = ""), is.numeric), data.frame(x = 1))
  isEqual(extract(list(x = 1, y = ""), is.numeric), list(x = 1))
  isEqual(extract(list(x = 1, y = ""), "y"), list(y = ""))

  isEqual(extract(list(xy = 1, zy = 2), "^z"), list(zy = 2))

})
