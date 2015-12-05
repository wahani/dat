test_that("reduce reduces", {

  isEqual <- function(a, b) {
    testthat::expect_equal(a, b)
  }

  isEqual(reduce(1:5, `+`), 15)
  isEqual(reduce(1:5, sum), 15)
  isEqual(reduce(1:5, sum, 1), 16)
  isEqual(reduce(1:5, `+`, 1), 16)
  isEqual(reduce(letters, paste0, ""), paste(letters, collapse = ""))
  isEqual(reduce(list(), c, list()), list())
  isEqual(reduce(list(), f(x, y) ~ c(x, y), list()), list())
  isEqual(reduce(list(), f(x, y) ~ c(x, y)), list())
  isEqual(reduce(1:1e2, Dots(c)), 1:1e2)

})
