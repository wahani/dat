test_that("replace", {

  expectEqual <- function(a, b) {
    testthat::expect_equal(a, b)
  }

  expectEqual(
    replace(c(1, 2, NA), ~ is.na(.), 3),
    1:3
  )

  expectEqual(
    replace(c(1, 2, NA), rep(TRUE, 3), 3),
    rep(3, 3)
  )

  expectEqual(
    replace(c(1, 2, NA), 3, 3),
    1:3
  )

  expectEqual(
    replace(list(x = 1, y = 2), "x", 0),
    list(x = 0, y = 2)
  )

  expectEqual(
    replace(list(x = 1, y = 2), "^x$", 0),
    list(x = 0, y = 2)
  )

  expectEqual(
    replace(list(x = 1, y = 2), ~ . == 1, 0),
    list(x = 0, y = 2)
  )

})
