context("mutar data table")

# helper for testing:
isIdentical <- function(a, b) {
  unify <- function(x) {
    x <- as.data.frame(x)
    rownames(x) <- NULL
    x
  }
  identical(unify(a), unify(b))
}

expectIdentical <- function(a, b) {
  testthat::expect_true(isIdentical(a, b))
}

test_that("", {
  WITHOUT_DPLYR({

    dat <- DataFrame(x = 1:10, y = 11:20, yx = 1)
    expected <- dat
    expected$a <- expected$x + expected$y
    expected$b <- log(expected$a)
    expected$c <- expected$a
    expected$n <- as.integer(10)

    expectIdentical(
      dat[a ~ x + y][b ~ log(a), c ~ a][n ~ .N],
      expected
    )

  })
})
