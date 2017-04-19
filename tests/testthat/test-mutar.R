context("mutar")

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

test_that("mutars by and sby", {

  dat <- DataFrame(x = 1:10, y = 11:20, yx = 1)

  expectIdentical(
    dat[id ~ x > 4][count ~ n(), by = "id"],
    dat %>%
      mutar(id ~ x > 4) %>%
      mutar(count ~ n(), by = "id")
  )

  expectIdentical(
    mutar(dat, gr ~ c(rep(1, 5), rep(2, 5))) %>%
      mutar(xMean ~ mean(x), by = "gr"),
    mutar(dat, gr ~ c(rep(1, 5), rep(2, 5))) %>%
      sac(mutar, "gr", xMean ~ mean(x))
  )

  expectIdentical(
    dat[~x > 5],
    filtar(dat, ~x > 5)
  )

  expectIdentical(
    dat[m ~ mean(x), sby = "yx"],
    sumar(dat, m ~ mean(x), by = "yx")
  )

})

test_that("S4 stuff", {

  expectIs <- function(x, a) {
    testthat::expect_is(x, a)
  }

  expectTrue <- function(x) {
    testthat::expect_true(x)
  }

  DataFrame : SomeData() %type% .Object

  dat <- SomeData(DataFrame(x = 1))
  dat %<>%
    mutar(~x == 1) %>%
    mutar("^x$") %>%
    mutar(y ~ x)

  expectIs(dat, "SomeData")
  expectTrue(isS4(dat))
  expectIdentical(S3Part(dat, TRUE), data.frame(x = 1, y = 1))

})

test_that("Scoping", {

  expectEqual <- function(x, a) {
    testthat::expect_equal(x, a)
  }

  fun <- function(val) {
    DataFrame(x = val) %>% mutar(y ~ val)
  }

  expectEqual(fun(1), DataFrame(x = 1, y = 1))

})

test_that("Parameterized formulas", {

  dat <- data.frame(x = 1, y = "", stringsAsFactors = FALSE)
  dat1 <- data.frame(x = 2, y = NA)

  someCol <- "x"

  dat <- mutar(
    dat,
    .n  ~ .n + 1 | someCol,
    .n  ~ .n | someCol,
    .n  ~ NA | is.character
  )

  testthat::expect_identical(dat, dat1)
  
})

test_that("Parameterized formulas with lists", {

  dat  <- data.frame(x = "", stringsAsFactors = FALSE)
  expectedResult <- data.frame(
    x = "1", y = "11", z = TRUE,
    stringsAsFactors = FALSE
  )

  dat <- mutar(
    dat,
    var ~ paste0(var, "1") | list(var = "x"),
    lhs ~ paste0(rhs, "1") | list(lhs = "y", rhs = "x"),
    var ~ (TRUE | FALSE) | list(var = "z")
  )

  testthat::expect_identical(dat, expectedResult)
  
})
