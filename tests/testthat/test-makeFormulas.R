context("Dynamic formulas")

expectIdenticalFormula <- function(a, b) {
  testthat::expect_true(identical(a, dat:::TwoSidedFormula(b)))
}

expectIdentical <- function(a, b) {
  testthat::expect_identical(a, b)
}

test_that("make formulas", {
  
  tmp <- FL(.nMean ~ mean(.n), .nSd ~ sd(.n), .n = letters[1:2])

  expectIdenticalFormula(tmp[[1]], aMean ~ mean(a))
	expectIdenticalFormula(tmp[[2]], bMean ~ mean(b))
	expectIdenticalFormula(tmp[[3]], aSd ~ sd(a))
	expectIdenticalFormula(tmp[[4]], bSd ~ sd(b))

  tmp <- FL(.nMean ~ mean(.n), bMean ~ mean(b), .n = "a")
  expectIdenticalFormula(tmp[[1]], aMean ~ mean(a))
	expectIdenticalFormula(tmp[[2]], bMean ~ mean(b))

  dat <- data.frame(x = 1:10, group = rep(1:2, 5))
  dat1 <- data.frame(group = 1:2, x = as.numeric(5:6))
  dat2 <- data.frame(x = 5.5, group = rep(1:2, 5))
  dat3 <- data.frame(x = 1.5, group = 1:2)

  expectIdentical(
    dat1,
    mutar(dat, FL(x ~ mean(x)), sby = "group")
  )

  expectIdentical(
    dat2,
    mutar(dat, FL(x ~ mean(x)))
  )
  
  expectIdentical(
    dat3,
    mutar(dat, ~1:2, FL(x ~ mean(x)))
  )
  
})
