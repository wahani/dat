context("Data Frame")

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

test_that("Basic syntax of a DataFrame", {
  WITH_DPLYR({

    dat <- DataFrame(x = 1:10, y = 11:20, yx = 1)
    datRef <- as.data.frame(dat)

    expectIdentical(
      dat[c(rep(TRUE, 9), FALSE), ],
      datRef[c(rep(TRUE, 9), FALSE), ]
    )

    expectIdentical(
      dat[1:5, ],
      datRef[1:5, ]
    )

    expectIdentical(
      dat[- (1:2), ],
      datRef[- (1:2), ]
    )

    expectIdentical(
      dat[~order(x), ],
      datRef[order(datRef$x), ]
    )

    expectIdentical(
      dat["^x$"],
      datRef[grep("^x$", names(datRef))]
    )

    expectIdentical(
      dat[, "^x$"],
      datRef[, grep("^x$", names(datRef))]
    )

    expectIdentical(
      dat[c("x", "y")],
      datRef[c("x", "y")]
    )

    expectIdentical(
      dat["^y"],
      datRef[grep("^y", names(datRef))]
    )

    expectIdentical(
      replace(dat, "x", NULL),
      datRef[!(names(datRef) %in% "x")]
    )

    expectIdentical(
      dat[c(TRUE, FALSE, TRUE)],
      datRef[c(TRUE, FALSE, TRUE)]
    )

    expectIdentical(
      dat[c(FALSE, TRUE)],
      datRef[c(FALSE, TRUE)]
    )

    expectIdentical(
      dat[~x == 1, ],
      datRef[datRef$x == 1, ]
    )

    expectIdentical(
      dat[newVar ~ x + 1],
      local({
        datRef["newVar"] <- datRef$x + 1
        datRef
      })
    )

    expectIdentical(
      dat[newVar ~ x + 1,
          newVar1 ~ x + 2],
      local({
        datRef["newVar"] <- datRef$x + 1
        datRef["newVar1"] <- datRef$x + 2
        datRef
      })
    )

    expectIdentical(
      dat[,
          newVar ~ x + 1,
          newVar1 ~ x + 2],
      local({
        datRef["newVar"] <- datRef$x + 1
        datRef["newVar1"] <- datRef$x + 2
        datRef
      })
    )

    expectIdentical(
      dat[~ x == 1,
          newVar ~ x + 1,
          newVar1 ~ x + 2],
      local({
        datRef <- datRef[datRef$x == 1, ]
        datRef["newVar"] <- datRef$x + 1
        datRef["newVar1"] <- datRef$x + 2
        datRef
      })
    )

    expectIdentical(
      dat[id ~ x > 4][count ~ dplyr::n(), sby = "id"],
      local({
        datRef$id <- datRef$x > 4
        datRef <- aggregate(x ~ id, datRef, length)
        names(datRef)[2] <- "count"
        datRef
      })
    )

    expectIdentical(
      dat[is.numeric],
      datRef[sapply(datRef, is.numeric)]
    )

    expectIdentical(
      dat[],
      datRef[]
    )

  })
})

test_that("Type conversion", {

  dat <- data.frame(x = 1)
  testthat::expect_is(as.DataFrame(dat), "DataFrame")
  testthat::expect_is(as.DataFrame(dat), "data.frame")
  testthat::expect_is(as.DataFrame(dat), "tbl_df")

  testthat::expect_is(as.DataFrame(list(x = 1)), "DataFrame")
  testthat::expect_is(as.DataFrame(list(x = 1)), "data.frame")
  testthat::expect_is(as.DataFrame(list(x = 1)), "tbl_df")

})
