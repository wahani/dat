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

test_that("mutar without [s]by", {
  WITHOUT_DPLYR({

    ## Some basic functionality
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

    ## We do not expect reference semantics
    dat <- DataFrame(x = 1:10, y = 11:20, yx = 1)
    dump <- dat[a ~ x + y]
    expectIdentical(
      dat,
      DataFrame(x = 1:10, y = 11:20, yx = 1)
    )

    ## ... especially not for data tables
    dat <- data.table::as.data.table(DataFrame(x = 1:10, y = 11:20, yx = 1))
    dump <- mutar(dat, a ~ x + y)
    expectIdentical(
      dat,
      DataFrame(x = 1:10, y = 11:20, yx = 1)
    )

    ## not so easy to override existing columns with data.table
    dat <- data.table::as.data.table(DataFrame(x = 1:10))
    dat <- mutar(dat, x ~ x + 1)
    expectIdentical(
      dat,
      DataFrame(x = 1:10 + 1)
    )

    ## but we can disable it:
    withReference({
      dat <- data.table::as.data.table(DataFrame(x = 1:2, y = 2))
      mutar(dat, x ~ as.integer(x + 1), y ~ 2 + 2)
      expectIdentical(
        dat,
        DataFrame(x = 2:3, y = 4)
      )
    })

    ## Referenced variables can be found
    dat <- DataFrame(x = 1:2)
    res <- local({
      val <- 2
      dat[a ~ val, b ~ mean(x + val)]
    })
    expectIdentical(
      res,
      DataFrame(x = 1:2, a = 2, b = 3.5)
    )
  })
})

test_that("mutar with by", {
  WITHOUT_DPLYR({

    dat <- DataFrame(x = 1:10, y = 11:20, group = rep(letters[1:2], 5))

    expectIdentical(
      dat[a ~ mean(x)],
      cbind(dat, list(a = mean(dat$x)))
    )

    expectIdentical(
      dat[a ~ mean(x), by = "group"][~ order(x)],
      cbind(dat, list(a = as.numeric(rep(5:6, 5))))
    )

    dat <- DataFrame(
      x = 1:10, y = 11:20,
      group1 = rep(letters[1:2], 5),
      group2 = rep(letters[3:4], each = 5)
    )

    expectIdentical(
      dat[a ~ mean(x), by = c("group2", "group1")][~ order(x)],
      cbind(dat, list(a = as.numeric(rep(c(3, 8), each = 5))))[order(dat$x), ]
    )

    dat <- data.table::as.data.table(DataFrame(x = 1:2, group = c("a", "b")))
    dat <- mutar(dat, x ~ x + 1, by = "group")
    expectIdentical(
      dat,
      DataFrame(group = c("a", "b"), x = as.numeric(2:3))
    )

    withReference({
      dat <- data.table::as.data.table(DataFrame(x = 1:2, group = c("a", "b")))
      mutar(dat, x ~ as.integer(x + 1), by = "group")
      expectIdentical(
        dat,
        DataFrame(group = c("a", "b"), x = 2:3)
      )
    })

    dat <- DataFrame(x = 1:2, group = letters[1:2])
    res <- local({
      val <- 2
      dat[a ~ val, b ~ mean(x + val), by = "group"]
    })
    expectIdentical(
      res,
      DataFrame(x = 1:2, group = c("a", "b"), a = 2, b = as.numeric(3:4))
    )

  })
})

test_that("mutar with sby", {
  WITHOUT_DPLYR({

    dat <- DataFrame(x = 1:10, group = rep(letters[1:2], 5))

    expectIdentical(
      dat[a ~ mean(x), sby = "group"],
      DataFrame(group = c("a", "b"), a = as.numeric(5:6))
    )

    dat <- DataFrame(
      x = 1:10,
      group1 = rep(letters[1:2], 5),
      group2 = rep(letters[3:4], each = 5)
    )

    expectIdentical(
      dat[a ~ mean(x), b ~ .N, sby = c("group2", "group1")],
      DataFrame(
        group2 = c("c", "c", "d", "d"),
        group1 = c("a", "b", "a", "b"),
        a = as.numeric(c(3, 3, 8, 8)),
        b = as.integer(c(3, 2, 2, 3))
      )
    )

    dat <- DataFrame(x = 1:10, group = rep(letters[1:2], 5))
    res <- local({
      val <- 2
      dat[a ~ val, b ~ mean(x + val), sby = "group"]
    })
    expectIdentical(
      res,
      DataFrame(group = c("a", "b"), a = 2, b = as.numeric(7:8))
    )

  })
})
