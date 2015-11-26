test_that("as.function", {

  equals <- function(x, y) {
    testthat::expect_equal(x, y)
  }

  is <- function(x, a) {
    testthat::expect_is(x, a)
  }

  as.function(~.[[1]])(list(1)) %>% equals(1)
  as.function(x ~ x[[1]])(list(1)) %>% equals(1)
  as.function(f(x) ~ x[1])(list(1)) %>% equals(list(1))
  as.function(f(x, y) ~ x + y)(1, 2) %>% equals(3)

  as.function(numeric : f(x, y) ~ x + y) %>% is("FunctionWithType")
  as.function(numeric : x ~ x[[1]]) %>% is("FunctionWithType")

  as.function(numeric(0) : f(x) ~ x) %>% is("FunctionWithPrototype")

})
