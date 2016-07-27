#' Extract elements from a vector
#'
#' Extract elements from an object as S4 generic function. See the examples.
#'
#' @param x (atomic | list) a vector.
#' @param ind (function | formula | character | numeric | integer | logical) a
#'   formula is coerced into a function. For lists the function is applied to
#'   each element (and has to return a logical of length 1). For atomics a
#'   vectorized function is expected. If you supply an atomic it is used for
#'   subsetting. A character of length 1 beginning with "^" is interpreted as
#'   regular expression.
#' @param ... arguments passed to ind.
#'
#' @export
#' @rdname extract
#'
#' @examples
#' extract(1:15, ~ 15 %% . == 0)
#' extract(list(xy = 1, zy = 2), "^z")
#' extract(list(x = 1, z = 2), 1)
#' extract(list(x = 1, y = ""), is.character)
#'
#' # Example: even numbers:
#' is.even <- function(x) (x %% 2) == 0
#' sum((1:10)[is.even(1:10)])
#' extract(1:10, ~ . %% 2 == 0) %>% sum
#' extract(1:10, is.even) %>% sum
#'
#' # Example: factors of 15
#' extract(1:15, ~ 15 %% . == 0)
#'
#' # Example: relative prime numbers
#' gcd <- function(a, b) {
#'   .gcd <- function(a, b) if (b == 0) a else Recall(b, a %% b)
#'   flatmap(a ~ b, .gcd)
#' }
#'
#' extract(1:10, x ~ gcd(x, 10) == 1)
#'
#' # Example: real prime numbers
#' isPrime <- function(n) {
#'   .isPrime <- function(n) {
#'     iter <- function(i) {
#'       if (i * i > n) TRUE
#'       else if (n %% i == 0 || n %% (i + 2) == 0) FALSE
#'       else Recall(i + 6)
#'     }
#'     if (n <= 1) FALSE
#'     else if (n <= 3) TRUE
#'     else if (n %% 2 == 0 || n %% 3 == 0) FALSE
#'     else iter(5)
#'   }
#'   flatmap(n, x ~ .isPrime(x))
#' }
#'
#' extract(1:10, isPrime)
extract(x, ind, ...) %g% standardGeneric("extract")

#' @export
#' @rdname extract
extract(x ~ list, ind ~ "function", ...) %m% {
  x[vapply(x, ind, logical(1), ...)]
}

#' @export
#' @rdname extract
extract(x ~ atomic, ind ~ "function", ...) %m% {
  x[ind(x, ...)]
}

#' @export
#' @rdname extract
extract(x ~ ANY, ind ~ formula, ...) %m% {
  extract(x, as.function(ind), ...)
}

#' @export
#' @rdname extract
extract(x ~ atomic | list, ind ~ numeric | integer | logical, ...) %m% {
  x[ind]
}

#' @export
#' @rdname extract
extract(x ~ ANY, ind ~ character, ...) %m% {
  stopifnot(!is.null(names(x)))
  ind <- if (length(ind) == 1 && grepl("^\\^", ind)) {
    grepl(ind, names(x), ...)
  } else {
    names(x) %in% ind
  }
  extract(x, ind)
}

#' @export
#' @rdname extract
extract(x ~ data.frame, ind ~ character, ...) %m% {
  mutar(x, , j = ind)
}

#' @export
#' @rdname extract
extract2(x, ind, ...) %g% standardGeneric("extract2")

#' @export
#' @rdname extract
extract2(x ~ atomic | list, ind ~ numeric | integer, ...) %m% {
  stopifnot(length(ind) == 1)
  x[[ind]]
}

#' @export
#' @rdname extract
extract2(x ~ ANY, ind ~ formula, ...) %m% {
  extract2(x, as.function(ind), ...)
}

#' @export
#' @rdname extract
extract2(x ~ atomic | list, ind ~ "function", ...) %m% {
  Find(addLengthCheck(addTypeCheck(ind, "logical"), 1), as.list(x), ...)
}

#' @export
#' @rdname extract
extract2(x ~ ANY, ind ~ character, ...) %m% {
  stopifnot(length(ind) == 1)
  stopifnot(!is.null(names(x)))
  ind <- if (grepl("^\\^", ind)) {
    grep(ind, names(x), ...)[1]
  } else {
    which(names(x) == ind)[1]
  }
  extract2(x, ind)
}
