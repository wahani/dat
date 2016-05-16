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
  ind <- if (length(ind) == 1 && grepl("^\\^", ind)) {
    grepl(ind, names(x), ...)
  } else {
    names(x) %in% ind
  }
  extract(x, ind)
}
