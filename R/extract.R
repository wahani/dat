#' Extract elements from a vector
#'
#' @param x (ANY) a vector
#' @param f (function | formula | character | numeric | integer | logical) a
#'   formula is coerced into a function. For lists the function is applied to
#'   each element (and has to return a logical of length 1). For atomics a
#'   vectorized function is expected. If you supply an atomic it is used for
#'   subsetting. A character of length 1 beginning with "^" is interpreted as
#'   regular expression.
#' @param ... arguments passed to f
#'
#' @export
#' @rdname extract
#'
#' @examples
#' extract(1:15, ~ 15 %% . == 0)
#' extract(list(xy = 1, zy = 2), "^z")
#' extract(list(x = 1, z = 2), 1)
extract(x, f, ...) %g% standardGeneric("extract")

#' @export
#' @rdname extract
extract(x ~ list, f ~ "function", ...) %m% {
  x[vapply(x, f, logical(1), ...)]
}

#' @export
#' @rdname extract
extract(x ~ atomic, f ~ "function", ...) %m% {
  x[f(x, ...)]
}

#' @export
#' @rdname extract
extract(x ~ ANY, f ~ formula, ...) %m% {
  extract(x, as.function(f), ...)
}

#' @export
#' @rdname extract
extract(x ~ ANY, f ~ numeric | integer | logical, ...) %m% {
  x[f]
}

#' @export
#' @rdname extract
extract(x ~ ANY, f ~ character, ...) %m% {
  ind <- if (length(f) == 1 && grepl("^\\^", f)) {
    grepl(f, names(x), ...)
  } else f
  x[ind]
}
