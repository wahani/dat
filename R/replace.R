#' Replace elements in a vector
#'
#' This function replaces elements in a vector. It is a link to
#' \link[base]{replace} as a generic function.
#'
#' @param x (atomic | list) a vector.
#' @inheritParams extract
#' @param values the values for replacement.
#' @param ... arguments passed to \code{ind} if it can be interpreted as
#'   function. For a regex arguments are passed to \link{grep}.
#'
#' @export
#' @rdname replace
#'
#' @examples
#' replace(c(1, 2, NA), is.na, 0)
#' replace(c(1, 2, NA), rep(TRUE, 3), 0)
#' replace(c(1, 2, NA), 3, 0)
#' replace(list(x = 1, y = 2), "x", 0)
replace(x, ind, values, ...) %g% standardGeneric("replace")

#' @export
#' @rdname replace
replace(x ~ list, ind ~ "function", values, ...) %m% {
  replace(x, vapply(x, ind, logical(1), ...), values)
}

#' @export
#' @rdname replace
replace(x ~ atomic, ind ~ "function", values, ...) %m% {
  replace(x, ind(x, ...), values)
}

#' @export
#' @rdname replace
replace(x ~ atomic | list, ind ~ integer | numeric | logical, values, ...) %m% {
  base::replace(x, ind, values)
}

#' @export
#' @rdname replace
replace(x ~ ANY, ind ~ formula, values, ...) %m% {
  replace(x, as.function(ind), values, ...)
}

#' @export
#' @rdname replace
replace(x ~ ANY, ind ~ character, values, ...) %m% {
  ind <- if (length(ind) == 1 && grepl("^\\^", ind)) {
    grep(ind, names(x), ...)
  } else {
    names(x) %in% ind
  }
  replace(x, ind, values, ...)
}
