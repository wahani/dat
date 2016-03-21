#' Replace elements in a vector
#'
#' This function replaces elements in a vector.
#'
#' @inheritParams extract
#'
#' @param with the values for replacement
#' @param ... arguments passed to f
#'
#' @export
#' @rdname replace
#'
#' @examples
#' replace(c(1, 2, NA), is.na, 0)
#' replace(c(1, 2, NA), rep(TRUE, 3), 0)
#' replace(c(1, 2, NA), 3, 0)
#' replace(list(x = 1, y = 2), "x", 0)
replace(x, f, with, ...) %g% standardGeneric("replace")

#' @export
#' @rdname replace
replace(x ~ list, f ~ "function", with, ...) %m% {
  replace(x, which(vapply(x, f, logical(1), ...)), with)
}

#' @export
#' @rdname replace
replace(x ~ atomic, f ~ "function", with, ...) %m% {
  replace(x, which(f(x, ...)), with)
}

#' @export
#' @rdname replace
replace(x ~ atomic | list, f ~ integer | numeric, with, ...) %m% {
  base::replace(x, f, with)
}

#' @export
#' @rdname replace
replace(x ~ ANY, f ~ formula, with, ...) %m% {
  replace(x, as.function(f), with, ...)
}

#' @export
#' @rdname replace
replace(x ~ ANY, f ~ logical, with, ...) %m% {
  replace(x, which(f), with, ...)
}

#' @export
#' @rdname replace
replace(x ~ ANY, f ~ character, with, ...) %m% {
  ind <- if (length(f) == 1 && grepl("^\\^", f)) {
    grep(f, names(x), ...)
  } else {
    names(x) %in% f
  }
  replace(x, ind, with, ...)
}
