#' Replace elements in a vector
#'
#' This function replaces elements in a vector. It is a link to
#' \link[base]{replace} as a generic function.
#'
#' @param x (atomic | list) a vector.
#' @param ind used as index for elements to be replaced. See details.
#' @param values the values used for replacement.
#' @param ... arguments passed to \code{ind} if it can be interpreted as
#'   function. For a regex arguments are passed to \link{grep}.
#'
#' @details The idea is to provide a more flexible interface for the
#'   specification of the index. It can be a character, numeric, integer or
#'   logical which is then simply used in \code{base::replace}. It can be a
#'   regular expression in which case \code{x} should be named -- a character of
#'   length 1 and a leading "^" is interpreted as regex. When \code{ind} is a
#'   function (or formula) and \code{x} is a list then it should be a predicate
#'   function -- see the examples. When x is an atomic the function is applied
#'   on x and the result is used for subsetting.
#'
#' @export
#' @rdname replace
#'
#' @examples
#' replace(c(1, 2, NA), is.na, 0)
#' replace(c(1, 2, NA), rep(TRUE, 3), 0)
#' replace(c(1, 2, NA), 3, 0)
#' replace(list(x = 1, y = 2), "x", 0)
#' replace(list(x = 1, y = 2), "^x$", 0)
#' replace(list(x = 1, y = "a"), is.character, NULL)
setGeneric("replace", function(x, ind, values, ...) base::replace(x, ind, values))

#' @export
#' @rdname replace
setMethod("replace", c("ANY", "function"), function(x, ind, values, ...) {
  if (is.atomic(x)) replace(x, ind(x, ...), values)
  else replace(x, vapply(x, ind, logical(1), ...), values)
})

#' @export
#' @rdname replace
setMethod("replace", c("ANY", "formula"), function(x, ind, values, ...) {
  replace(x, as.function(ind), values, ...)
})

#' @export
#' @rdname replace
setMethod("replace", c("ANY", "character"), function(x, ind, values, ...) {
  ind <- if (length(ind) == 1 && grepl("^\\^", ind)) {
    grep(ind, names(x), ...)
  } else {
    stopifnot(all(ind %in% names(x)))
    names(x) %in% ind
  }
  replace(x, ind, values, ...)
})
