#' An implementation of map
#'
#' An implementation of map.
#'
#' @param x (\link{vector} | \link{data.frame}) if x inherits from data.frame, a
#'   data.frame is returned. Use \link{as.list} if this is not what you want.
#' @param f (\link{function} | \link{formula} | character | logical | numeric)
#'   something which can be interpreted as a function. formula objects are
#'   coerced to a function. atomics are used for subsetting in each element of
#'   x. See the examples.
#' @param p (function | formula) a predicate function indicating which columns
#'   in a data.frame to use in map.
#' @param useNames see USE.NAMES in \link{vapply}
#' @param simplify see SIMPLIFY in \link{mapply}
#'
#' @param ... further arguments passed to \link{lapply}, \link{vapply} or
#'   \link{mapply}
#'
#' @export
#' @rdname map
#'
#' @examples
#' map(data.frame(y = 1:10, z = 2), x ~ x + 1)
#' map(data.frame(y = 1:10, z = 2), x ~ x + 1, is.numeric)
#' map(data.frame(y = 1:10, z = 2), x ~ x + 1, x ~ all(x == 2))
#'
#' map(1, x ~ x)
#' map(list(1:2, 3:4), 2)
#' map(ML(1:2, 3:4), f(x, y) ~ x + y)
#' map(ML(1:2, 3:4), f(x, y) ~ x + y, simplify = TRUE)
#' map(ML(1:2, 3:4), f(x, y, z) ~ x + y + z, z = 1)
#'
#' map(list(1:3, 2:5), 2:3)
#' map(list(1:3, 2:5), c(TRUE, FALSE, TRUE))
#'
#' map(as.numeric(1:2), numeric : x ~ x)
#' map(1:2, integer(1) : x ~ x)
#' map(1:2, numeric(1) : x ~ x + 0.5)
map(x, f, ...) %g% {
  standardGeneric("map")
}

#' @export
#' @rdname map
map(x ~ ANY, f ~ "function", ...) %m% {
  lapply(x, f, ...)
}

#' @export
#' @rdname map
map(x ~ data.frame, f ~ "function", p = function(x) TRUE, ...) %m% {

  is.formula <- function(x) inherits(x, "formula")
  stopifnot(is.formula(p) | is.function(p))

  ind <- vapply(x, as.function(p), logical(1))
  x[ind] <- lapply(x[ind], f, ...)
  x

}

#' @export
#' @rdname map
map(x ~ ANY, f ~ formula, ...) %m% {
  map(x, as.function(f), ...)
}

#' @export
#' @rdname map
map(x ~ ANY, f ~ FunctionWithPrototype, ..., useNames = TRUE) %m% {
  vapply(X = x, FUN = f@fun, FUN.VALUE = f@prototype, ..., USE.NAMES = useNames)
}

#' @export
#' @rdname map
map(x ~ ANY, f ~ numeric | character | logical, ...) %m% {
  force(f)
  map(x, . ~ .[f], ...)
}

#' @export
#' @rdname map
map(x ~ MList, f ~ "function", ..., simplify = FALSE) %m% {
  do.call(mapply, c(list(FUN = f), x, SIMPLIFY = simplify, ...))
}
