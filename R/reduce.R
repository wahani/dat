#' Reduce
#'
#' @param x (ANY) a vector to be reduced
#' @param f (function | formula) a binary function
#' @param init an optional initial value
#'
#' @export
#' @rdname reduce
reduce(x, f, init = NULL) %g% standardGeneric("reduce")

#' @export
#' @rdname reduce
reduce(x ~ ANY, f ~ "function", init ~ ANY) %m% {
  .reduce <- function(x, f, init) {
    if (is.null(init)) {
      init <- x[1]
      x <- x[-1]
    }
    for (el in x) init <- f(init, el)
    init
  }
  # browser()
  if (length(x) == 0 && length(init) == 0) x
  else if (length(x) == 0 && !is.null(init)) init
  else if (names(formals(args(f)))[1] == "...") do.call(f, as.list(c(init, x)))
  else .reduce(x, f, init)
}

#' @export
#' @rdname reduce
reduce(x ~ ANY, f ~ formula, init ~ ANY) %m% {
  reduce(x, as.function(f), init)
}
