#' Reduce
#'
#' Very similar to \link{Reduce}.
#'
#' @param x (ANY) a vector to be reduced
#' @param f (function | formula) a binary function
#' @param init an optional initial value
#'
#' @export
#' @rdname reduce
#'
#' @examples
#' add <- function(x) reduce(x, `+`)
#' add(1:3)
#'
#' # Standard but inefficient:
#' reduce(as.list(1:3), c)
#' # Using Dots can be more efficient when the function itself already knows
#' # how to reduce a vector into a scalar.
#' reduce(as.list(1:3), Dots(c))
#' # The result is the same as
#' do.call(c, as.list(1:3))
reduce(x, f, init) %g% {
  f <- as.function(f)
  for (el in x) init <- f(init, el)
  init
}

#' @export
#' @rdname reduce
reduce(x ~ ANY, f ~ "function | formula", init ~ missing) %m% {
  if (length(x) == 0) x
  else reduce(x[-1], f, x[[1]])
}

#' @export
#' @rdname reduce
reduce(x ~ ANY, f ~ FunctionWithDots, init ~ missing) %m% {
  do.call(what = f, args = as.list(x))
}
