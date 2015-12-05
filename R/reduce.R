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
