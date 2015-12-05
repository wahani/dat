#' Helper Types
#'
#' Defferent types used for dispatch. \code{ML} and \code{MList} are constructor
#' functions for a list (used in \link{map} to indicate multivariate map).
#' \code{Dots} wraps a function in a type to declare that it knows how to deal
#' with an arbitrary number of arguments (used in \link{reduce} instead of
#' \code{do.call}).
#'
#' @param ... arguments passed to \link{list}
#' @param f (function)
#'
#' @export
#' @rdname HelperTypes
list : MList() %type% .Object

#' @export
#' @rdname HelperTypes
ML <- function(...) new("MList", list(...))

#' @export
#' @rdname HelperTypes
MList <- function(...) new("MList", list(...))

#' @export
#' @rdname HelperTypes
"function" : FunctionWithDots() %type% {
  stopifnot(names(formals(args(.Object)))[1] == "...")
  .Object
}

#' @export
#' @rdname HelperTypes
Dots <- function(f) {
  force(f)
  new("FunctionWithDots", function(...) f(...))
}
