#' Helper Types
#'
#' Defferent types used for dispatch.
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

#' @export
#' @rdname HelperTypes
FunctionWithDots <- Dots
