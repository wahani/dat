#' Helper Types
#'
#' @include bindRows.R
#'
#' Different types used for dispatch. \code{ML} and \code{MList} are constructor
#' functions for a list (used in \link{map} to indicate multivariate map).
#'
#' @param ... arguments passed to \link{list}
#' @param var (character) the variable names to use in split
#' @param combine (function) a function which know how to combine the list of
#'   results. \link{bindRows} is the default.
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
By(var ~ character, combine = bindRows) %type% .Object

#' @export
#' @rdname HelperTypes
By <- function(var, combine = bindRows, ...) {
  new("By", var = var, combine = combine, ...)
}
