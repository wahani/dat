#' Helper Types
#'
#' @include bindRows.R
#'
#' Different types used for dispatch. \code{ML} and \code{MList} are constructor
#' functions for a list (used in \link{map} to indicate multivariate map).
#'
#' @param ... arguments passed to \link{list}
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
