#' Helper Types
#'
#' Different types used for dispatch. \code{ML} and \code{MList} are constructor
#' functions for a list (used in \link{map} to indicate multivariate map). A
#' MList is just a S4 list. You probably do not need to use this type
#' interactively.
#'
#' @param ... arguments passed to \link{list}
#'
#' @export
#' @rdname HelperTypes
#'
#' @examples
#' MList(x = 1, y = "a")
list : MList() %type% .Object

#' @export
#' @rdname HelperTypes
ML <- function(...) new("MList", list(...))

#' @export
#' @rdname HelperTypes
MList <- function(...) new("MList", list(...))
