#' S4 list
#'
#' A type inheriting from list. Mainly used for dispatch in \link{map}.
#'
#' @param ... arguments passed to \link{list}
#'
#' @export
#' @rdname List
list : List() %type% .Object

#' @export
#' @rdname List
L <- function(...) new("List", list(...))

#' @export
#' @rdname List
List <- function(...) new("List", list(...))
