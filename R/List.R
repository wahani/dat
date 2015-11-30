#' S4 list
#'
#' A type inheriting from list. Used for dispatch in \link{map}.
#'
#' @param ... arguments passed to \link{list}
#'
#' @export
#' @rdname MList
list : MList() %type% .Object

#' @export
#' @rdname MList
ML <- function(...) new("MList", list(...))

#' @export
#' @rdname MList
MList <- function(...) new("MList", list(...))
