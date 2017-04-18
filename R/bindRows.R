#' Bind rows
#'
#' This is a wrapper around \link[data.table]{rbindlist} to preserve the input
#' class.
#'
#' @param x (list) a list of data frames
#' @param id,useNames,fill passed to \link[data.table]{rbindlist}
#' 
#' @return
#' If the first element of \code{x} inherits from \code{data.frame} the type
#' that first element. 
#'
#' \code{x} else.
#'
#' @export
bindRows <- function(x, id = NULL, useNames = TRUE, fill = TRUE) {
  if (inherits(x[[1]], "data.frame")) {
    memClassHandler <- MemClassHandler()
    memClassHandler$memClass(x[[1]])
    ret <- data.table::rbindlist(x, idcol = id, use.names = useNames, fill = fill)
    ret <- as.data.frame(ret)
    memClassHandler$wrapClass(ret)
  } else {
    x
  }
}
