#' Bind rows
#'
#' This is a wrapper around \link[dplyr]{bind_rows} to preserve the input class.
#'
#' @param x (list)
#' @param id passed to \link[dplyr]{bind_rows} as \code{.id}
#'
#' @return
#' If the first element of \code{x} inherits from \code{data.frame} the type
#' that first eleent.
#'
#' \code{x} else.
#'
#' @export
bindRows <- function(x, id = NULL) {
  if (inherits(x[[1]], "data.frame")) {
    memClassHandler <- MemClassHandler()
    memClassHandler$memClass(x[[1]])
    dplyr::bind_rows(x, .id = id) %>% memClassHandler$wrapClass()
  } else {
    x
  }
}
