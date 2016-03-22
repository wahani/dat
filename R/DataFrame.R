#' DataFrame and methods
#'
#' There are many ways to use a DataFrame. Please see the vignette to see some
#' examples. \code{mutar} is literally the same function as \code{[.DataFrame}
#' and can be used as a generic interface to use dplyr.
#'
#' @include helper.R
#'
#' @param x (DataFrame | data.frame)
#' @param i (logical | numeric | integer | OneSidedFormula | TwoSidedFormula)
#' @param j (logical | character | TwoSidedFormula | function) character
#'   beginning with '^' are interpreted as regular expression
#' @param ... arbitrary number of args
#'    \cr in \code{[} (TwoSidedFormulas)
#'    \cr in constructor see \link[dplyr]{data_frame}
#' @param by (character) variable name
#' @param drop ignored
#'
#' @details
#' \code{OneSidedFormula} is always used for subsetting rows.
#'
#' \code{TwoSidedFormula} is used instead of name-value expressions in
#' \code{summarise} and \code{mutate}.
#'
#' @examples
#' \dontrun{
#'   vignette("Introduction", "dat")
#' }
#'
#' @rdname DataFrame
#' @export
DataFrame <- function(...) {
  dat <- dplyr::data_frame(...)
  addClass(dat, "DataFrame")
}

# To make the inheritance of DataFrame clear for S4:
setOldClass(c("DataFrame", "data.frame"))
# dplyr functions destroy the class you give them, so we need to explain that
# they are data.frames:
setOldClass(c("tbl_df", "data.frame"))

#' @rdname DataFrame
#' @export
as.DataFrame <- function(x, ...) UseMethod("as.DataFrame")

#' @rdname DataFrame
#' @export
as.DataFrame.default <- function(x, ...) {
  do.call(DataFrame, as.list(x))
}

#' @rdname DataFrame
#' @export
as.DataFrame.data.frame <- function(x, ...) {
  addClass(x, c("DataFrame", "tbl_df", "tbl", "data.frame"))
}

#' @rdname DataFrame
#' @export
"[.DataFrame" <- function(x, i, j, ..., by, drop) {
  # this is basically the dispatch function. I do NOT use default values because
  # missing arguments have a meaning in the special syntax expected from [

  # Assertions:
  stopifnot(inherits(x, "data.frame"))

  # This is hands on dispatch for missing arguments:
  j <- if (missing(j)) NULL else j
  j <- if (!missing(i) && nargs() == 2) i else j
  i <- if (missing(i) || nargs() == 2) NULL else i
  by <- if (missing(by)) NULL else by
  memClassHandler <- MemClassHandler()

  x %>%
    memClassHandler$memClass() %>%
    handleRows(dispatcher(i)) %>%
    handleCols(dispatcher(i), dispatcher(j), ..., by = dispatcher(by)) %>%
    memClassHandler$wrapClass()

}

#' @rdname DataFrame
#' @export
mutar <- `[.DataFrame`

data.frame : handleRows(x, i) %g% standardGeneric("handleRows")

handleRows(x ~ data.frame, i ~ NULL) %m% x

handleRows(x ~ data.frame, i ~ logical) %m% {
  .__i__ <- i
  dplyr::filter(x, .__i__)
}

handleRows(x ~ data.frame, i ~ numeric | integer) %m% {
  ".__i__" <- i
  dplyr::slice(x, .__i__)
}

handleRows(x ~ data.frame, i ~ OneSidedFormula) %m% {
  envir <- environment(i)
  expr <- parse(text = sub("~", "", deparse(i)))
  handleRows(x, do.call(with, list(x, expr), envir = envir))
}

handleRows(x ~ data.frame, i ~ TwoSidedFormula) %m% x

################################################################################

data.frame : handleCols(x, i, j, ..., by) %g% standardGeneric("handleCols")

handleCols(x ~ data.frame, i ~ NULL, j ~ NULL, ..., by ~ NULL) %m% x

handleCols(x ~ data.frame, i ~ NULL, j ~ character, ..., by ~ NULL) %m% {
  dplyr::select_(x, .dots = j)
}

handleCols(x ~ data.frame, i ~ NULL, j ~ RegEx, ..., by ~ NULL) %m% {
  dplyr::select(x, matches(j))
}

handleCols(x ~ data.frame, i ~ NULL, j ~ logical, ..., by ~ NULL) %m% {
  `[.data.frame`(x, j)
}

handleCols(x ~ data.frame, i ~ NULL, j ~ "function", ..., by ~ NULL) %m% {
  mutar(x, vapply(x, j, logical(1)))
}

handleCols(x ~ data.frame, i ~ NULL, j ~ OneSidedFormula, ..., by ~ NULL) %m% {
  handleRows(x, j)
}

handleCols(x ~ data.frame,
           i ~ logical | numeric | integer | OneSidedFormula,
           j ~ ANY, ..., by ~ ANY) %m% {
             handleCols(x, NULL, j, ..., by = by)
           }

handleCols(x ~ data.frame,
           i ~ TwoSidedFormula | NULL,
           j ~ TwoSidedFormula | NULL,
           ..., by ~ NULL) %m% {
             args <- constructArgs(i, j, ...)
             dplyr::mutate_(x, .dots = args)
           }

handleCols(x ~ data.frame,
           i ~ TwoSidedFormula | NULL,
           j ~ TwoSidedFormula | NULL,
           ..., by ~ character) %m% {
             args <- constructArgs(i, j, ...)
             dplyr::group_by_(x, .dots = by) %>%
               dplyr::summarise_(.dots = args) %>%
               as.data.frame
           }
