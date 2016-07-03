#' DataFrame and methods
#'
#' This is a 'data.table' like implementation of a data.frame. dplyr is used as
#' backend. The only purpose is to have \code{R CMD check} friendly syntax.
#'
#' @include helper.R
#' @include HelperTypes.R
#'
#' @param x (DataFrame | data.frame)
#' @param i (logical | numeric | integer | OneSidedFormula | TwoSidedFormula)
#' @param j (logical | character | TwoSidedFormula | function) character
#'   beginning with '^' are interpreted as regular expression
#' @param ... arbitrary number of args
#'    \cr in \code{[} (TwoSidedFormulas)
#'    \cr in constructor see \link[tibble]{data_frame}
#' @param by,sby (character) variable names used in \link{group_by}. Using `sby`
#'   triggers a summarise.
#' @param drop (ignored) never drops the class.
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
#' @seealso \link{mutar}
#'
#' @rdname DataFrame
#' @export
DataFrame <- function(...) {
  dat <- tibble::data_frame(...)
  addClass(dat, "DataFrame")
}

#' @name DataFrame
#' @export
#' @rdname DataFrame
setOldClass(c("DataFrame", "data.frame"))

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
"[.DataFrame" <- function(x, i, j, ..., by, sby, drop) {
  # this is basically the dispatch function. I do NOT use default values because
  # missing arguments have a meaning in the special syntax expected from [

  # Assertions:
  stopifnot(inherits(x, "data.frame"))

  # This is hands on dispatch for missing arguments:
  j <- if (missing(j)) NULL else j
  j <- if (!missing(i) && nargs() == 2) i else j
  i <- if (missing(i) || nargs() == 2) NULL else i
  by <- if (missing(by)) NULL else by
  sby <- if (missing(sby)) NULL else sby
  memClassHandler <- MemClassHandler()

  x %>%
    memClassHandler$memClass() %>%
    handleRows(dispatcher(i)) %>%
    handleCols(dispatcher(i), dispatcher(j), ..., by = by, sby = sby) %>%
    memClassHandler$wrapClass()

}

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

handleRows(x ~ data.frame, i ~ FormulaList) %m% x


################################################################################

data.frame : handleCols(x, i, j, ..., by, sby) %g% standardGeneric("handleCols")

handleCols(x ~ data.frame, i ~ NULL, j ~ NULL, ..., by ~ NULL, sby ~ NULL) %m% x

handleCols(x ~ data.frame, i ~ NULL, j ~ character, ..., by ~ NULL, sby ~ NULL) %m% {
  dplyr::select_(x, .dots = j)
}

handleCols(x ~ data.frame, i ~ NULL, j ~ RegEx, ..., by ~ NULL, sby ~ NULL) %m% {
  dplyr::select(x, dplyr::matches(j))
}

handleCols(x ~ data.frame, i ~ NULL, j ~ logical, ..., by ~ NULL, sby ~ NULL) %m% {
  `[.data.frame`(x, j)
}

handleCols(x ~ data.frame, i ~ NULL, j ~ "function", ..., by ~ NULL, sby ~ NULL) %m% {
  mutar(x, vapply(x, j, logical(1)))
}

handleCols(x ~ data.frame, i ~ NULL, j ~ OneSidedFormula, ..., by ~ NULL, sby ~ NULL) %m% {
  handleRows(x, j)
}

handleCols(x ~ data.frame,
           i ~ NULL | FormulaList, j ~ NULL | FormulaList, ...,
           by ~ ANY, sby ~ ANY) %m% {
             do.call(
               handleCols,
               c(list(x = x, i = NULL, by = by, sby = sby), i, j, list(...))
             )
           }

handleCols(x ~ data.frame,
           i ~ logical | numeric | integer | OneSidedFormula,
           j ~ ANY, ..., by ~ ANY, sby ~ ANY) %m% {
             handleCols(x, NULL, j, ..., by = by, sby = sby)
           }

handleCols(x ~ data.frame,
           i ~ TwoSidedFormula | NULL,
           j ~ TwoSidedFormula | NULL,
           ..., by ~ NULL, sby ~ NULL) %m% {
             args <- constructArgs(i, j, ...)
             dplyr::mutate_(x, .dots = args)
           }

handleCols(x ~ data.frame,
           i ~ TwoSidedFormula | NULL,
           j ~ TwoSidedFormula | NULL,
           ..., by ~ NULL, sby ~ character) %m% {
             args <- constructArgs(i, j, ...)
             dplyr::group_by_(x, .dots = sby) %>%
               dplyr::summarise_(.dots = args) %>%
               as.data.frame
           }

handleCols(x ~ data.frame,
           i ~ TwoSidedFormula | NULL,
           j ~ TwoSidedFormula | NULL,
           ..., by ~ character, sby ~ NULL) %m% {
             args <- constructArgs(i, j, ...)
             dplyr::group_by_(x, .dots = by) %>%
               dplyr::mutate_(.dots = args) %>%
               as.data.frame
           }
