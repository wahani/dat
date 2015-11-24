#' DataFrame and methods
#'
#' There are many ways to use a DataFrame. Please see the vignette to see some
#' examples.
#'
#' @include helper.R
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

setOldClass("DataFrame")

#' @rdname DataFrame
#' @export
as.DataFrame <- function(x) UseMethod("as.DataFrame")

#' @rdname DataFrame
#' @export
as.DataFrame.default <- function(x) {
  do.call(DataFrame, as.list(x))
}

#' @param x (DataFrame | ANY)
#' @param i (logical | numeric | integer | OneSidedFormula | TwoSidedFormula)
#' @param j (logical | character | TwoSidedFormula | function) character which
#'   begin with '__' or '^' are interpreted as regular expression
#' @param ... arbitrary number of args
#'    \cr in \code{[} (TwoSidedFormulas)
#'    \cr in constructor see \link[dplyr]{data_frame}
#' @param by (character) variable name
#' @param drop ignored
#'
#' @rdname DataFrame
#' @export
"[.DataFrame" <- function(x, i, j, ..., by, drop) {
  # this is basically the dispatch function. I do NOT use default values because
  # missing arguments have a meaning in the special syntax expected from [

  j <- if (missing(j)) NULL else j
  j <- if (!missing(i) && nargs() == 2) i else j
  i <- if (missing(i) || nargs() == 2) NULL else i
  by <- if (missing(by)) NULL else by

  x %>%
    handleRows(dispatcher(i)) %>%
    handleCols(dispatcher(i), dispatcher(j), ..., by = dispatcher(by))

}

DataFrame : handleRows(x, i) %g% standardGeneric("handleRows")

handleRows(x ~ DataFrame, i ~ NULL) %m% x

handleRows(x ~ DataFrame, i ~ logical) %m% {
  .__i__ <- i
  addClass(dplyr::filter(x, .__i__), "DataFrame")
}

handleRows(x ~ DataFrame, i ~ numeric | integer) %m% {
  ".__i__" <- i
  addClass(dplyr::slice(x, .__i__), "DataFrame")
}

handleRows(x ~ DataFrame, i ~ OneSidedFormula) %m% {
  envir <- environment(i)
  expr <- parse(text = sub("~", "", deparse(i)))
  addClass(x[do.call(with, list(x, expr)), ], "DataFrame")
}

handleRows(x ~ DataFrame, i ~ TwoSidedFormula) %m% x


DataFrame : handleCols(x, i, j, ..., by) %g% standardGeneric("handleCols")

handleCols(x ~ DataFrame, i ~ NULL, j ~ NULL, ..., by ~ NULL) %m% x

handleCols(x ~ DataFrame, i ~ NULL, j ~ character, ..., by ~ NULL) %m% {
  dplyr::select_(x, .dots = j)
}

handleCols(x ~ DataFrame, i ~ NULL, j ~ RegEx, ..., by ~ NULL) %m% {
  dplyr::select(x, matches(j))
}

handleCols(x ~ DataFrame, i ~ NULL, j ~ logical, ..., by ~ NULL) %m% {
  `[.data.frame`(x, j)
}

handleCols(x ~ DataFrame, i ~ NULL, j ~ "function", ..., by ~ NULL) %m% {
  x[vapply(x, j, logical(1))]
}

handleCols(x ~ DataFrame, i ~ NULL, j ~ OneSidedFormula, ..., by ~ NULL) %m% {
  handleRows(x, j)
}

handleCols(x ~ DataFrame,
           i ~ logical | numeric | integer | OneSidedFormula,
           j ~ ANY, ..., by ~ ANY) %m% {
             handleCols(x, NULL, j, ..., by = by)
           }

handleCols(x ~ DataFrame,
           i ~ TwoSidedFormula | NULL,
           j ~ TwoSidedFormula | NULL,
           ...,
           by ~ NULL) %m% {
             args <- constructArgs(i, j, ...)
             addClass(
               class = "DataFrame",
               eval(
                 parse(text = paste0(
                   "dplyr::mutate(x,", paste0(args, collapse = ","), ")"))
               )
             )
           }

handleCols(x ~ DataFrame,
           i ~ TwoSidedFormula | NULL, j ~ TwoSidedFormula | NULL,
           ..., by ~ character) %m% {
             args <- constructArgs(i, j, ...)
             x <- dplyr::group_by_(x, .dots = by)
             addClass(class = "DataFrame", eval(parse(text = paste0(
               "dplyr::summarise(x,", paste0(args, collapse = ","), ")")))
             )
           }
