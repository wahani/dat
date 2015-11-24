#' DataFrame and methods
#'
#' @param ... dots
#'
#' @include helper.R
#'
#' @rdname DataFrame
#' @export
DataFrame <- function(...) {
  dat <- dplyr::data_frame(...)
  addClass(dat, "DataFrame")
}

setOldClass("DataFrame")

#' @param x an object
#' @rdname DataFrame
#' @export
as.DataFrame <- function(x) UseMethod("as.DataFrame")

#' @rdname DataFrame
#' @export
as.DataFrame.default <- function(x) addClass(as.data.frame(x), "DataFrame")

#' @param x (DataFrame)
#' @param i (logical | numeric | integer | OneSidedFormula)
#' @param j (logical | character | TwoSidedFormula)
#' @param ... (TwoSidedFormulas)
#' @param by (character) variable name
#' @param drop ignored
#'
#' @rdname DataFrame
#' @export
"[.DataFrame" <- function(x, i, j, ..., by, drop) {
  # this is basically the dispatch function. I do NOT use default values because
  # missing values have a meaning in the special syntax expected from [

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
             args <- list(i, j, ...)
             args <- args[sapply(args, Negate(is.null))]
             args <- lapply(args, function(x) text = sub("~", "=", deparse(x)))
             addClass(class = "DataFrame",
                      eval(
                        parse(text = paste0(
                          "dplyr::mutate(x,", paste0(args, collapse = ","), ")"))
                      )
             )
           }

handleCols(x ~ DataFrame,
           i ~ TwoSidedFormula | NULL, j ~ TwoSidedFormula | NULL,
           ..., by ~ character) %m% {
             args <- list(i, j, ...)
             args <- args[sapply(args, Negate(is.null))]
             args <- lapply(args, function(x) text = sub("~", "=", deparse(x)))
             x <- dplyr::group_by_(x, .dots = by)
             addClass(class = "DataFrame", eval(parse(text = paste0(
               "dplyr::summarise(x,", paste0(args, collapse = ","), ")")))
             )
           }
