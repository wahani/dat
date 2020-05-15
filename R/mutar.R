#' Tools for Data Frames
#'
#' \code{mutar} is literally the same function as \code{[.DataFrame} and can be
#' used as interface to dplyr or data.table. Other functions here listed are a
#' convenience to mimic dplyr's syntax in a \code{R CMD check} friendly way.
#' These functions can also be used with S4 data.frame(s) / data_frame(s) /
#' data.table(s). They will always try to preserve the input class.
#'
#' @inheritParams [.DataFrame
#'
#' @details
#' The real workhorse of this interface is \code{mutar}. All other functions
#' exist to ease the transition from dplyr.
#'
#' \code{OneSidedFormula} is always used for subsetting rows.
#'
#' \code{TwoSidedFormula} is used instead of name-value expressions. Instead of
#'   writing \code{x = 1} you simply write \code{x ~ 1}.
#'
#' \code{FormulaList} can be used to repeat the same operation on different
#'   columns. See more details in \link{FL}.
#'
#' @rdname mutar
#' @export
#'
#' @seealso \link{extract}, \link{DataFrame}, \link{FL}
#'
#' @examples
#' data("airquality")
#' airquality %>%
#'   filtar(~Month > 4) %>%
#'   mutar(meanWind ~ mean(Wind), by = "Month") %>%
#'   sumar(meanWind ~ mean(Wind), by = "Month") %>%
#'   extract("meanWind")
#'
#' airquality %>%
#'   sumar(
#'     .n ~ mean(.n) | c("Wind", "Temp"),
#'     by = "Month"
#'   )
#'
#' # Enable data.tables reference semantics with:
#' withReference({
#'   x <- data.table::data.table(x = 1)
#'   mutar(x, y ~ 2)
#' })
#'
#' \dontrun{
#' # Use dplyr as back-end:
#' options(dat.use.dplyr = TRUE)
#' x <- data.frame(x = 1)
#' mutar(x, y ~ dplyr::n())
#' }
#'
mutar <- `[.DataFrame`

#' @rdname mutar
#' @export
filtar <- function(x, i) {
  mutar(x, i = i, )
}

#' @rdname mutar
#' @export
sumar <- function(x, ..., by) {
  mutar(x, ..., sby = by)
}

#' @param expr (expression) any R expression that should be evaluated using data
#'   tables reference semantics on data transformations.
#' @rdname mutar
#' @export
withReference <- function(expr) {
  old <- options(dat.use.reference.semantics = TRUE)
  on.exit(options(old))
  expr
}
