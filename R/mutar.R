#' Tools for Data Frames
#'
#' \code{mutar} is literally the same function as \code{[.DataFrame} and can be
#' used as a generic interface to dplyr. Other functions here listed are a
#' convenience to mimic dplyr's syntax in a \code{R CMD check} friendly way.
#' These functions can also be used with S4 data.frame(s) / data_frame(s) /
#' data.table(s). They will always preserve the input class.
#'
#' @inheritParams [.DataFrame
#'
#' @details
#' The real workhorse of this interface is \code{mutar}. All other functions
#' exist to ease the transition from dplyr.
#'
#' @rdname mutar
#' @export
#'
#' @examples
#' data("airquality")
#' airquality %>%
#'   filtar(~Month > 4) %>%
#'   mutar(meanWind ~ mean(Wind), by = "Month") %>%
#'   sumar(meanWind ~ mean(Wind), by = "Month") %>%
#'   extract("meanWind")
mutar <- `[.DataFrame`

#' @rdname mutar
#' @export
filtar <- function(x, i) {
  mutar(x, i = i)
}

#' @rdname mutar
#' @export
sumar <- function(x, j, ..., by) {
  mutar(x, , j = j, ..., sby = by)
}
