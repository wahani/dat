# Notes:
#
# - Need to import data.table so dplyr functions can handle that format. If not
# I get obscure error messages from dplyr functions.

#' @importFrom Formula Formula
#' @importFrom magrittr %>% %<>%
#' @importFrom stats as.formula formula update
#' @importFrom parallel mclapply mcmapply detectCores
#' @import methods
#' @import aoos
#' @import data.table
#' @import tibble
NULL

setClassUnion(
  "atomic",
  c("logical", "integer", "numeric", "complex", "character", "raw"),
  topenv(environment())
)

# This package uses S4. If we use the package without attaching it (using `::`
# instead of `library`) some things may not work. This can only be changed by
# not using S4; which involves too much time and effort to get rid of at this
# point. Hence we have to make sure, that as soon something from this package is
# used, 'methods' is attached - simply importing it is not enough:
setLoadAction(function(ns) {
  eval(parse(text = "library('methods')"), envir = ns)
})
