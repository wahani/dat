# Notes:
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
