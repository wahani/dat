WITH_DPLYR <- function(expr) {
  old <- options(dat.use.dplyr = TRUE)
  on.exit(options(old))
  expr
}

WITHOUT_DPLYR <- function(expr) {
  old <- options(dat.use.dplyr = FALSE)
  on.exit(options(old))
  expr
}
