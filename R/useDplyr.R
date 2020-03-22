useDplyr <- function() {
  if (useDplyr <- getOption("dat.use.dplyr", FALSE)) {
    if (!requireNamespace("dplyr", quietly = TRUE)) {
      stop("option('dat.use.dplyr') is set to TRUE but the package can",
           "not be loaded. You have to install dplyr for this to work.)")
    }
  }
  useDplyr
}
