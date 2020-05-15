useDplyr <- function() {
  if (useDplyr <- getOption("dat.use.dplyr", FALSE)) {
    if (!requireNamespace("dplyr", quietly = TRUE)) {
      stop("option('dat.use.dplyr') is set to TRUE but the package can",
           "not be loaded. Set option to FALSE and use data.table or install",
           "dplyr.)")
    }
  }
  useDplyr
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "To use dplyr as backend set 'options(dat.use.dplyr = TRUE)'."
  )
}
