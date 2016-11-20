#' Verbose apply function
#'
#' This apply function has a progress bar and enables computations in
#' parallel. By default it is not verbose. As an interactive version with proper
#' 'verbose' output by default please use \link{vmap}.
#'
#' @param x (vector)
#' @param f (function)
#' @param ... arguments passed to \code{.mapper} and hence \code{f}
#' @param .mc (integer) the number of processes to start
#' @param .mapper (function) the actual apply function used. Should have an
#'   argument \code{mc.cores}.
#' @param .bar (character) one in 'none', '.' or 'bar'
#'
#' @export
#' 
#' @examples
#' \dontrun{
#' verboseApply(
#'   1:4,
#'   function(...) Sys.sleep(1),
#'   .bar = "bar",
#'   .mc = 2 
#' )
#' }
verboseApply <- function(x, f, ..., .mc = 1, .mapper = mclapply, .bar = "none") {
  stopifnot(is.element(.bar, c("none", ".", "bar")))
  stopifnot(is.numeric(.mc) & length(.mc) == 1)
  
  initStatusBar <- function(n, step) {
    pb <- progress::progress_bar$new(total = n)
    function() {
      pb$tick(step)
    }
  }

  bar <- if (.bar == "none") function() NULL
    else if (.bar == ".") function() cat(".")
    else if (.bar == "bar") initStatusBar(length(x), .mc)

  fun <- function(...) {
    res <- f(...)
    bar()
    res
  }

  as.function(.mapper)(x, fun, ..., mc.cores = .mc)
    
}
