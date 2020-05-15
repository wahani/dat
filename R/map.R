#' An implementation of map
#'
#' An implementation of map and flatmap. They support the use of formulas as
#' syntactic sugar for anonymous functions.
#'
#' @param x (\link{vector} | \link{data.frame} | formula) if x inherits from
#'   data.frame, a data.frame is returned. Use \link{as.list} if this is not
#'   what you want. When x is a formula it is interpreted to trigger a
#'   multivariate map.
#' @param f (\link{function} | \link{formula} | character | logical | numeric)
#'   something which can be interpreted as a function. formula objects are
#'   coerced to a function. atomics are used for subsetting in each element of
#'   x. See the examples.
#' @param p (function | formula) a predicate function indicating which columns
#'   in a data.frame to use in map. This is a filter for the map operation, the
#'   full data.frame is returned.
#' @param simplify see SIMPLIFY in \link{mapply}
#' @param by (e.g. character) argument is passed to \link{extract} to select
#'   columns.
#' @param combine (function | formula) a function which knows how to combine the
#'   list of results. \link{bindRows} is the default.
#' @param flatten (function | formula) a function used to flatten the results.
#' @param .mc (integer) the number of cores. Passed down to \link{mclapply} or
#'   \link{mcmapply}.
#' @param .bar (character) see \link{verboseApply}.
#'
#' @param ... further arguments passed to the apply function.
#'
#' @details
#' \code{map} will dispatch to \link{lapply}. When \code{x} is a
#' formula this is interpreted as a multivariate map; this is implemented
#' using \code{mapply}.  When \code{x} is a data.frame \code{map} will iterate
#' over columns, however the return value is a \code{data.frame}. \code{p} can
#' be used to map over a subset of \code{x}.
#'
#' \code{flatmap} will dispatch to \code{map}. The result is then wrapped by
#' \code{flatten} which is \link{unlist} by default.
#'
#' \code{sac} is a naive implementation of split-apply-combine and implemented
#' using \code{flatmap}.
#'
#' \code{vmap} is a 'verbose' version of \code{map} and provides a progress bar
#' and a link to parallel map (\link{mclapply}).
#'
#' \code{map}, \code{flatmap}, and \code{sac} can be extended; they are S4
#' generic functions. You don't and should not implement a new method for
#' formulas. This method will coerce a formula into a function and pass it down
#' to your map(newtype, function) method.
#'
#' @export
#' @rdname map
#'
#' @examples
#' # Sugar for anonymous functions
#' map(data.frame(y = 1:10, z = 2), x ~ x + 1)
#' map(data.frame(y = 1:10, z = 2), x ~ x + 1, is.numeric)
#' map(data.frame(y = 1:10, z = 2), x ~ x + 1, x ~ all(x == 2))
#' sac(data.frame(y = 1:10, z = 1:2), df ~ data.frame(my = mean(df$y)), "z")
#'
#' # Trigger a multivariate map with a formula
#' map(1:2 ~ 3:4, f(x, y) ~ x + y)
#' map(1:2 ~ 3:4, f(x, y) ~ x + y, simplify = TRUE)
#' map(1:2 ~ 3:4, f(x, y, z) ~ x + y + z, z = 1)
#'
#' # Extracting values from lists
#' map(list(1:2, 3:4), 2)
#' map(list(1:3, 2:5), 2:3)
#' map(list(1:3, 2:5), c(TRUE, FALSE, TRUE))
#'
#' # Some type checking along the way
#' map(as.numeric(1:2), numeric : x ~ x)
#' map(1:2, integer(1) : x ~ x)
#' map(1:2, numeric(1) : x ~ x + 0.5)
setGeneric("map", function(x, f, ...) {
  lapply(x, f, ...)
})

#' @export
#' @rdname map
setMethod("map", c("ANY", "formula"), function(x, f, ...) {
  map(x, as.function(f), ...)
})

#' @export
#' @rdname map
setMethod("map", c("atomic","function"), function(x, f, ...) {
  map(as.list(x), f, ...)
})

#' @export
#' @rdname map
setMethod("map", c("list", "function"), function(x, f, p = function(x) TRUE, ...) {
  mapList(x, f, p, ...)
})

# This generic exists to dipatch on p
mapList(x, f, p, ...) %g% {
  standardGeneric("mapList")
}

mapList(x ~ list, f ~ "function", p ~ formula, ...) %m% {
  mapList(x, f, as.function(p), ...)
}

mapList(x ~ list, f ~ "function", p ~ "function", ...) %m% {
  ind <- vapply(x, p, logical(1))
  mapListOnIndex(x, f, ind, ...)
}

mapList(x ~ list, f ~ "function", p ~ character, ...) %m% {
  ind <- if (length(p) == 1 && grepl("^\\^", p)) {
    names(x)[grepl(p, names(x))]
  } else p
  mapListOnIndex(x, f, ind, ...)
}

mapListOnIndex <- function(x, f, ind, ...) {
  memClassHandler <- MemClassHandler()
  x <- memClassHandler$memClass(x)
  x[ind] <- verboseApply(as.list(x[ind]), f, ...)
  memClassHandler$wrapClass(x)
}

setClassUnion("numericORcharacteORlogical", c("numeric", "character", "logical"))

#' @export
#' @rdname map
setMethod("map", c("list", "numericORcharacteORlogical"), function(x, f, ...) {
  force(f)
  map(x, . ~ .[f], ...)
})

#' @export
#' @rdname map
setMethod("map", c("MList", "function"), function(x, f, ..., simplify = FALSE) {

  localmc <- function(x, f, ...) {
    do.call(mcmapply, c(list(FUN = f), x, SIMPLIFY = simplify, ...))
  }

  verboseApply(x, f, ..., .mapper = localmc)

})

#' @export
#' @rdname map
setMethod("map", c("formula", "function"), function(x, f, ...) {
  mlist <- eval(
    parse(text = paste0("list(", gsub("~", ",", deparse(x)), ")")),
    envir = environment(x)
  )
  map(do.call(MList, mlist), f, ...)
})

#' @export
#' @rdname map
setGeneric("flatmap", function(x, f, ..., flatten = unlist) {
  as.function(flatten)(map(x, f, ...))
})

#' @export
#' @rdname map
setMethod("flatmap", c("ANY", "formula"), function(x, f, ..., flatten) {
  flatmap(x, as.function(f), ..., flatten = flatten)
})

#' @export
#' @rdname map
setGeneric("sac", function(x, f, by, ..., combine = bindRows) standardGeneric("sac"))

#' @export
#' @rdname map
setMethod("sac", c("data.frame", "function"), function(x, f, by, ..., combine) {
  indList <- split(seq_len(nrow(x)), extract(x, by))
  flatmap(indList, ind ~ f(x[ind, TRUE, drop = FALSE], ...), flatten = combine)
})

#' @export
#' @rdname map
setMethod("sac", c("ANY", "formula"), function(x, f, by, ..., combine) {
  sac(x, as.function(f), by, ..., combine = combine)
})

#' @export
#' @rdname map
vmap <- function(x, f, ..., .mc = min(length(x), detectCores()), .bar = "bar") {
  map(x, f, ..., .mc = .mc, .bar = .bar)
}
