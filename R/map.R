#' An implementation of map
#'
#' An implementation of map and flatmap. It supports the use of formulas as
#' syntactic sugar for anonymous functions. Also there is special awareness of
#' data.frames.
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
#' @param recursive see recursive in \link{unlist}
#' @param useNames see use.names in \link{unlist}
#' @param simplify see SIMPLIFY in \link{mapply}
#' @param by (e.g. character) argument is passed to \link{mutar} to select
#'   columns.
#' @param combine (function | formula) a function which knows how to combine
#'   the list of results. \link{bindRows} is the default.
#'
#' @param ... further arguments passed to \link{lapply} and \link{mapply}
#'
#' @details
#' \code{map} will dispatch to \link{lapply}. When \code{x} is a formula this is
#' interpreted as a multivariate map; this is implemented using \code{mapply}.
#'
#' \code{flatmap} will dispatch to \code{map}. The result is then wrapped by
#' \link{unlist}.
#'
#' When the first argument is a \code{data.frame} the behaviour is
#' special. If this is not what you want, then use \link{as.list}. \code{map}
#' will iterate over columns, however the return value is a \code{data.frame}.
#' \code{flatmap} is an implamentation of split-apply-combine; so we iterate
#' over rows/blocks and then combine.
#'
#' \code{map} and \code{flatmap} can be extended; both are S4 generic functions.
#' You don't and should not implement a new method for formulas. This method
#' will coerce a formula into a function and pass it down to your map(newtype,
#' function) method.
#'
#' @export
#' @rdname map
#'
#' @examples
#' # Sugar for anonymous functions
#' map(data.frame(y = 1:10, z = 2), x ~ x + 1)
#' map(data.frame(y = 1:10, z = 2), x ~ x + 1, is.numeric)
#' map(data.frame(y = 1:10, z = 2), x ~ x + 1, x ~ all(x == 2))
#' map(1, x ~ x)
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
map(x, f, ...) %g% {
  standardGeneric("map")
}

#' @export
#' @rdname map
map(x ~ atomic | list, f ~ "function", ...) %m% {
  lapply(x, f, ...)
}

#' @export
#' @rdname map
map(x ~ data.frame, f ~ "function", p = function(x) TRUE, ...) %m% {
  mapDataFrame(x, f, p, ...)
}

mapDataFrame(x, f, p, ...) %g% {
  # This generic exists to dipatch on p
  standardGeneric("mapDataFrame")
}

mapDataFrame(x ~ data.frame, f ~ "function", p ~ formula, ...) %m% {
  mapDataFrame(x, f, as.function(p), ...)
}

mapDataFrame(x ~ data.frame, f ~ "function", p ~ "function", ...) %m% {
  ind <- names(x)[vapply(x, p, logical(1))]
  mapDataFrameOnIndex(x, f, ind, ...)
}

mapDataFrame(x ~ data.frame, f ~ "function", p ~ character, ...) %m% {
  ind <- if (length(p) == 1 && grepl("^\\^", p)) {
    names(x)[grepl(p, names(x))]
  } else p
  mapDataFrameOnIndex(x, f, ind, ...)
}

mapDataFrameOnIndex <- function(x, f, ind, ...) {
  memClassHandler <- MemClassHandler()
  x <- memClassHandler$memClass(x)
  x[ind] <- lapply(x[ind], f, ...)
  memClassHandler$wrapClass(x)
}

#' @export
#' @rdname map
map(x ~ ANY, f ~ formula, ...) %m% {
  map(x, as.function(f), ...)
}

#' @export
#' @rdname map
map(x ~ list, f ~ numeric | character | logical, ...) %m% {
  force(f)
  map(x, . ~ .[f], ...)
}

#' @export
#' @rdname map
map(x ~ MList, f ~ "function", ..., simplify = FALSE) %m% {
  do.call(mapply, c(list(FUN = f), x, SIMPLIFY = simplify, ...))
}

#' @export
#' @rdname map
map(x ~ formula, f ~ "function", ...) %m% {
  mlist <- eval(
    parse(text = paste0("list(", gsub("~", ",", deparse(x)), ")")),
    envir = environment(x)
  )
  map(do.call(MList, mlist), f, ...)
}

#' @export
#' @rdname map
flatmap(x, f, ..., recursive = FALSE, useNames = TRUE) %g% {
  unlist(map(x, f, ...), recursive, useNames)
}

#' @export
#' @rdname map
flatmap(x ~ ANY, f ~ formula, ..., recursive, useNames) %m% {
  flatmap(x, as.function(f), ..., recursive = recursive, useNames = useNames)
}

#' @export
#' @rdname map
flatmap(x ~ data.frame, f ~ "function", by, ..., combine = bindRows, recursive, useNames) %m% {
  indList <- split(seq_len(nrow(x)), mutar(x, j = by))
  datList <- map(indList, ind ~ f(x[ind, , drop = FALSE], ...))
  as.function(combine)(datList)
}

