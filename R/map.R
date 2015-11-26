
map(x, f, ...) %g% {
  standardGeneric("map")
}

map(x ~ ANY, f ~ "function", ...) %m% {
  lapply(x, f, ...)
}

map(x ~ data.frame, f ~ "function", p = function(x) TRUE, ...) %m% {

  is.formula <- function(x) inherits(x, "formula")
  stopifnot(is.formula(p) | is.function(p))

  ind <- vapply(x, as.function(p), logical(1))
  x[ind] <- lapply(x[ind], f, ...)
  x

}

map(x ~ ANY, f ~ formula, ...) %m% {
  map(x, as.function(f), ...)
}

map(x ~ ANY, f ~ FunctionWithPrototype, ...) %m% {
  vapply(X = x, FUN = f, FUN.VALUE = f@prototype, ..., USE.NAMES = TRUE)
}

map(x ~ ANY, f ~ numeric | character | logical, ...) %m% {
  force(f)
  map(x, . ~ .[f], ...)
}

list : List() %type% .Object
L <- function(...) new("List", list(...))

map(x ~ List, f ~ "function", simplify = FALSE, ...) %m% {
  do.call(mapply, c(list(FUN = f), x, SIMPLIFY = simplify, ...))
}

map(data.frame(y = 1:10, z = 2), x ~ x + 1)
map(data.frame(y = 1:10, z = 2), x ~ x + 1, is.numeric)
map(data.frame(y = 1:10, z = 2), x ~ x + 1, x ~ all(x == 2))

map(1, x ~ x)
map(list(1:2, 3:4), 2)
map(L(1:2, 3:4), f(x, y) ~ x + y)
map(L(1:2, 3:4), f(x, y) ~ x + y, simplify = TRUE)
map(L(1:2, 3:4), f(x, y, z) ~ x + y + z, z = 1)

map(list(1:3, 2:5), 2:3)
map(list(1:3, 2:5), c(TRUE, FALSE, TRUE))
