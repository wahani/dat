#' Coerce a formula into a function
#'
#' Convert a formula into a function. See \link{map} and \link{extract} for
#' examples.
#'
#' @param x (formula) see examples
#' @param ... not used
#'
#' @return An object inheriting from class function.
#'
#' @export
#' @examples
#' as.function(~ .)(1)
#' as.function(x ~ x)(1)
#' as.function(f(x, y) ~ c(x, y))(1, 2)
#' as.function(numeric : x ~ x)(1) # check for class
#' as.function(numeric(1) : x ~ x)(1) # check for class + length
as.function.formula <- function(x, ...) {
  # 1: ~ . -> function(.) .
  # 2: x ~ x -> function(x) x
  # 3: f(x, y) ~ x + y -> function(x, y) x + y
  # 4: numeric : x ~ x -> checkType(function(x) x)
  # 5: numeric(1) : x ~ x -> funWithPrototype(function(x) x)

  if (length(x) == 2) {
    args <- deparseLhs(x)
    type <- NULL
  }
  else if (length(x) == 3) {
    args <- getArgs(x)
    type <- getType(x)
  }
  else stop ("Don't know what to do with this formula.")

  addReturnType(
    aoos:::makeFunDef(args, deparseRhs(x), environment(x)),
    returnTypeDispatcher(type)
  )
}

deparseLhs <- function(x) {
  if (length(x) == 2) "." else deparse(x[[2]])
}

deparseRhs <- function(x) {
  if (length(x) == 2) deparse(x[[2]]) else deparse(x[[3]])
}

getArgs <- function(x) {
  lhs <- deparseLhs(x)
  lhs <- rev(aoos:::splitTrim(lhs, ":"))[1]
  if (grepl("\\(", lhs)) {
    lhs <- aoos:::deleteBeforeParan(lhs)
    lhs <- aoos:::deleteEnclosingParan(lhs)
    aoos:::splitTrim(lhs, ",")
  } else {
    lhs
  }
}

getType <- function(x) {
  type <- deparseLhs(x)
  type <- aoos:::splitTrim(type, ":")
  if (length(type) == 1) NULL else type[1]
}

returnTypeDispatcher(x) %g% x

returnTypeDispatcher(x ~ character) %m% {
  if (length(x) == 1 && grepl("\\(", x)) ReturnPrototype(x)
  else ReturnType(x)
}
