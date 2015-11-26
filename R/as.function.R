setGeneric("as.function")

as.function(x ~ formula, ...) %m% {
  # 1: ~ . -> function(.) .
  # 2: x ~ x -> function(x) x
  # 3: f(x, y) ~ x + y -> function(x, y) x + y
  # 4: numeric : x ~ x -> checkType(function(x) x)
  # 5: numeric(1) : x ~ x -> funWithPrototype(function(x) x)

  deparseLhs <- function(x) {
    if (length(x) == 2) "." else deparse(x[[2]])
  }

  deparseRhs <- function(x) {
    if (length(x) == 2) deparse(x[[2]]) else deparse(x[[3]])
  }

  getArgs <- function(x) {
    lhs <- deparseLhs(x)
    if (grepl("\\(", lhs)) {
      lhs <- rev(aoos:::splitTrim(lhs, ":"))[1]
      lhs <- aoos:::deleteBeforeParan(lhs)
      lhs <- aoos:::deleteEnclosingParan(lhs)
      aoos:::splitTrim(lhs, ",")
    } else {
      rev(aoos:::splitTrim(lhs, ":"))[1]
    }
  }

  getType <- function(x) {
    type <- deparseLhs(x)
    type <- aoos:::splitTrim(type, ":")
    if (length(type) == 1) NULL else type[1]
  }

  dispatcher(x) %g% x

  dispatcher(x ~ character) %m% {
    if (length(x) == 1 && grepl("\\(", x)) ReturnPrototype(x)
    else ReturnType(x)
  }

  if (length(x) == 2) {
    args <- deparseLhs(x)
    type <- NULL
  }
  else if (length(x) == 3) {
    args <- getArgs(x)
    type <- getType(x)
  }
  else stop ("Don't know what to do with this formula.")

  checkReturnType(
    aoos:::makeFunDef(args, deparseRhs(x), environment(x)),
    dispatcher(type)
  )
}

character : ReturnPrototype() %type% {
  stopifnot(length(.Object) == 1)
  stopifnot(grepl("\\(", .Object))
  .Object
}

character : ReturnType() %type% {
  stopifnot(length(.Object) == 1)
  .Object
}

"function" : FunctionWithPrototype(prototype ~ ANY) %type% .Object

"function" : FunctionWithType(type ~ ReturnType) %type% .Object

checkReturnType(f, type) %g% f

checkReturnType(f, type ~ ReturnPrototype) %m% {
  FunctionWithPrototype(f, prototype = eval(parse(text = type)))
}

checkReturnType(f, type ~ ReturnType) %m% {
  force(f); force(type)
  function(...) {
    out <- f(...)
    if (!inherits(out, type))
      stop("Function does not return correct type
           expected: ", type, "
           observed: ", class(out))
    else out
  }
}

as.function(~.[[1]])(list(1))
as.function(x ~ x[[1]])(list(1))
# as.function(numeric : x ~ x[[1]])(list(""))
as.function(f(x) ~ x[1])(list(1))
as.function(f(x, y) ~ x + y)(1, 2)
as.function(f(x, y) ~ x + y)(1, 2)
as.function(numeric : f(x, y) ~ x + y)(1, 2)
as.function(numeric(0) : f(x) ~ x)(2)
