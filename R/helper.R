# Internal helper functions and types

setClassUnion(
  "atomic",
  c("logical", "integer", "numeric", "complex", "character", "raw")
)

character : ReturnPrototype() %type% {
  # wraps a protoype of the return value of a function
  stopifnot(length(.Object) == 1)
  stopifnot(grepl("\\(", .Object))
  .Object
}

character : ReturnType() %type% {
  # wraps the type of the return value of a function
  stopifnot(length(.Object) == 1)
  .Object
}

"function" : FunctionWithPrototype(fun ~ "function", prototype ~ ANY) %type% {
  S3Part(.Object) <- addTypeCheck(.Object@fun, class(.Object@prototype)) %>%
    addLengthCheck(length(.Object@prototype))
  .Object
}

"function" : FunctionWithType(fun ~ "function", type ~ ReturnType) %type% {
  S3Part(.Object) <- addTypeCheck(.Object@fun, .Object@type)
  .Object
}

addLengthCheck <- function(f, l) {
  force(f); force(l)
  function(...) {
    out <- f(...)
    if (length(out) != l) {
      stop("Function does not return correct length
           expected: ", l, "
           observed: ", length(out))
    } else {
      out
    }
  }
}

addTypeCheck <- function(f, type) {
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

addReturnType(f, type) %g% f

addReturnType(f, type ~ ReturnPrototype) %m% {
  FunctionWithPrototype(fun = f, prototype = eval(parse(text = type)))
}

addReturnType(f, type ~ ReturnType) %m% {
  FunctionWithType(fun = f, type = type)
}

addClass <- function(x, class) {
  class(x) <- unique(c(class, class(x)))
  x
}

constructArgs <- function(i, j, ...) {
  # constructs arguments (name-value expressions) for the use in mutate and
  # summarise.
  # '...' can be anything so the type is checked on the fly:
  args <- c(list(i, j), lapply(list(...), TwoSidedFormula))
  args <- args[sapply(args, Negate(is.null))]
  lapply(args, function(x) sub("~", "=", deparse(x)))
}

# dispatcher is used in "[.DataFrame" to link attributes to internal classes:
dispatcher(x) %g% x

dispatcher(x ~ character) %m% {
  if (length(x) == 1 && grepl("^\\^", x)) RegEx(x)
  else x
}

dispatcher(x ~ formula) %m% {
  if (length(x) == 2) OneSidedFormula(x)
  else TwoSidedFormula(x)
}

# This type is used for dispatch
character : RegEx() %type% {
  stopifnot(length(.Object) == 1)
  .Object
}

# I distiguish between one and two sided formulas. They are interpreted
# differnetly
formula : OneSidedFormula() %type% {
  stopifnot(length(.Object) == 2)
  .Object
}

formula : TwoSidedFormula() %type% {
  stopifnot(length(.Object) == 3)
  .Object
}
