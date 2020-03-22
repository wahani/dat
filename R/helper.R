# Internal helper functions and types

list : MList() %type% .Object

ML <- function(...) new("MList", list(...))

MList <- function(...) new("MList", list(...))


MemClassHandler <- function(x) {
  # An instance can be used to memorize the class of an object. And then to add
  # that class, or wrap that S4 instance to an object.

  classOfX <- NULL
  s4Object <- NULL

  memClass <- function(x) {
    if (isS4(x)) {
      xS3 <- S3Part(x, TRUE)
      .self$classOfX <- class(xS3)

      S3Part(x, needClass = "data.frame") <- data.frame()
      .self$s4Object <- x

      xS3
    }
    else {
      .self$classOfX <- class(x)
      x
    }
  }

  wrapClass <- function(x) {
    # dplyr and data.table are sometimes eager to add their own classes. To
    # avoid conflicts, especially with S4, they are removed first:
    x <- if (inherits(x, "data.frame")) as.data.frame(x) else x
    if (!is.null(s4Object)) {
      out <- s4Object
      S3Part(out) <- class(x, classOfX)
      out
    }
    else if (!is.null(classOfX)) {
      class(x, classOfX)
    }
    else {
      stop("Don't know what this is.")
    }
  }

  stripClass <- function(x) {
    # Check if DataFrame is in class(x), if so we need to remove it: otherwise
    # we will have a recursive call from dplyr::slice due to calling `[` on the
    # x.
    if (useDplyr())
      class(x) <- class(x)[class(x) != "DataFrame"]
    else
      x <- data.table::as.data.table(x)
    x
  }

  retList("MemClassHandler")
}

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

class <- function(x, class) {
  if (missing(class)) {
    base::class(x)
  } else {
    class(x) <- class
    x
  }
}

constructArgs <- function(i, j, ..., dat) {
  # constructs arguments (name-value expressions) for the use in mutate and
  # summarise.
  formulas <- c(list(i, j, lapply(list(...), asFormula)), recursive = TRUE)
  args <- c(lapply(formulas, resolveFormula, dat = dat), recursive = TRUE)
  args <- args[sapply(args, Negate(is.null))]
  argNames <- sapply(args, function(x) deparse(x[[2]]))
  args <- lapply(args, function(x) {
    x[2] <- NULL
    S3Part(x, strictS3 = TRUE)
  })
  names(args) <- argNames
  args
}

# dispatcher is used in "[.DataFrame" to link attributes to internal classes:
dispatcher(x) %g% x

dispatcher(x ~ character) %m% {
  if (length(x) == 1 && grepl("^\\^", x)) RegEx(x)
  else x
}

dispatcher(x ~ formula) %m% {
  asFormula(x)
}

asFormula <- function(x) {

  tmp <- Formula(x)
  
  if (all(length(tmp) == c(0, 1))) OneSidedFormula(x)
  else if (length(tmp)[2] == 1) TwoSidedFormula(x)
  else AugmentedTwoSidedFormula(x)
  
}

# This type is used for dispatch
character : RegEx() %type% {
  stopifnot(length(.Object) == 1)
  .Object
}

formula : OneSidedFormula() %type% {
  stopifnot(length(.Object) == 2)
  .Object
}

formula : TwoSidedFormula() %type% {
  stopifnot(length(.Object) == 3)
  .Object
}

TwoSidedFormula : AugmentedTwoSidedFormula(.n ~ ANY) %type% {
  tmp <- Formula(.Object)
  S3Part(.Object) <- formula(tmp, lhs = 1, rhs = 1)
  .nUnevaluated <- formula(tmp, lhs = 0, rhs = 2)[[2]]
  .Object@.n <- eval(.nUnevaluated, envir = environment(.Object))
  .Object
}

AugmentedTwoSidedFormula <- function(f, .n = NULL) {
  new("AugmentedTwoSidedFormula", .n = .n, f)
}

resolveFormula(x, ...) %g% x

resolveFormula(x ~ AugmentedTwoSidedFormula, dat, ...) %m% {
  update(FL(S3Part(x, TRUE), .n = x@.n), data = dat)
} 

