# Internal helper functions

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
  if (length(x) == 1 && grepl("^__|\\^", x)) RegEx(sub("^__", "", x))
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
