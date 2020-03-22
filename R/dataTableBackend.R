dataTableArgs <- function(args, fun = "':='") {
  paste0(fun, "(", paste(collapse = ", ", paste(
    names(args),
    unlist(lapply(args, function(x) as.character(x)[2])),
    sep = " = "
  )), ")")
}

dataTableMutate <- function(x, args) {
  arg <- dataTableArgs(args)
  expr <- parse(text = paste0(".__x__[,", arg, "]"))
  env <- new.env(parent = environment(args[[1]]))
  env$.__x__ <- x
  eval(expr, envir = env)
}

dataTableMutateBy <- function(x, args, by) {
  arg <- dataTableArgs(args)
  by <- paste(by, collapse = ", ")
  expr <- parse(text = paste0(".__x__[,", arg, ", keyby = .(", by, ")]"))
  env <- new.env(parent = environment(args[[1]]))
  env$.__x__ <- x
  eval(expr, envir = env)
}

dataTableSummariseBy <- function(x, args, by) {
  arg <- dataTableArgs(args, fun = ".")
  by <- paste(by, collapse = ", ")
  expr <- parse(text = paste0(".__x__[,", arg, ", keyby = .(", by, ")]"))
  env <- new.env(parent = environment(args[[1]]))
  env$.__x__ <- x
  eval(expr, envir = env)
}
