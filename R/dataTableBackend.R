dataTableMutate <- function(x, args) {
  arg <- paste0("':='(", paste(collapse = ", ", paste(
    names(args),
    unlist(lapply(args, function(x) as.character(x)[2])),
    sep = " = "
  )), ")")
  expr <- parse(text = paste0("x[,", arg, "]"))
  eval(expr)
}
