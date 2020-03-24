
dataTableMutate <- function(x, args) {
  byReference <- getOption("dat.use.reference.semantics", FALSE)
  mutateDataTable(
    if (byReference) x else data.table::copy(x),
    args, "':='", NULL
  )
}

dataTableMutateBy <- function(x, args, by) {
  byReference <- getOption("dat.use.reference.semantics", FALSE)
  mutateDataTable(
    if (byReference) x else data.table::copy(x),
    args, "':='", by
  )
}

dataTableSummariseBy <- function(x, args, by) {
  callDataTable(x, args, ".", by)
}

mutateDataTable <- function(x, args, fun, by) {
  ## Prepare column names for processing:
  colsTmp <- paste0(".__", names(args), "__")
  cols <- names(args)
  names(args) <- colsTmp
  listOfNames <- as.list(cols)
  names(listOfNames) <- colsTmp
  colsInX <- names(x)[names(x) %in% cols]
  ## Compute new cols
  callDataTable(x, args, fun, by)
  ## Rename and drop old columns
  if (length(colsInX) > 0) x[, (colsInX) := NULL]
  do.call(rename, c(list(x), listOfNames))
}

callDataTable <- function(x, args, fun, by) {
  by <- if (!is.null(by)) paste0(",keyby=.(", paste(by, collapse = ","), ")")
  arg <- dataTableArgs(args, fun)
  expr <- parse(text = paste0(".__x__[,", arg, by, "]"))
  env <- new.env(parent = environment(args[[1]]))
  env$.__x__ <- x
  eval(expr, envir = env)
}

dataTableArgs <- function(args, fun = "':='") {
  paste0(fun, "(", paste(collapse = ", ", paste(
    names(args),
    unlist(lapply(args, function(x) as.character(x)[2])),
    sep = " = "
  )), ")")
}

rename <- function(x, ...) {
  args <- list(...)
  for (n in names(args)) {
    data.table::setnames(x, n, args[[n]])
  }
  x
}
