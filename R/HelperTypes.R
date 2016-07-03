list : MList() %type% .Object

ML <- function(...) new("MList", list(...))

MList <- function(...) new("MList", list(...))


##' Dynamically generate formulas
##'
##' Function to dynamically generate formulas to be used in \link{mutar}.
##'
##' @param ... (formulas)
##' @param .n (character) names to be used in formulas
##' @param pattern (character) pattern to be replaced in formulas
##'
##' @export
##' @examples
##'
##' FL(.n ~ mean(.n), .n = "variable")
FL <- function(..., .n = ".n", pattern = "\\.n") {
  new("FormulaList", makeFormulas(..., .n = .n, pattern = pattern), .n = .n)
}

list : FormulaList(.n ~ character) %type% .Object

makeFormulas <- function(..., .n, pattern) {

  formulas <- list(...)
  .each <- length(.n)
  .n <- rep(.n, times = length(formulas))

  formulaList <- map(formulas, deparse)
  formulaList <- rep(formulaList, each = .each)
  formulaList <- map(pattern ~ .n ~ formulaList, gsub)
  formulaList <- map(formulaList, as.formula, env = environment(formulas[[1]]))
  formulaList <- map(formulaList, TwoSidedFormula)

  names(formulaList) <- NULL

  formulaList

}
