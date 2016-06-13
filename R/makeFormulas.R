makeFormulas <- function(..., .n = ".n", pattern = "\\.n") {

  formulas <- list(...)
  .each <- length(.n)
  .n <- rep(.n, times = length(formulas))

  formulaList <- map(formulas, deparse)
  formulaList <- rep(formulaList, each = .each)
  formulaList <- map(pattern ~ .n ~ formulaList, gsub)
  formulaList <- map(formulaList, as.formula, env = environment(formulas[[1]]))

  formulaList

}

makeFormulas(.nMean ~ mean(.n), .nSd ~ sd(.n), .n = letters[1:2])
