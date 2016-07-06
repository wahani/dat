list : MList() %type% .Object

ML <- function(...) new("MList", list(...))

MList <- function(...) new("MList", list(...))


##' Dynamically generate formulas
##'
##' Function to dynamically generate formulas - (F)ormula (L)ist  to be used in
##' \link{mutar}.
##'
##' @param ... (formulas)
##' @param .n (character) names to be used in formulas
##' @param pattern (character) pattern to be replaced in formulas
##' @param lazy (logical) flag to indicate if \code{.n} should be interpreted
##'   within the data, i.e. in the future. In this cas \code{.n} is an object
##'   which can be used in \link{extract}.
##'
##' @seealso \link{mutar}
##'
##' @rdname FormulaList
##' @export
##' @examples
##' FL(.n ~ mean(.n), .n = "variable")
FL <- function(..., .n = ".n", pattern = "\\.n", lazy = isLazyFormula(.n)) {
  if (lazy) {
    new("FormulaList", list(...), .n = .n, pattern = pattern)
  } else {
    new("FormulaList", makeFormulas(..., .n = .n, pattern = pattern))
  }
}

list : FormulaList(.n ~ ANY, pattern ~ character) %type% .Object

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

isLazyFormula(.n) %g% TRUE

isLazyFormula(.n ~ character) %m% {
  if (length(.n) == 1 && grepl("^\\^", .n)) TRUE else FALSE
}

update.NULL <- function(object, ...) NULL

update.FormulaList <- function(object, data, ...) {
  
  if (is.null(object@.n)) {
    object
  } else {    
    .n <- extractNames(data, object@.n)
    new(
      "FormulaList",
      do.call(makeFormulas, c(object, list(.n = .n, pattern = object@pattern)))
    )

  }
  
}

extractNames <- function(x, ind, ...) names(extract(x, ind))
