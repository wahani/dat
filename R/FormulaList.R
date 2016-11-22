#' Dynamically generate formulas
#'
#' Function to dynamically generate formulas - (F)ormula (L)ist - to be used in
#' \link{mutar}.
#'
#' @param ... (formulas)
#' @param .n names to be used in formulas. Can be any object which
#'	 can be used by \link{extract} to select columns. NULL is
#'	 interpreted to use the formulas without change.
#' @param pattern (character) pattern to be replaced in formulas
#' @param object (FormulaList)
#' @param data (data.frame)
#'
#' @seealso \link{mutar}
#'
#' @include NAMESPACE.R
#' @rdname FormulaList
#' @export
#' @examples
#' FL(.n ~ mean(.n), .n = "variable")
#' as(makeFormulas(.n ~ mean(.n), .n = "variable"), "FormulaList")
FL <- function(..., .n = NULL, pattern = "\\.n") {
  new("FormulaList", list(...), .n = .n, pattern = pattern)
}

list : FormulaList(.n ~ ANY, pattern ~ character) %type% .Object

setAs("list", "FormulaList", function(from) {
  new("FormulaList", from, .n = NULL, pattern = "\\.n")
})

##' @export
##' @rdname FormulaList
makeFormulas <- function(..., .n, pattern = "\\.n") {

  formulas <- list(...)
  map(formulas, f ~ stopifnot(is(f, "formula")))
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

update.NULL <- function(object, ...) NULL

##' @export
##' @rdname FormulaList
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
