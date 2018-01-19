deparse <- function(x) {
  # This function was necessary in response to:
  # https://bugs.r-project.org/bugzilla/show_bug.cgi?id=17378
  # which led to an incorrect behavior when x is an S4 object containing
  # a formula. This function will override the default behaviour of base to
  # conform with the idea of of how deparse should work within 'dat'
  base::deparse(asS3(x))
}
