library("dat")

if (requireNamespace("testthat", quietly = TRUE)) {
  files <- list.files("testthat", full.names = TRUE)
  for (fname in files) source(files)
}
