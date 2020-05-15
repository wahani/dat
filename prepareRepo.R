devtools::build_vignettes()
knitr::knit("vignettes/Introduction.Rmd", "README.md")

batches <- c(
  "[![Travis-CI Build Status](https://travis-ci.org/wahani/dat.svg?branch=master)](https://travis-ci.org/wahani/dat)",
  "[![codecov.io](https://codecov.io/github/wahani/dat/coverage.svg?branch=master)](https://codecov.io/github/wahani/dat?branch=master)",
  "[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/dat)](http://cran.r-project.org/package=dat)",
  "[![Downloads](http://cranlogs.r-pkg.org/badges/dat?color=brightgreen)](http://www.r-pkg.org/pkg/dat)",
  ""
)

readme <- readLines("README.md")
readme <- c(batches, readme[-(1:10)])
writeLines(readme, "README.md")

## TODO
## - progress bar + verbose apply in rstudio + windows
##     - not working in either one myself
## - extract with a matches not
library(dat)
library(dplyr)
options(dat.use.dplyr=TRUE)
dat::mutar(data.frame(x=1:10), id ~ n())


dat::vmap(1:10, ~ Sys.sleep(.))

data.frame(x = 1:10, y = 2) %>%
  mutar(.n ~ mean(.n) | "x", sby = "y")

a <- "x"
data.frame(x = 1:10, y = 2) %>%
  mutar(n ~ mean(n) | list(n = a), sby = "y")

ll <- list(n = a)
data.frame(x = 1:10, y = 2) %>%
  mutar(n ~ mean(n) | ll, sby = "y")

extract(table(letters[1:2]), "a")
replace(table(letters[1:2]), .~.==1, 2)
