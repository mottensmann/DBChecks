#' Render Rmd in script
#'
#' @param file file name
#' @export
#'
source.rmd <- function(file, ...) {
  library(knitr)
  source(purl(file, output = tempfile()), ...)
  print("Done")
}
