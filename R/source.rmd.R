#' Render Rmd in script
#'
#' @param file file name
#' @param ... optional arguments passed on to source
#'
#' @export
#'
source.rmd <- function(file, ...) {
  library(knitr)
  source(purl(file, output = tempfile()), ...)
  print("Done")
}
