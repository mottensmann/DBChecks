#' Temporary shape file name
#' @export
#' @return path
temp.shp <- function() {
  temp.file <-
    file.path(tempdir(),
              paste0(paste0(sample(c(LETTERS, letters, 1:9), 8, replace = T),
                            collapse = ""), ".shp"))
  return(temp.file)
}
