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

#' Remove temporary shape files
#' @param file file name
#' @export
#'
unlink.shp <- function(file = NULL) {
  ## make names
  to_unlink <- stringr::str_replace(string = file, pattern = ".shp",
                       replacement = c(".shp", ".dbf", ".prj", ".shx", ".qpj"))
  silent <- lapply(to_unlink, unlink)
}


