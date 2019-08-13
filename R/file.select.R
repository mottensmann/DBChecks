#' Select most recent version of a file
#'
#' @param path folder to look up files
#' @param pattern pattern
#' @param type file extension (e.g. ".csv")
#' @param decreasing Boolean. If TRUE newest file is selected
#' @param not_in_pattern optional. Pattern which is not allowed
#' @export
#' 
file.select <- function(path = NULL, pattern = NULL, type = NULL, decreasing = T, not_in_pattern = NULL) {
  matching.files <- list.files(path = path, pattern = pattern, full.names = T)
  if (!is.null(type)) {
    matching.files <- matching.files[grepl(pattern = type, x = matching.files)]
  }
  if (!is.null(not_in_pattern)) {
    test.case <- matching.files[grepl(pattern = not_in_pattern, x = matching.files)]
    matching.files <-  matching.files[-which(matching.files %in% test.case)]
  }
  mtime <- lapply(matching.files, function(x) file.info(x)[,"mtime"])
  mtime <- do.call("c", mtime)
  matching.files[order(mtime, decreasing = decreasing)[1]]  
}

