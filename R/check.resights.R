#' Check for duplicated resights
#'
#' @param buzzard_db path
#' @import magrittr
#' @export
#'
check.resights <- function(buzzard_db = "RData/buzzard_db.RData") {
  load(buzzard_db)

  ## get resights
  data <- buzzard_db[["resights"]]
  data[["check.sum"]] <- paste0(data[,"Ring"], "*", data[,"Date"])

  ## check duplicates
  dupl <- data[["check.sum"]][duplicated(data[["check.sum"]])]

  x <- dplyr::filter(data, check.sum %in% dupl)

  unique.tag <- unique(x[["Ring"]])

  check <- lapply(unique.tag, function(xy) {
    test <- dplyr::filter(x, Ring == xy)
    ## check if different oberservers and location
    if (nrow(test) > 1) {
      if (any(duplicated(test[["Observer"]]))) {
        return(test)
      } else {
        return(data.frame())
      }
    }
  }) %>%
    do.call("rbind",.)
  return(check)
}

# buzzard_db <- "../../01-PhD/00-Raw/RData/buzzard_db.RData"
