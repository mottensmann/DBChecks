#' remove duplicated entries and discard unknown individuals
#'
#' @param resights data frame
#' @param threshold difference in distance to nest
#' @return data frame
#' @export
#'
remove.dupl.resight <- function(resights = NULL, threshold = 1) {
  ## discard unknown rings
  resights_known <- dplyr::filter(resights, !is.na(Ring))
  ## lapply over each unique ring
  out <- lapply(resights_known$Ring %>% unique, function(ind) {
    ## subset data
    resights_ind <- dplyr::filter(resights_known, Ring == ind)
    ## check if there are duplicates
    ## Same date and same coordincates (resolution 1km)
    if (any(duplicated(resights_ind$Date))) {
      out <- lapply(resights_ind$Date %>% unique, function(date) {
        resights_date <- dplyr::filter(resights_ind, Date == date)
        if (nrow(resights_date) == 2) {
          if (diff(resights_date$Dist2Nest) <= threshold) {
            obs <- unique(resights_date$Observer) %>%
              paste0(., collapse = ";")
            resights_date$Observer[1] <- obs
            return(resights_date[1,])
          } else {
            return(resights_date)
          }
        } else {
          return(resights_date)
        }
      }) %>%
        do.call("rbind",.)# end inner lapply
    } else {
      return(resights_ind)
    }
  }) %>%
    do.call("rbind",.)# end outer lapply
  return(out)
}# end function
