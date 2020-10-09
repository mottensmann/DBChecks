#' Check for if breeders were not resighted
#'
#' @param buzzard_db path
#' @import magrittr
#' @export
#' @examples
#' ## not run
#' #test <- check.breeders(buzzard_db = "../../01-PhD/00-Raw/RData/buzzard_db.RData")
#'
check.breeders <- function(buzzard_db = "RData/buzzard_db.RData") {
  load(buzzard_db)

  males <- filter(buzzard_db$repro_fledge_db, stringr::str_detect(Male_ID, "R")) %>%
    mutate(Ring = stringr::str_remove(Male_ID, "R"))
  females <- filter(buzzard_db$repro_fledge_db, stringr::str_detect(Fem_ID, "R")) %>%
    mutate(Ring = stringr::str_remove(Fem_ID, "R"))

  data <- rbind(males, females)

  lapply(1:nrow(data), function(x) {
    df <- data[x, c("Territory", "Nest", "Year", "Fem_ID", "Male_ID", "Comment", "Ring")]
    res <- filter(buzzard_db$resights,
                  Ring == df$Ring,
                  Year == df$Year,
                  Month %in% c("03", "04", "05", "06", "07"))
    if (nrow(res) >= 1) {
      data.frame()
    } else {
      df$Conflict <- paste(df$Ring, "not resighted in breeding season")
      return(subset(df, select = -c(Ring)))
    }
  }) %>%
    do.call("rbind",.)
}


# library(magrittr)
# library(dplyr)
# buzzard_db <- "../../01-PhD/00-Raw/RData/buzzard_db.RData"

