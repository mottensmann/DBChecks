#' next neighbour
#'
#' @export
#' @param df data frame
#' @param year integer
#' @param terr.id Terr_ID
#' @param threshold threshold im km
#' @import magrittr
#'
nearest.neighbour <- function(df = NULL, year = NULL, terr.id = NULL, threshold = 1) {

  # if(!is.numeric(terr.id))
  #cat("terr.id=", terr.id)

  if (!is.null(year)) {
    ## filter based on year
    df <- dplyr::filter(df, Year == year)[,c("Terr_ID", "Mean.Y", "Mean.X")]
  } else {
    df <- df[,c("Terr_ID", "Mean.Y", "Mean.X")] %>%
      unique.data.frame()
  }

  ## pairiwse distances
  distances <- lapply(1:nrow(df), function(row) {
    dist <- geosphere::distGeo(
      p1 = df[which(df$Terr_ID == terr.id),c("Mean.X","Mean.Y")],
      p2 = df[row,c("Mean.X", "Mean.Y")])/1000
    data.frame(Terr_ID = df$Terr_ID[row],
               dist = dist)
  }) %>%
    do.call("rbind",.) %>%
    dplyr::filter(., dist <= 1)

return(distances$Terr_ID %>% unique)
}


# rm(list = ls())
 load("../../01-PhD/00-Raw/RData/buzzard_db.RData")
# df = buzzard_db$repro_fledge_db
# year = 2019
# terr.id <- 383
# library(magrittr)
