#' Add nest ID where missing to ringing list
#' @param ring rining list
#' @param repro repro & fledge data
#' @export
gpx.add <- function(ring = ring_db, repro = repro_fledge_db) {
  
  ## 1. Missing Nests
  tempDf <- dplyr::filter(ring, is.na(Nest))[,c("no", "Brood_ID")] 
  tempDf <- dplyr::left_join(tempDf, repro[,c("Brood_ID", "Nest")], by = "Brood_ID")
  
  ## 2. paste back to dataset
  ring[tempDf[["no"]], "Nest"] <- as.character(tempDf[["Nest"]])
  return(ring)
}