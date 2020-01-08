#' Dist2Nest
#'
#' @param resights resights
#' @param ring_db ring_db
#' @param repro_fledge_db repro_fledge_db (only for Buteo buteo)
#' @param species default Buteo buteo
#' @return distance to nest in Km
#' @import magrittr
#' @export
#'
dist2nest <- function(ring_db = NULL, resights = NULL, repro_fledge_db = NULL, species = c("Buteo, buteo", "Milvus milvus")) {

  ## currently only working for buteo buteo
  #species <- match.arg(species)

  #if (species == "Buteo buteo") {
    ring_db <- ring_db[,c("Ring", "Brood_ID")] %>%
      unique.data.frame() %>%
      na.omit()

    ## if not unique, remove both cases ;-(
    case <- ring_db$Ring[(duplicated(ring_db$Ring))]
    if (length(case) > 0) ring_db <- dplyr::filter(ring_db, Ring != case)

    ## add nests to resights
    x <- dplyr::left_join(resights, ring_db, by = "Ring") %>%
      dplyr::left_join(., repro_fledge_db[, c("Brood_ID", "N", "E")],
                       by = "Brood_ID")

  #} else if (species == "Milvus milvus") {

  #}

  out <- geosphere::distGeo(p1 = x[,c("E","N")],
                            p2 = x[,c("long", "lat")])/1000
  return(out)

}
