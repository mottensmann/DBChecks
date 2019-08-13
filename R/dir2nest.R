#' Dir2Nest
#'
#' @param resights resights
#' @param ring_db ring_db
#' @param repro_fledge_db repro_fledge_db
#' @return distance to nest in Km
#' @import magrittr
#' @export
#'
dir2nest <- function(ring_db = NULL, resights = NULL, repro_fledge_db = NULL) {

  ring_db <- ring_db[,c("Ring", "Brood_ID")] %>%
    unique.data.frame() %>%
    na.omit()

  ## if not unique, remove both case ;-(
  case <- ring_db$Ring[(duplicated(ring_db$Ring))]
  ring_db <- dplyr::filter(ring_db, Ring != case)

  ## add nests to resights
  x <- dplyr::left_join(resights, ring_db, by = "Ring") %>%
    dplyr::left_join(., repro_fledge_db[, c("Brood_ID", "N", "E")],
                     by = "Brood_ID")

  out <- geosphere::bearing(p1 = x[,c("E","N")],
                            p2 = x[,c("long", "lat")])

  return(out)

}

