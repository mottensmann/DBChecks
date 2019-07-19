#' Centoid of a territory
#'
#' @param name Territory name
#' @param df data frame
#' @param weighted Logical. By default weighted mean of all breeding attempts
#' @return Centroid lon and lat coordinates
#' @import magrittr
#' @export
#'
terr.centroid <- function(name = NULL, df = buzzard_db[["repro_fledge_db"]], weighted = T) {
  if (is.null(name)) stop("Specify a Territory")
  if (name %in% df[["Territory"]]) {
    out <- dplyr::filter(df, Territory == name, !is.na(E), !is.na(N))[,c("E",  "N")]
    if (nrow(out) < 4) {
      coords <- data.frame(centroid.lat = out[["N"]][1],
                           centroid.long = out[["E"]][1])
    } else if (unique(out[["N"]]) %>% length == 1) {
      coords <- data.frame(centroid.lat = out[["N"]][1],
                           centroid.long = out[["E"]][1])
    } else if (unique(out[["E"]]) %>% length == 1) {
      coords <- data.frame(centroid.lat = out[["N"]][1],
                           centroid.long = out[["E"]][1])
    } else if (unique.data.frame(out) %>% nrow > 3) {
      coords <- geosphere::centroid(out[,c("E", "N")]) %>%
        .[2:1] %>%
        set_names(x = ., value =  c("centroid.lat", "centroid.long"))
    } else {
      coords <- data.frame(centroid.lat = out[["N"]][1],
                           centroid.long = out[["E"]][1])
    }
  } else {
    stop("Territory", name, "does not exist")
  }
  return(coords)
}
# load("../../01-PhD/00-Raw/RData/buzzard_db.RData")
# library(magrittr)
# name <- "Dreck"
# df = buzzard_db[["repro_fledge_db"]]
