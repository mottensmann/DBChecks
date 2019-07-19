#' Centoid of a territory
#'
#' @param name Territory name
#' @param df data frame
#' @param weighted Logical. By default only considers unique nests
#' @return Centroid lon and lat coordinates
#' @import magrittr
#' @export
#'
terr.centroid <- function(name = NULL, df = buzzard_db[["repro_fledge_db"]], weighted = F) {
  if (is.null(name)) stop("Specify a Territory")
  if (name %in% df[["Territory"]]) {
    out <- dplyr::filter(df, Territory == name, !is.na(E), !is.na(N))[,c("E",  "N")]
    # rename N, E to lat and long
    names(out) <- c("Long", "Lat")
    if (isFALSE(weighted)) out <- unique.data.frame(out)
    if (nrow(out) < 4) {
      coords <- data.frame(centroid.lat = out[["Lat"]][1],
                           centroid.long = out[["Long"]][1])
    } else if (unique(out[["Lat"]]) %>% length == 1) {
      coords <- data.frame(centroid.lat = out[["Lat"]][1],
                           centroid.long = out[["Long"]][1])
    } else if (unique(out[["Long"]]) %>% length == 1) {
      coords <- data.frame(centroid.lat = out[["Lat"]][1],
                           centroid.long = out[["Long"]][1])
    } else if (unique.data.frame(out) %>% nrow > 3) {
      coords <- geosphere::centroid(out[,c("Long", "Lat")]) %>%
        .[2:1] %>%
        set_names(x = ., value =  c("centroid.lat", "centroid.long"))
    } else {
      coords <- data.frame(centroid.lat = out[["Lat"]][1],
                           centroid.long = out[["Long"]][1])
    }
  } else {
    stop("Territory", name, "does not exist")
  }
  return(coords)
}
# load("../../01-PhD/00-Raw/RData/buzzard_db.RData")
# library(magrittr)
# name <- "Wittenbrock III"
# df = buzzard_db[["repro_fledge_db"]]
