#' Create shape file using data frame as input
#'
#' @param df data frame
#' @param dsn output directory
#' @param layer layer name
#' @param Long Longitude
#' @param Lat Latitude
#' @export
#'
df2shape <- function(df = NULL, dsn = NULL, layer = NULL, Long = "E", Lat = "N") {

  long.var <- which(names(df) == Long)
  lat.var <- which(names(df) == Lat)
  names(df)[c(long.var, lat.var)] <- c("Long", "Lat")

  df <- dplyr::filter(df, !is.na(Long), !is.na(Lat))

  ## set coords
  sp::coordinates(df) =~Long+Lat

  ## set CRS
  sp::proj4string(df) <- sp::CRS("+proj=longlat +datum=WGS84")

  rgdal::writeOGR(obj = df, dsn = dsn, layer = layer, driver = "ESRI Shapefile", overwrite_layer = T)
}
