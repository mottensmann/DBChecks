#' Create shape for a single coordinate
#'
#' @param lat lat
#' @param long long
#' @param CRS CRS for given lat and long
#' @param out By default temp file
#' @param EPSG EPSG code
#' @import magrittr
#' @inheritParams meancoordinates
#' @export
#'
coord2shape <- function(lat = NULL, long = NULL,
                        out = DBChecks::temp.shp(),
                        CRS = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
                        root = 'C:/OSGeo4W64',
                        EPSG = "EPSG:32632") {
  ## set up RQGIS
  ## ###########################################################################
  if ("RQGIS3" %in% rownames(installed.packages())) {
    library(RQGIS3, quietly = T)
    ## Where to find RQGIS
    if (!is.null(root)) {
      RQGIS3::set_env(root = root)
    } else {
      RQGIS3::set_env()
    }
  } else {
    stop("Install RQGIS3 to proceed")
  }
  ## ###########################################################################


  ## Turn coordinates into a shape file
  shape <- data.frame(N = lat, E = long)

  ## set coords
  sp::coordinates(shape) =~E+N

  ## set CRS
  sp::proj4string(shape) <- sp::CRS(CRS)

  ## convert to SpatialPointsDataFrame
  shape <- as(shape,"SpatialPointsDataFrame")
  attr(shape,"data") <- data.frame(name = "coordinate", id = 1)

  ## write to file
  temp.file <- temp.shp()
  rgdal::writeOGR(obj = shape, dsn = temp.file, driver = "ESRI Shapefile", layer = "coord")

  ## reproject
  x <- RQGIS3::run_qgis(
    alg = "native:reprojectlayer",
    INPUT = temp.file,
    TARGET_CRS = EPSG,
    OUTPUT = out,
    load_output = F,
    show_output_paths = F)
  unlink(temp.file)

  return(out)
}
