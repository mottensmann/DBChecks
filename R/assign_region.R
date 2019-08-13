#' Assign nest to natural region
#'
#' @inheritParams PlaceInfo
#' @param overlay shape file
#'
#' @export
#'
assign_region <- function(lat = NULL, long = NULL, overlay = NULL, root = 'C:/OSGeo4W64') {
  if ("RQGIS3" %in% rownames(installed.packages())) {
    library(RQGIS3, quietly = T)
  } else {
    stop("Install RQGIS3 to proceed")
  }

  ## check that shape file is there
  if (file.exists(system.file("extdata", "NaturalRegion.shp", package = "DBChecks"))) {
    region <- system.file("extdata", "NaturalRegion.shp", package = "DBChecks")
  } else {
    stop("NaturalRegion.shp not found in extdata")
  }

  ## Where to find RQGIS
  if (!is.null(root)) {
    RQGIS3::set_env(root = root)
  } else {
    RQGIS3::set_env()
  }

  ## Turn coordinates into a shape file
  shape <- data.frame(N = lat, E = long)

  ## set coords
  sp::coordinates(shape) =~E+N

  ## set CRS
  sp::proj4string(shape) <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

  ## convert to SpatialPointsDataFrame
  shape <- as(shape,"SpatialPointsDataFrame")
  attr(shape,"data") <- data.frame(name = "coordinate", id = 1)

  ## write a temp file
  shape.temp <- file.path(tempdir(),
                          paste0(paste0(sample(c(LETTERS, letters, 1:9), 8, replace = T), collapse = ""), "shape.shp")
  )
  rgdal::writeOGR(obj = shape, dsn = shape.temp, driver = "ESRI Shapefile", layer = "coord")


  ## Query algorithm

  joined.temp <- file.path(tempdir(),
                           paste0(paste0(sample(c(LETTERS, letters, 1:9), 8, replace = T), collapse = ""), "joined.shp")
  )

  out <- RQGIS3::run_qgis(alg = "qgis:joinattributesbylocation",
                          INPUT = region,
                          JOIN = shape.temp,
                          METHOD = 1,
                          PREDICATE = 0,
                          DISCARD_NONMATCHING = T,
                          OUTPUT = joined.temp,
                          load_output = T,
                          show_output_paths = F)
  ## reformat
  out <- data.frame(Country = out[['NAME_0']],
                    State = out[['NAME_1']],
                    District = out[['NAME_2']],
                    Municipality = out[['NAME_3']])
  ## discard temp files
  unlink(shape.temp)
  unlink(joined.temp)
  return(out)

}

