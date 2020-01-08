#' Nest2polygon
#'
#' @description
#' Creates polygons from nest coordinates by connecting nests forming the
#' maximum territory extent by lines. Then, lines are transformed to polygons.
#' Requires `RQGIS3` and `QGIS`
#'
#' @param
#' df data frame containing the following variables:
#'  `E` (Long), `N` (Lat) and `Terr_ID` as a grouping variable
#'
#' @inheritParams PlaceInfo
#'
#' @import magrittr reticulate
#'
#' @return
#'
#' @export
#'
Nest2Polygon <- function(df = NULL, root = 'C:/OSGeo4W64') {

  ## load and set up interface to QGIS
  if ("RQGIS3" %in% rownames(installed.packages())) {
    library(RQGIS3, quietly = T)
  } else {
    stop("Install RQGIS3 to proceed")
  }

  ## Where to find RQGIS
  if (!is.null(root)) {
    RQGIS3::set_env(root = root)
  } else {
    RQGIS3::set_env()
  }

  ## get corner coordinates for all territories
  shape <- lapply(unique(buzzard_db[["repro_fledge_db"]][["Territory"]]), function(terr) {
    df <-
      dplyr::filter(buzzard_db$repro_fledge_db, Territory == terr)[,c("E","N", "Terr_ID")] %>%
      unique.data.frame()

    N <- order(df[["N"]], decreasing = T)[1]
    S <- order(df[["N"]], decreasing = F)[1]
    E <- order(df[["E"]], decreasing = T)[1]
    W <- order(df[["E"]], decreasing = F)[1]

    df2 <- df[c(N, E, S, W, N),]
    df2[["seq"]] <- 1:nrow(df2)
    return(df2)
  }) %>%
    do.call("rbind",.)

  ## set coords
  sp::coordinates(shape) =~E+N

  ## set CRS
  sp::proj4string(shape) <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

  ## convert to SpatialPointsDataFrame
  shape <- as(shape,"SpatialPointsDataFrame")

  ## create temp files
  shape.temp <- file.path(tempdir(),
                          paste0(paste0(sample(c(LETTERS, letters, 1:9), 8, replace = T), collapse = ""), "shape.shp")
  )
  path.temp <- file.path(tempdir(),
                         paste0(paste0(sample(c(LETTERS, letters, 1:9), 8, replace = T), collapse = ""), "path.shp")
  )
  polygons.temp <- file.path(tempdir(),
                             paste0(paste0(sample(c(LETTERS, letters, 1:9), 8, replace = T), collapse = ""), "polygons.shp")
  )

  ## write shape file
  rgdal::writeOGR(obj = shape, dsn = shape.temp, driver = "ESRI Shapefile", layer = "coord")

  ## Query algorithm to create path (i.e. a line geometry)
  RQGIS3::run_qgis(alg = "qgis:pointstopath",
                   INPUT = shape.temp,
                   ORDER_FIELD = "seq",
                   GROUP_FIELD = "Terr_ID",
                   OUTPUT = path.temp,
                   load_output = F,
                   show_output_paths = F)

  # RQGIS3::find_algorithms("linestopolygons")
  # RQGIS3::get_args_man("saga:convertlinestopolygons")
  # RQGIS3::get_options("saga:convertlinestopolygons")

  ## path to polygons
  RQGIS3::run_qgis(alg = "saga:convertlinestopolygons",
                   LINES = path.temp,
                   POLYGONS = polygons.temp,
                   load_output = F,
                   show_output_paths = F)
  ## discard temp files
  unlink(shape.temp)
  unlink(path.temp)
  unlink(polygons.temp)

}#end Nest2polygon
# rm(list = ls())
# library(magrittr)
# library(reticulate)
# library(RQGIS3)
# load("../../01-PhD/00-Raw/RData/buzzard_db.RData")


