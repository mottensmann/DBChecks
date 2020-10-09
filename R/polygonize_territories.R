#' Polygonize territories
#'
#' @description
#' Based on all nest coordinates for a given territory, a polygon area is
#' created using `Delaunay triangulation`. Geometries are simplified using the
#' `Dissolve option` in `QGIS3`
#'
#' @details
#' Applied to each territory separately to avoid issues caused by overlapping
#' geometric features. Resulting polygon are then merged within a single
#' shape file layer. Note, the triangulation and polygonization only works if
#' there are at least three coordinates available. Hence, if this criterion is
#' not fullfilled dummy coordinates are added.
#'
#' @param nests data frame
#'
#' @param out path
#'
#' @param tempdir temp directory
#'
#' @inheritParams PlaceInfo
#'
#' @export
#'
polygonise_territories <- function(nests = buzzard_db[["repro_fledge_db"]],
                                   root = 'C:/OSGeo4W64',
                                   temp.dir = "Shapes/temp/",
                                   out = "Shapes/TerrPolygon2.shp") {

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

  ## Apply for each territory

  silent <- pbapply::pblapply(unique(nests[["Terr_ID"]]), function(terr) {
    ## select nests and create shape file
    shape <- dplyr::filter(nests, Terr_ID == terr)[,c("N", "E", "Terr_ID")] %>%
      unique.data.frame()

    ## check if minimum number of three nests exist
    if (nrow(shape) < 3) {
      ## add dummy coordinates to create polygon
      if (nrow(shape) == 1) {
        dummy_north <- shape
        dummy_north[["N"]] <- dummy_north[["N"]] + .00037
        dummy_north[["E"]] <- dummy_north[["E"]] + .00037
        dummy_north2 <- shape
        dummy_north2[["N"]] <- dummy_north2[["N"]] + .00037
        dummy_north2[["E"]] <- dummy_north2[["E"]] - .00037
        dummy_south <- shape
        dummy_south[["N"]] <- dummy_south[["N"]] - .00037
        dummy_south[["E"]] <- dummy_south[["E"]] - .00037
        dummy_south2 <- shape
        dummy_south2[["N"]] <- dummy_south2[["N"]] - .00037
        dummy_south2[["E"]] <- dummy_south2[["E"]] + .00037
        shape <- rbind(shape, dummy_north, dummy_north2, dummy_south, dummy_south2)
      } else if (nrow(shape) == 2) {
        # north:
        north <- which(shape[["N"]] == max(shape[["N"]]))
        south <- which(shape[["N"]] == min(shape[["N"]]))
        west <- which(shape[["E"]] == min(shape[["E"]]))
        east <- which(shape[["E"]] == max(shape[["E"]]))
        if (north == west) {
          dummy_north <- shape[north,]
          dummy_south <- shape[south,]
          dummy_north[["E"]] <- dummy_north[["E"]] + .00075
          dummy_south[["E"]] <- dummy_south[["E"]] - .00075
        } else {
          dummy_north <- shape[north,]
          dummy_south <- shape[south,]
          dummy_north[["E"]] <- dummy_north[["E"]] - .00075
          dummy_south[["E"]] <- dummy_south[["E"]] + .00075
        }

        shape <- rbind(shape, dummy_north, dummy_south)
      } else {
        shape <- rbind(shape, shape[1,])
      }
    }

    ## set coords
    sp::coordinates(shape) =~E+N

    ## convert to SpatialPointsDataFrame
    shape <- as(shape,"SpatialPointsDataFrame")

    ## create temp shape files
    shape.temp <-
      file.path(tempdir(),
                paste0(paste0(sample(c(LETTERS, letters, 1:9), 8, replace = T), collapse = ""),
                       "shape.shp")
    )
    triangulation.temp <-
      file.path(tempdir(),
                paste0(paste0(sample(c(LETTERS, letters, 1:9), 8, replace = T), collapse = ""),
                       "shape.shp")
      )
    polygon.temp <- paste0(terr, ".shp")

    ## write shape file
    rgdal::writeOGR(obj = shape, dsn = shape.temp, driver = "ESRI Shapefile", layer = "coord")

    ## conduct triangulation
    RQGIS3::run_qgis(alg = "qgis:delaunaytriangulation",
                     INPUT = shape.temp,
                     OUTPUT = triangulation.temp,
                     load_output = F,
                     show_output_paths = F)

    ## dissolve segments
    RQGIS3::run_qgis(alg = "native:dissolve",
                       INPUT = triangulation.temp,
                       OUTPUT = paste0(temp.dir, polygon.temp, collapse = "/"),
                       load_output = F,
                       show_output_paths = F)
    ## remove temp file
    unlink(shape.temp)
    unlink(triangulation.temp)

  }) #end lapply

  # Merge layers into one file
  RQGIS3::run_qgis(alg = "native:mergevectorlayers",
                   LAYERS = list.files(temp.dir, pattern = ".shp", full.names = T),
                   OUTPUT = out,
                   load_output = F,
                   show_output_paths = F)

  ## erase temp files
  list_of_files <- list.files(path = temp.dir, full.names = T)
  silent <- lapply(list_of_files, unlink)
}#end Main Function

# rm(list = ls())
# load("../../01-PhD/00-Raw/RData/buzzard_db.RData")
# library(magrittr)
# root = 'C:/OSGeo4W64'
# nests <- buzzard_db[["repro_fledge_db"]]
# terr <- dplyr::filter(buzzard_db$repro_fledge_db, Territory == "Ahlerbruch Nord")[["Terr_ID"]] %>%
#   unique()
# out = "../../01-PhD/00-Raw/Shapes/TerrPolygon2.shp"
# temp.dir = "../../01-PhD/00-Raw/Shapes/temp/"

# RQGIS3::find_algorithms("merge")
# RQGIS3::get_args_man("native:mergevectorlayers")
# # RQGIS3::get_options("saga:convertlinestopolygons")
