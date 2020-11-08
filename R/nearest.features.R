#' Habitat analysis: Nearest features
#'
#' @description
#' Calculates the shortest distance between a point and the edges of a feature.
#' Preprocessing as in habitat.analysis.R
#'
#' @details
#' 1.) If feature is a polygon convert to lines using SAGA
#' 2.) Create points along lines
#' 3.) find nearest match between points of both layers
#'
#' @param features list of ESRI shapes
#' @param overlay overlay ESRI shape file
#' @param point point ESRI shape file
#' @inheritParams meancoordinates
#' @import magrittr
#' @export
#'
nearest.features <- function(features = NULL,
                             overlay =  NULL,
                             point = stringr::str_replace(overlay, "buffer.shp", ".shp"),
                             root =  'C:/OSGeo4W64') {

  ## checks:
  ## ###########################################################################
  if (class(features) != "list") stop("features must be a list of shapes")

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

  ## define output file
  .RData <- stringr::str_replace(overlay, ".shp", ".edges.RData")


  ## Loop through features
  ## ###########################################################################
  results <- lapply(features, function(one_feature) {
    #cat(one_feature, "\n")

    temp.file <- temp.shp()
    ## Clip to overlay extent
    ## ######################
    x <- RQGIS3::run_qgis(
      alg = "native:clip",
      INPUT = one_feature,
      OVERLAY = overlay,
      OUTPUT = temp.file,
      load_output = T,
      show_output_paths = F)
    ## ######################

    ## Convert to Lines
    ## ####################
    if (nrow(x) > 1) {
    temp.file2 <- temp.shp()
    if (any(names(x) == "Area")) {
      x <- RQGIS3::run_qgis(
        alg = "saga:convertpolygonstolines",
        POLYGONS = temp.file,
        LINES = temp.file2,
        load_output = T,
        show_output_paths = F)
    } else if (any(names(x) %in% c("length", "Length"))) {
      temp.file2 <- temp.file
    } else {
      stop(paste("Unknown geometry in", one_feature))
    }

    ## Points along line
    ## ####################
    temp.file3 <- temp.shp()
    #temp.file3 <- "../../01-PhD/01-Paper/03-Age-of-first-reproduction/data/GIS/nest01-points.shp"
    x <- RQGIS3::run_qgis(
      alg = "native:pointsalonglines",
      INPUT = temp.file2,
      OUTPUT = temp.file3,
      load_output = T,
      show_output_paths = F)

    ## Nearest match
    temp.file4 <- temp.shp()
    #temp.file4 <- "../../01-PhD/01-Paper/03-Age-of-first-reproduction/data/GIS/nest01-knea.shp"
    x <- RQGIS3::run_qgis(
      alg = "qgis:distancematrix",
      # INPUT = temp.file3,
      # INPUT_FIELD = "ID",
      TARGET = temp.file3,
      TARGET_FIELD = "ID",
      # TARGET = point,
      # TARGET_FIELD = "name",
      INPUT = point,
      INPUT_FIELD = "name",
      NEAREST_POINTS = 1,
      OUTPUT = temp.file4,
      show_output_paths = F,
      load_output = T)


    ## discard temp files
    ## ##################
    unlink.shp(temp.file)
    unlink.shp(temp.file2)
    unlink.shp(temp.file3)
    unlink.shp(temp.file4)
    #####################
    return(ifelse(length(x$Distance) > 0, min(x$Distance), NA))

    } else {
    ## no geometry detected
    return(NA)
    }


  }) %>%
    set_names(names(features)) %>%
    do.call("cbind",.) %>%
    as.data.frame()

  save(results, file = .RData)
  return(results)
}


# library(DBChecks)
# test <- nearest.features(features = list(
#   Forest = "../../01-PhD/GIS-Projects/04-HabitatImprinting/DLM/Forest.shp",
#   Residential = "../../01-PhD/GIS-Projects/04-HabitatImprinting/DLM/Residential.shp",
#   Wood =  "../../01-PhD/GIS-Projects/04-HabitatImprinting/DLM/Wood.shp",
#   Streams =  "../../01-PhD/GIS-Projects/04-HabitatImprinting/DLM/Streams.shp",
#   Street =  "../../01-PhD/GIS-Projects/04-HabitatImprinting/DLM/Streets.shp",
#   Mainroad = "../../01-PhD/GIS-Projects/04-HabitatImprinting/DLM/MainRoad.shp"),
#   overlay = "../../01-PhD/01-Paper/03-Age-of-first-reproduction/data/GIS/nest22buffer.shp",
#   root = 'C:/OSGeo4W64')
# one_feature <- features$Wood
