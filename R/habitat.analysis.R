#' Habitat analysis
#'
#' @description Crops vector layers using overlay and recalculates area
#' (if polygon) and length (if line) of objects
#'
#' @details vectorised if length overlay > 1
#'
#' @details
#' 1.) Clip to overlay extent (native:clip)
#' 2.) Recalculate geometry of objects
#' 3.) Output data.frame
#'
#' @param features list of ESRI shapes
#' @param overlay overlay ESRI shape file
#' @inheritParams meancoordinates
#' @import magrittr
#' @export
#'
habitat.analysis <- function(features = NULL,
                             overlay =  NULL,
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
.RData <- stringr::str_replace(overlay, ".shp", ".RData")


## Loop through features
## ###########################################################################
results <- lapply(features, function(one_feature) {

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

  ## Recalculate geometry
  ## ####################
  temp.file2 <- temp.shp()
  if (any(names(x) == "Area")) {
    x <- RQGIS3::run_qgis(
      alg = "qgis:fieldcalculator",
      INPUT = temp.file,
      FIELD_NAME = "NEW_AREA",
      FIELD_TYPE = 0,
      FIELD_PRECISION = 3,
      FORMULA = "$area/10000", #ha
      OUTPUT = temp.file2,
      load_output = T,
      show_output_paths = F)
    out <- sum(x[["NEW_AREA"]])
  } else if (any(names(x) %in% c("length", "Length"))) {
    x <- RQGIS3::run_qgis(
      alg = "qgis:fieldcalculator",
      INPUT = temp.file,
      FIELD_NAME = "NEW_LENGTH",
      FIELD_TYPE = 0,
      FIELD_PRECISION = 3,
      FORMULA = "$length", #ha
      OUTPUT = temp.file2,
      load_output = T,
      show_output_paths = F)
    out <- sum(x[["NEW_LENGTH"]])
  } else {
    stop(paste("Unknown geometry in", one_feature))
  }

  ## discard temp files
  ## ##################
  unlink(temp.file)
  unlink(temp.file2)
  #####################
  return(out)
}) %>%
  set_names(names(features))

save(results, file = .RData)
return(results)
}

