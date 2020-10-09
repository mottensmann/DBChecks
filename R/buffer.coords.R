#' buffer around point
#'
#' @description creates a buffer of radius n around points within a shape
#' @details radius within units of the coordinate system (i.e. apply using UTM32N)
#' @param input shape file with points
#' @param radius integer
#' @param out shape file
#' @import magrittr
#' @inheritParams habitat.analysis
#' @export
#'
buffer.coords <- function(input = NULL, radius = 500, out = NULL, root = "C:/OSGeo4W64") {
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

  ## Buffer
  ## ######################
  x <- RQGIS3::run_qgis(
    alg = "native:buffer",
    INPUT = input,
    DISTANCE = radius,
    SEGMENTS = 10,
    OUTPUT = out,
    load_output = F,
    show_output_paths = F)
  rm(x)
  ## ######################
}
