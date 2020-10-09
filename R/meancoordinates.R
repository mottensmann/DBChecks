#' Wrapper to use QGIS native:meancoordinates function
#'
#' @description
#' Creates a SpatialPointsDataFrame from a data frame and then uses the
#' meancoordinates function of QGIS vias RQGIS3 to calculate the mean
#' coordinates of each territory.
#'
#' @param df data frame
#' @param root root of QGIS installation. By default attemtps to find it
#' @return data frame with mean x and y coordinates
#' @import magrittr
#' @export
#'
meancoordinates <- function(df = NULL, root = NULL) {

  if ("RQGIS3" %in% rownames(installed.packages())) {
    library(RQGIS3)
  } else {
    warning("Install RQGIS3 to proceed")
    input <- readline(prompt="Requires RQGIS3: Press 'Y' to install")
    if (input == "Y") {
      devtools::install_github("jannes-m/RQGIS3")
    }
  }

  ## Where to find RQGIS
  if (!is.null(root)) {
    RQGIS3::set_env(root = root)
  } else {
    RQGIS3::set_env()
  }

  ## select data
  shape <- df[,c("Terr_ID", "N", "E")] %>%
    na.omit()

  ## set coords
  sp::coordinates(shape) =~E+N

  ## set CRS
  sp::proj4string(shape) <- sp::CRS("+proj=longlat +datum=WGS84")

  out <- RQGIS3::run_qgis(alg = "native:meancoordinates",
                          INPUT = shape,
                          UID = "Terr_ID",
                          OUTPUT = file.path(tempdir(), "mean_coords.shp"),
                          load_output = TRUE,
                          show_output_paths = F)
  out <- data.frame(Terr_ID = out[["Terr_ID"]],
                    Mean.X = out[["MEAN_X"]],
                    Mean.Y = out[["MEAN_Y"]])
  return(out)
}


