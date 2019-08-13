#' K-means
#'
#' @param k number of clusters to compute
#' @inheritParams birdinfo
#' @inheritParams meancoordinates
#' @import magrittr
#' @export
#'
k.means <- function(buzzard_db = "RData/buzzard_db.RData", root =  'C:/OSGeo4W64', k = 2) {

  load(buzzard_db)

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

## create shape file from nest coords
## Turn coordinates into a shape file
shape.df <- data.frame(N = buzzard_db[["repro_fledge_db"]][["N"]], E = buzzard_db[["repro_fledge_db"]][["E"]],
                    Nest = buzzard_db[["repro_fledge_db"]][["Nest"]]) %>%
  na.omit() %>%
  unique.data.frame()
shape <- shape.df
n <- nrow(shape)


## set coords
sp::coordinates(shape) =~E+N

## set CRS
sp::proj4string(shape) <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

## convert to SpatialPointsDataFrame
shape <- as(shape,"SpatialPointsDataFrame")
attr(shape,"data") <- data.frame(id = 1:n, Nest = shape.df$Nest, N = shape.df$N, E = shape.df$E)

## write a temp file
shape.temp <- file.path(tempdir(),
                        paste0(paste0(sample(c(LETTERS, letters, 1:9), 8, replace = T), collapse = ""), "shape.shp")
)
rgdal::writeOGR(obj = shape, dsn = shape.temp, driver = "ESRI Shapefile", layer = "coord")


# 1. Call k-means clustering in QGIS
## ###########################################################################
temp.file <- temp.shp()
x <- RQGIS3::run_qgis(
  alg = "native:kmeansclustering",
  INPUT = shape.temp,
  OUTPUT = temp.file,
  CLUSTERS = k,
  FIELD_NAME = "Terr_ID",
  load_output = T,
  show_output_paths = F)
unlink(shape.temp)
unlink(temp.file)
## ###########################################################################
## ###########################################################################
## ###########################################################################

return(x)
# 2. Caclculate SS within clusters for each k
# 3. evaluate

}
