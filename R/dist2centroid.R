#' Distance between Nest and territory centroid
#'
#' @param df data frame where all entries belong to the same territory
#' @return vector of distances in km
#' @export
#'
dist2centroid <- function(df = NULL) {
  geosphere::distGeo(p1 = df[,c("E", "N")],
                     p2 = df[,c("Mean.X", "Mean.Y")])/1000
}
