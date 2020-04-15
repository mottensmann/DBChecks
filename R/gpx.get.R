#' Get corresponding GPX waypoint for a pair of coordinates
#' @param gpx.sp gpx waypoints
#' @param repro repro & fledge data
#' @param digits desired resolution (i.e. decimal point)
#' @export
gpx.get <- function(gpx.sp = gpx,
                    repro = repro_fledge_db, digits = 5) {

  ## 0. Remove column nest
  repro <- repro[,-(which(names(repro) == "Nest"))]

  ## 1. get gpx data
  gpx.sp <- data.frame(gpx[["name"]], gpx@coords[,2:1]) %>%
    set_colnames(c("Nest", "N", "E"))

  ## 2. apply rounding
  gpx.sp[,c("N", "E")] <- apply(gpx.sp[,c("N", "E")], 2, round, digits = digits)
  repro[,c("N", "E")] <- apply(repro[,c("N", "E")], 2, round, digits = digits)

  dim1 <- nrow(repro)
  ## 3. add information from repro_fledge
  repro <- dplyr::left_join(repro, gpx.sp, by = c("N", "E"))
  if (nrow(repro) != dim1) stop("At least one GPX point is duplicated. Please fix to proceed")

  which.nest <- which(names(repro) == "Nest")

  return(repro[,c(1:3, which.nest, 4:(which.nest - 1))])
}
