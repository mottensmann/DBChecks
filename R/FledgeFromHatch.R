#' Calculate Fledging date for hatching date
#' @param x Julian day
#' @param year Year
#' @param verbose logical
#' @return Fledging day as June day
#' @export
#'
FledgeFromHatch <- function(x = NULL, year = "2019", verbose = T) {
  origin <- paste0(year,"-01-01")
  out <- as.Date((x + 46), origin = origin)
  if (verbose == T) {
  print(paste("Fledging date:", as.Date(out, format = "MM-DD")))
  print(paste("Julian day:", DBChecks::doy(out)))
  print(paste("June day:", out - as.Date(paste0(year,"-05-31"))))
  }
  return(as.numeric(out - as.Date(paste0(year,"-05-31"))))
}


