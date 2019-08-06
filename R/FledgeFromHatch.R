#' Calculate Fledging date for hatching date
#' @param x Julian day
#' @return Fledging day as June day
#' @export
#'
FledgeFromHatch <- function(x) {
  out <- as.Date((x + 46), origin = "2019-01-01")
  print(out)
  return(DBChecks::doy(out))
}


