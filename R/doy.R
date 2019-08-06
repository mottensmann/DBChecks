#' Julian Day for any date
#'
#' @param date Object of class date or a string of format "YYYY-MM-DD"
#' @return Julian day
#'
#' @examples
#' ## day of current date
#' doy()
#'
#' @export
#'
doy <- function(date = Sys.Date()) {
if (is.character(date)) as.Date(date, origin = substr(date, 1,4))
  out <- strftime(date, format = "%j")
  return(as.numeric(out))
}
