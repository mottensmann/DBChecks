#' Mode: Calculates the mode of a vector
#'
#' @param x
#' Vector of numbers or characters
#'
#' @param na.rm
#' Boolean indicating to remove NA values
#'
#' @details
#' Ignores NA, if na.rm = TRUE
#'
#' @export
#'
Mode <- function(x, na.rm = TRUE) {
  if (isTRUE(na.rm)) {
    ux <- unique(x[!is.na(x)])
    x <- x[!is.na(x)]
  } else {
    ux <- unique(x)
  }
  return(ux[which.max(tabulate(match(x, ux)))])
}



