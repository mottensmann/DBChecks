#' Shannon Diversity index
#'
#' @param x vector
#' @export
#'
shannon <- function(x) {
  x <- x[!is.na(x) & x > 0]
  # estimate proportions
  prop <- x/sum(x)
  # estimate variation
  var <- sum(prop*log2(prop))*-1
  return(var)
}


