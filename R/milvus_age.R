#' Estimate the age of juvenile Red Kite based on wing length
#'
#' @description Red Kite age is estimated based on a standard growth curve modeled from published data by
#' Mammen & Stubbe 1995: Alterseinschätzung und Brutbeginn des Rotmilans (Milvus milvus). Vogel und Umwelt Band 8:91:98
#'
#' @param x wing length in mm
#'
#' @references
#' Mammen & Stubbe 1995: Alterseinschätzung und Brutbeginn des Rotmilans (Milvus milvus). Vogel und Umwelt Band 8:91:98
#'
#' @export
#'
#' @param x wing length in cm
#'
#' @return estimated age
#'
milvus_age <- function(x) {
    y <- -3.88 + (3.663*exp(1)^-1*x) - (2.105*exp(1)^(-3*x^2)) + (6.3*exp(1)^(-6*x^3)) - (6.43859*exp(1)^(-9*x^4))
    return(y)
}
