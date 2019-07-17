#' Identifies and returns duplicated characters
#' @param var variable name
#' @param df data frame
#' @param not string to ignore
#' @return character vector
#' @export
#'
check.dupl <- function(var = NULL, df = buzzard_db[["ring_db"]], not = NULL) {
  if (!is.null(not)) df <- dplyr::filter(df, get(var) != not)
  duplicates <- summary(df[[var]] %>% as.factor, maxsum = 9999) %>%
    .[which(. > 1)]
  return(names(duplicates))
}
