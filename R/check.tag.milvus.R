#' Identify to which of multiple possible individuals are wingtag belongs
#'
#' @description
#' Wingtags have been reused after individuals deceased. This function tests which individual is plausible when there are multiple. Additionally tests that wingtag existed at the reported date.
#'
#' @param ring_db ringing list
#' @param resights data.frame
#' @import dplyr magrittr
#' @export
#'
check.tag.milvus <- function(ring_db = NULL, resights = NULL) {

  ## Split task row by row
  out <- lapply(1:nrow(resights), function(row) {
    ## 1. Case: Ring but no ID
    if (!is.na(resights[row, "Ring"]) & is.na(resights[row, "ID"])) {
      TempDf <- dplyr::filter(ring_db,
                              Ring == resights[row, "Ring"],
                              Date < resights[row, "Date"])
      TempDf <- TempDf[order(TempDf[["Date"]], decreasing = T),]
      if (nrow(TempDf) > 0) resights[row, "ID"] <- TempDf[["ID"]][1]
      ## 2. Case ID but not Ring
    } else if (!is.na(resights[row, "ID"]) & is.na(resights[row, "Ring"])) {
      TempDf <- dplyr::filter(ring_db,
                              ID == resights[row, "ID"],
                              Date < resights[row, "Date"])
      TempDf <- TempDf[order(TempDf[["Date"]], decreasing = T),]
      if (nrow(TempDf) > 0) resights[row, "Ring"] <- TempDf[["Ring"]][1]
    }
    return(resights[row,])
  })  %>%
    do.call("rbind",.)
  return(out)
}
