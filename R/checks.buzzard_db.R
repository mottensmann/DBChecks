#' Checking buzzard_db
#' @description
#' Checks entries of the ringing list for potential issues
#'
#' @param input path to 'buzzard_db.RData'
#' @param output destination for output
#' @return text file
#' @export
#' @import magrittr
#'
check.buzzard_db <- function(input = "RData/buzzard_db.RData",
                             output = paste0("Checks/buzzard_db-", Sys.Date(),".txt")) {

  ## set up
  ## -------------------------------------------------------------------------
  options(width = 1e4)
  # source("R/check.dupl.R")
  load(input)
  sink(file = output, append = F, split = F)
  cat("Checking buzzard_db\t\t", as.character(Sys.time()),"\n",
      "=======================================================================================")
  cat("\n\nbuzzard_db version info:\n")
  print(buzzard_db[["version"]], row.names = F)
  cat("=======================================================================================",
      "\n\n")
  sink()
  ## -------------------------------------------------------------------------

  ## 0. Replace "Umlaute"
  ## -------------------------------------------------------------------------
  # buzzard_db[["ring_db"]][["Territory"]] <-
  #   stringi::stri_replace_all_fixed(
  #     str = buzzard_db[["ring_db"]][["Territory"]],
  #     pattern = c("ä", "ö", "ü", "Ä", "Ö", "Ü", "ß"),
  #     replacement = c("ae", "oe", "ue", "Ae", "Oe", "Ue", "ss"),
  #     vectorize_all = FALSE
  #   )
  #
  # buzzard_db[["repro_fledge_db"]][["Territory"]] <-
  #   stringi::stri_replace_all_fixed(
  #     str = buzzard_db[["repro_fledge_db"]][["Territory"]],
  #     pattern = c("ä", "ö", "ü", "Ä", "Ö", "Ü", "ß"),
  #     replacement = c("ae", "oe", "ue", "Ae", "Oe", "Ue", "ss"),
  #     vectorize_all = FALSE
  #   )
  ## -------------------------------------------------------------------------

  ## 1. Duplicated rings:
  ## -------------------------------------------------------------------------
  dupl.rings <-
    dplyr::filter(buzzard_db[["ring_db"]],
                  Ring %in% check.dupl("Ring", buzzard_db[["ring_db"]])) %>%
    .[,c("Territory", "Year", "Ring", "ID")] %>%
    unique.data.frame %>%
    dplyr::filter(., Ring %in% check.dupl("Ring", df = .))
  pruned <- lapply(dupl.rings[["Ring"]] %>% unique, function(x) {
    TempDf <- dplyr::filter(dupl.rings, Ring == x)

    ## check if only ID difers
    test1 <- unique.data.frame(TempDf[, c("Territory", "Ring")]) %>%
      nrow
    ## check if ID is either "Metal right" or NA
    test2 <- any(TempDf[["ID"]] %in% c("Metal right", NA))
    if (test1 == 1 & isTRUE(test2)) {
      return(data.frame())
    } else {
      return(TempDf)
    }
  }) %>%
    do.call("rbind",.) %>%
    as.data.frame
  sink(output, append = T)
  cat("Duplicated entries Rings:\n")
  cat("=======================================================================================")
  cat("\n")
  print(pruned[,c("Ring", "ID", "Territory", "Year")], row.names = F, right = F)
  cat("=======================================================================================")
  sink()
  ## -------------------------------------------------------------------------

  ## 2. Duplicated IDs:
  ## -------------------------------------------------------------------------
 # x <- "Metal right, white wingtag N5"
  dupl.ids <-
    dplyr::filter(buzzard_db[["ring_db"]],
                  ID %in% check.dupl("ID", buzzard_db[["ring_db"]],
                                     not = "Metal right")) %>%
    .[,c("Territory", "Year", "Ring", "ID","Date", "Dead", "DateDeath")] %>%
    unique.data.frame %>%
    dplyr::filter(., ID %in% check.dupl("ID", df = .))

  pruned <- lapply(dupl.ids[["ID"]] %>% unique, function(x) {
    TempDf <- dplyr::filter(dupl.ids, ID == x)
    if (nrow(TempDf) > 1) {
      test <- sapply(2:nrow(TempDf), function(r) {
      check <- TempDf[["Date"]][r] > TempDf[["DateDeath"]][r - 1]
          if (isTRUE(check)) out <- r
          else out <- 1
      })
      if (length(test) > 0) TempDf <- TempDf[-(test - 1),]
      if (nrow(TempDf) == 1) TempDf <- data.frame()
      return(TempDf)
    }

    ## check if only ID difers
    test1 <- unique.data.frame(TempDf[, c("Territory", "Ring", "ID")]) %>%
      nrow

    ## check if ID is either "Metal right" or NA
    test2 <- any(TempDf[["ID"]] %in% c("Metal right", NA))
    if (test1 == 1 & isTRUE(test2)) {
      return(data.frame())
    } else {
      return(TempDf)
    }
  }) %>%
    do.call("rbind",.) %>%
    as.data.frame
  sink(output, append = T)
  cat("\n\n")
  cat("Duplicated entries Wingtags:\n")
  cat("=======================================================================================")
  cat("\n")
  print(pruned[,c("ID", "Ring", "Territory", "Year", "Dead", "DateDeath")], row.names = F, right = F)
  cat("=======================================================================================")
  sink()
  ## -------------------------------------------------------------------------

  ## 3. Duplicated Terr_IDs
  ## -------------------------------------------------------------------------
  dupl.terrid <- buzzard_db[["ring_db"]][,c("Territory", "Terr_ID")]
    # dplyr::filter(buzzard_db[["ring_db"]],
    #               Terr_ID %in% check.dupl("Terr_ID", buzzard_db[["ring_db"]])) %>%
    # .[,c("Territory", "Terr_ID")] %>%
    # unique.data.frame

  pruned <- lapply(dupl.terrid[["Terr_ID"]] %>% unique, function(x) {
    TempDf <- dplyr::filter(dupl.terrid, Terr_ID == x)

    ## check if only ID difers
    test <- unique.data.frame(TempDf)
    if (nrow(test) == 1) {
      return(data.frame())
    } else {
      return(TempDf)
    }
  }) %>%
    do.call("rbind",.) %>%
    as.data.frame
  sink(output, append = T)
  cat("\n\n")
  cat("Duplicated Terr_ID:\n")
  cat("=======================================================================================")
  cat("\n")
  print(pruned, row.names = F, right = F)
  cat("=======================================================================================")
  sink()
  ## -------------------------------------------------------------------------
  ## ## -------------------------------------------------------------------------
  ##

}# end check.buzzard_db
