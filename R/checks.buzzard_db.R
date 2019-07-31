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
  cat("Checking buzzard_db\t\t", as.character(Sys.time()))
  cat("\n")
  cat("=======================================================================================")
  cat("\n\nbuzzard_db version info:\n")
  print(buzzard_db[["version"]], row.names = F)
  cat("=======================================================================================",
      "\n\n")
  sink()
  ## -------------------------------------------------------------------------

  ## 1. Duplicated rings:
  ## -------------------------------------------------------------------------
  dupl.rings <-
    dplyr::filter(buzzard_db[["ring_db"]],
                  Ring %in% check.dupl("Ring", buzzard_db[["ring_db"]])) %>%
    .[,c("Territory", "Year", "Ring", "ID", "Age")] %>%
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
  cat("1. Duplicated Ring entries:\n")
  cat("=======================================================================================")
  cat("\n")
  print(pruned[,c("Ring", "ID", "Territory", "Year", "Age")], row.names = F, right = F)
  cat("=======================================================================================")
  sink()
  ## -------------------------------------------------------------------------

  ## 2. Duplicated IDs:
  ## -------------------------------------------------------------------------
  # x <- "Metal right, white wingtag TN"
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
        else out <- NA
      })
      test <- na.omit(test)
      if (length(test) > 0) TempDf <- TempDf[-(test - 1),]
      if (nrow(TempDf) == 1) TempDf <- data.frame()
    }

    if (nrow(TempDf) != 0) {
      ## check if only ID difers
      test1 <- unique.data.frame(TempDf[, c("Territory", "Ring", "ID")]) %>%
        nrow

      ## check if ID is either "Metal right" or NA
      test2 <- any(TempDf[["ID"]] %in% c("Metal right", NA))
      if (test1 == 1 & isTRUE(test2)) {
        return(data.frame())
      } else {
        TempDf2 <- TempDf[,-which(names(TempDf) == "Date")] %>%
          unique.data.frame
        if (nrow(TempDf2) == 1) {
          return(data.frame())
        } else {
          ## last check to avoid reporting resampled ind
          if (length(unique(TempDf[["Ring"]])) > 1) return(TempDf)
          else return(data.frame())        }
      }
    }

  }) %>%
    do.call("rbind",.) %>%
    as.data.frame
  pruned[,"Dead"][is.na(pruned[,"Dead"])] <- ""
  pruned[, "DateDeath"] <- as.character(pruned[, "DateDeath"])
  pruned[,"DateDeath"][is.na(pruned[,"DateDeath"])] <- ""
  sink(output, append = T)
  cat("\n\n")
  cat("2. Duplicated Wingtags:\n")
  cat("=======================================================================================")
  cat("\n")
  print(pruned[,c("ID", "Ring", "Territory", "Year", "Dead", "DateDeath")], row.names = F, right = F)
  cat("=======================================================================================")
  sink()
  ## -------------------------------------------------------------------------

  ## 3. Duplicated Terr_IDs in ring_db
  ## -------------------------------------------------------------------------
  dupl.terrid <- buzzard_db[["ring_db"]][,c("Territory", "Terr_ID")]

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
  cat("3.1 Duplicated Terr_ID in Ringing list:\n")
  cat("=======================================================================================")
  cat("\n")
  print(pruned, row.names = F, right = F)
  cat("=======================================================================================")
  sink()

  ## 3. Duplicated Terr_IDs in repro_fledge
  ## -------------------------------------------------------------------------
  dupl.terrid <- buzzard_db[["repro_fledge_db"]][,c("Territory", "Terr_ID")]

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
  cat("3.2 Duplicated Terr_ID in Repro & Fledging:\n")
  cat("=======================================================================================")
  cat("\n")
  print(pruned, row.names = F, right = F)
  cat("=======================================================================================")
  sink()

  ## 4. Duplicated nests
  ## -------------------------------------------------------------------------
  dupl.nests <-
    dplyr::filter(buzzard_db[["repro_fledge_db"]],
                  Nest %in% check.dupl("Nest", buzzard_db[["repro_fledge_db"]])) %>%
    .[,c("Territory", "Nest")] %>%
    unique.data.frame %>%
    dplyr::filter(., Nest %in% check.dupl("Nest", df = .))
  sink(output, append = T)
  cat("\n\n")
  cat("4. Nests shared among territories:\n")
  cat("=======================================================================================")
  cat("\n")
  print(dupl.nests[order(dupl.nests[["Nest"]]), 2:1], row.names = F, right = F)
  cat("=======================================================================================")
  sink()
  ## -------------------------------------------------------------------------

  ## 5. Missing coords for broods
  ## -------------------------------------------------------------------------
  missing.coords <- dplyr::filter(buzzard_db[["repro_fledge_db"]],
                                  is.na(N),
                                  is.na(E))
  sink(output, append = T)
  cat("\n\n")
  cat("5. Missing Nest coordinates:\n")
  cat("=======================================================================================")
  cat("\n")
  print(missing.coords[, c("Territory", "Year", "Repro")], row.names = F, right = F)
  cat("=======================================================================================")
  sink()
  ## -------------------------------------------------------------------------

  ## Changes in Morphs
  ## -------------------------------------------------------------------------
  morphs <- buzzard_db[["ring_db"]] %>%
    dplyr::filter(!is.na(Ring),
                  !is.na(Morph)) %>%
    dplyr::filter(Ring %in% .[["Ring"]][duplicated(.[["Ring"]])]) %>%
    dplyr::filter(Territory %in% .[["Territory"]][duplicated(.[["Territory"]])]) %>%
    dplyr::filter(Ring %in% .[["Ring"]][duplicated(.[["Ring"]])]) %>%
    .[,c("Territory", "Year", "Ring", "Morph", "Sex", "Date")] %>%
    .[order(.[,"Ring"]),]

  pruned <- lapply(unique(morphs[["Ring"]]), function(x) {
    temp <- dplyr::filter(morphs,
                          Ring == x,
                          Morph != "grey",
                          Morph != 'white')[, c("Ring", "Territory", "Year", "Morph", "Sex")] %>%
      unique.data.frame()

    if (nrow(temp) == 1) temp <- data.frame()
    if (nrow(temp) > 1) {
      temp <- dplyr::left_join(temp, morphs,  by = c("Ring", "Territory", "Year", "Morph", "Sex"))
    }
    return(temp)
  }) %>%
    do.call("rbind",.)
  sink(output, append = T)
  cat("\n\n")
  cat("6. Changes in assigned Morphs or Sex:\n")
  cat("\t[i.e. scoring differs between sampling points]\n")
  cat("=======================================================================================")
  cat("\n")
  print(pruned, row.names = F, right = F)
  cat("=======================================================================================")
  sink()
  ## -------------------------------------------------------------------------

  ## Unknown broods
  ## -------------------------------------------------------------------------
  ## ringing entries
  rings <- dplyr::filter(buzzard_db[["ring_db"]], Age == "juvenile") %>%
    .[order(.[["Ring"]]),c("Ring", "Territory", "Year", "Brood_ID", "Terr_Year")]
  rings[["Brood_ID"]] <- as.character(rings[["Brood_ID"]])

  ## corresponding broods
  broods <-
    buzzard_db[["repro_fledge_db"]][,c("Brood_ID", "Repro", "Fledging")]
  broods[["Brood_ID"]] <- as.character(broods[["Brood_ID"]])


  ## merge
  merged <- dplyr::left_join(rings, broods, by = "Brood_ID") %>%
    dplyr::filter(., is.na(Brood_ID))

  sink(output, append = T)
  cat("\n\n")
  cat("7. Failures to match Brood information to Individual:\n")
  cat("\t[i.e. it is not unambiguous where a chick was raised]\n")
  cat("=======================================================================================")
  cat("\n")
  print(merged, row.names = F, right = F)
  cat("=======================================================================================")
  sink()
  ## -------------------------------------------------------------------------

  ## Check that Repro <= Number of ringed
  ## -------------------------------------------------------------------------
  Terr <- unique(buzzard_db[["ring_db"]][["Territory"]])

  out <- lapply(Terr, function(x) {
    # for every Territory
    df1 <- dplyr::filter(buzzard_db[["ring_db"]],
                        Territory == x)
    Year <- unique(df1[["Year"]])
    # for every year with data
    out <- lapply(Year, function(y) {
      df2 <- dplyr::filter(df1, Year == y)
      return(data.frame(Year = y,
             Rings = length(unique(df2[["Ring"]]))))
    }) %>%
      do.call("rbind",.)
    out[["Territory"]] <- x
    return(out)
  }) %>%
    do.call("rbind",.) %>%
    dplyr::left_join(.,buzzard_db[["repro_fledge_db"]][,c("Territory", "Year", "Repro")],
                     by = c("Territory","Year"))

  pruned <- dplyr::filter(out, Rings != Repro)
  pruned <- pruned[, c("Territory", "Year", "Repro", "Rings")]

  sink(output, append = T)
  cat("\n\n")
  cat("9. Repro != Ring number. Please check:\n")
  cat("=======================================================================================")
  cat("\n")
  print(pruned, row.names = F, right = F)
  cat("=======================================================================================")
  sink()
  ## -------------------------------------------------------------------------


  ## Unexpected Territory size:  > 2 km between Nest and Centroid
  ## -------------------------------------------------------------------------
  territory <- unique(buzzard_db[["repro_fledge_db"]][["Territory"]])
  out <- lapply(territory, function(focal.terr) {
    ## subset the data
    df <- dplyr::filter(buzzard_db[["repro_fledge_db"]],
                        Territory == focal.terr,
                        !is.na(N),
                        !is.na(E))
    data.frame(Territory = df[["Territory"]],
               Year = df[["Year"]],
               Nest = df[["Nest"]],
               Lat = df[["N"]],
               Long = df[["E"]],
               Mean.N = df[["Mean.Y"]],
               Mean.E = df[["Mean.X"]],
               Dist2Centroid = DBChecks::dist2centroid(df))

  }) %>%
    do.call("rbind",.) %>%
    dplyr::filter(., Dist2Centroid > 1.5)

  sink(output, append = T)
  cat("\n\n")
  cat("8. Distance between Nest and Territory center > 1.5 km:\n")
  cat("=======================================================================================")
  cat("\n")
  print(out, row.names = F, right = F)
  cat("=======================================================================================")
  sink()
  ## -------------------------------------------------------------------------

  ## 6. Missing nests
  ## -------------------------------------------------------------------------
  missing.nests <- dplyr::filter(buzzard_db[["repro_fledge_db"]],
                                 is.na(Nest))
  sink(output, append = T)
  cat("\n\n")
  cat("9. Missing Nests (i.e. no GPX available):\n")
  cat("=======================================================================================")
  cat("\n")
  print(missing.nests[, c("Territory", "Year", "Repro")], row.names = F, right = F, max = 999)
  cat("=======================================================================================")
  sink()

  ## Check if values are plausible
  ## -------------------------------------------------------------------------
  repro.tests <- data.frame(
    Terr_pro = dplyr::filter(buzzard_db[["repro_fledge_db"]],
                             !is.na(Terr_pro)) %>%
      with(., any(Terr_pro > 1 || Terr_pro < 0)),
    Hatch = dplyr::filter(buzzard_db[["repro_fledge_db"]],
                          !is.na(Hatch)) %>%
      with(., any(is.na(Fledging)))
  )

  if (any(isFALSE(repro.tests))) {
    sink(output, append = T)
    cat("\n\n")
    cat("Plausability of values in Repro & Fledge")
    cat("=======================================================================================")
    cat("\n")
    print(tests, row.names = F, right = F, max.print = 999)
    cat("=======================================================================================")
    sink()
  }
  ## -------------------------------------------------------------------------

    ## -------------------------------------------------------------------------
  ## -------------------------------------------------------------------------
  ## -------------------------------------------------------------------------
  ## -------------------------------------------------------------------------
  ## -------------------------------------------------------------------------


  ##

}# end check.buzzard_db
#load("../../01-PhD/00-Raw/RData/buzzard_db.RData")
