#' Checking buzzard_db
#' @description
#' Checks entries of the ringing list for potential issues
#'
#' @param input path to 'buzzard_db.RData'
#' @param output destination for output
#' @return text file
#' @import magrittr
#' @export
check.buzzard_db <- function(input = "RData/buzzard_db.RData",
                             output = paste0("Checks/buzzard_db-", Sys.Date(),".txt")) {

  ## set up
  ## -------------------------------------------------------------------------
  options(width = 1e4, encoding = "UTF-8")
  # source("R/check.dupl.R")
  load(input)
  sink(file = output, append = F, split = F)
  cat("Checking buzzard_db\t\t", as.character(Sys.time()))
  cat("\n")
  cat("=======================================================================================")
  cat("\n\nbuzzard_db version info:\n")
  print(buzzard_db[["version"]], row.names = F, max.print = 999)
  cat("=======================================================================================",
      "\n\n")
  sink()

  results <- list()
  results$date <- Sys.Date()
  results$version <- buzzard_db[["version"]]
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
  print(pruned[,c("Ring", "ID", "Territory", "Year", "Age")], row.names = F, right = F, max.print = 999)
  cat("=======================================================================================")
  sink()
  results$dupl.rings <- pruned[,c("Ring", "ID", "Territory", "Year", "Age")]
  ## -------------------------------------------------------------------------

  ## 2. Duplicated IDs:
  ## -------------------------------------------------------------------------
  # x <- "Metal right, white wingtag TN"

  buzzard_db$ring_db$ID <- stringr::str_replace(buzzard_db$ring_db$ID, "Metal left", "Metal right")

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
  print(pruned[,c("ID", "Ring", "Territory", "Year", "Dead", "DateDeath")], row.names = F, right = F, max.print = 999)
  cat("=======================================================================================")
  sink()
  results$dupl.tags <- pruned[,c("ID", "Ring", "Territory", "Year", "Dead", "DateDeath")]
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
  print(pruned, row.names = F, right = F, max.print = 999)
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
  print(pruned, row.names = F, right = F, max.print = 999)
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
  print(dupl.nests[order(dupl.nests[["Nest"]]), 2:1], row.names = F, right = F,max.print = 999)
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
  print(missing.coords[, c("Territory", "Year", "Repro")], row.names = F, right = F, max.print = 999)
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
  print(pruned, row.names = F, right = F, max.print = 999)
  cat("=======================================================================================")
  sink()
  ## -------------------------------------------------------------------------

  ## Unknown broods
  ## -------------------------------------------------------------------------
  ## ringing entries
  rings <- dplyr::filter(buzzard_db[["ring_db"]], Age == "juvenile") %>%
    .[order(.[["Ring"]]),c("Ring", "Territory", "Year", "Brood_ID", "Terr_Year", "Terr_ID")]
  rings[["Brood_ID"]] <- as.character(rings[["Brood_ID"]])

  ## corresponding broods
  broods <-
    buzzard_db[["repro_fledge_db"]][,c("Brood_ID", "Terr_ID", "Repro", "Fledging", "Year")]
  broods[["Brood_ID"]] <- as.character(broods[["Brood_ID"]])


  ## merge
  # merged <- dplyr::left_join(rings, broods, by = c("Brood_ID", "Year")) %>%
  #   dplyr::filter(., is.na(Brood_ID))

  ## For each individual, check that there is a known brood
  unknown_broods <- lapply(1:nrow(rings), function(row) {
    if (rings[["Brood_ID"]][row]  %in% broods[["Brood_ID"]]) {

    } else {
      rings[row,c("Ring", "Territory", "Brood_ID", "Year")]
    }
  }) %>%
    do.call("rbind",.)
  unknown_broods <- unknown_broods[order(unknown_broods$Year, decreasing = F),]


  sink(output, append = T)
  cat("\n\n")
  cat("7. Failures to match Brood information to Individual:\n")
  cat("\t[i.e. it is ambiguous where a chick was raised]\n")
  cat("=======================================================================================")
  cat("\n")
  print(unknown_broods, row.names = F, right = F, max.print = 999)
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
  print(pruned, row.names = F, right = F, max.print = 999)
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
  print(out, row.names = F, right = F, max.print = 999)
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
  print(missing.nests[, c("Territory", "Year", "Repro")], row.names = F, right = F, max.print = 999)
  cat("=======================================================================================")
  sink()


  ## 6. Multiple nest per territory within a year
  ## -------------------------------------------------------------------------
  df <- buzzard_db[["ring_db"]][,c("Ring", "Nest", "Year", "Territory")] %>%
    dplyr::left_join(., buzzard_db[["repro_fledge_db"]][, c("Year", "N", "E", "Territory")],
                     by = c("Territory", "Year"))

  ## divide in years
  seasons <- unique(df[["Year"]])
  out <- lapply(seasons, function(season) {
    # subset the data
    data.sub <- dplyr::filter(df, Year == season)
    # divide in territories
    territories <- unique(data.sub[["Territory"]])
    out <- lapply(territories, function(territory) {
      data.sub.sub <- dplyr::filter(data.sub, Territory == territory)
      if (length(unique(data.sub.sub[["Nest"]])) > 1) {
        data.frame(Territory = territory,
                   Year = data.sub.sub[["Year"]][1])
      } else {
        data.frame()
      }
    }) %>% do.call("rbind",.)
    return(out)
  }) %>%
    do.call("rbind",.)
  sink(output, append = T)
  cat("\n\n")
  cat("6. Multiple Nests for the same territory within the same year:\n")
  cat("=======================================================================================")
  cat("\n")
  print(out, row.names = F, right = F, max.print = 9999)
  cat("=======================================================================================")
  sink()

  ## -------------------------------------------------------------------------
  ##

  ## Check if values are plausible
  ## -------------------------------------------------------------------------
  ring.db <- buzzard_db$ring_db
  morphs <- ring.db$Morph %>% as.factor() %>% summary()
  morphs <- data.frame(Morph = names(morphs),
                       Entries = morphs)
  morphs <- morphs[order(morphs$Entries, decreasing = T),]
  ekto <- ring.db$Ekto %>% as.factor() %>% summary()
  ekto <- data.frame(Ekto = names(ekto),
                     Entries = ekto)


  sink(output, append = T)
  cat("\n\n")
  cat("Summary of factor levels Ringing list")
  cat("=======================================================================================")
  cat("\n")
  cat("Morphs:\n")
  print(morphs, row.names = F, right = F, max.print = 999)
  cat("Ekto:\n")
  print(ekto, row.names = F, right = F, max.print = 999)
  cat("=======================================================================================")
  sink()


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
    cat("Plausability of values in Repro & Fledge\n")
    cat("=======================================================================================")
    cat("\n")
    print(tests, row.names = F, right = F, max.print = 999)
    cat("=======================================================================================")
    sink()
  }

  ## no resightings of breeding birds

  missing.res <- check.breeders()
  sink(output, append = T)
  cat("\n\n")
  cat("Breeding individuals without resighting in the breeding season\n")
  cat("=======================================================================================")
  cat("\n")
  print(missing.res, row.names = F, right = F, max.print = 999)
  cat("=======================================================================================")
  sink()

  ## resightings after death
  dead <- dplyr::filter(buzzard_db$ring_db, !is.na(Dead))[["Ring"]] %>%
    unique()
 # dead <- dead[113]
  test <- lapply(dead, function(x) {
    date <- dplyr::filter(buzzard_db$ring_db, Ring == x)[,c("DateDeath", "AccDateDeath")]
    date <- date[!is.na(date$DateDeath),]
    resights <- dplyr::filter(buzzard_db$resights, Ring == x)[["Date"]]
    if (length(resights) > 0) {
      if (any(resights > date$DateDeath)) {
        data.frame(Ring = x, DateDeath = date$DateDeath,
                   AccDateDeath = date$AccDateDeath, Resighted = resights[which(resights > date$DateDeath)])
      } else {
        data.frame()
      }
    } else {
      data.frame()
    }

  }) %>%
    do.call("rbind",.)

  if (nrow(test) > 0) {
    sink(output, append = T)
    cat("\n\n")
    cat("Check plausability of resights for dead encounters\n")
    cat("=======================================================================================")
    cat("\n")
    print(test, row.names = F, right = F, max.print = 999)
    cat("=======================================================================================")
    sink()
  }

  ## Check that death info is present for all entries of an individual

  tmp <- dplyr::filter(buzzard_db$ring_db, !is.na(Dead))
  check <- lapply(unique(tmp$Ring), function(x) {
    tmp2 <- dplyr::filter(buzzard_db$ring_db, Ring == x)
    if (any(is.na(tmp2$Dead))) {
      tmp2[,c("no", "Ring", "Dead", "DateDeath")]
    } else {
      data.frame()
    }
  }) %>%
    do.call("rbind",.)

  if (nrow(check) > 0) {
    sink(output, append = T)
    cat("\n\n")
    cat("Missing death information:\n")
    cat("=======================================================================================")
    cat("\n")
    print(check, row.names = F, right = F, max.print = 999)
    cat("=======================================================================================")
    sink()
  }

  resights <- buzzard_db$resights %>%
    dplyr::filter(., Ring %in% buzzard_db$ring_db$Ring)

  ## question 1: Resight before ringing
  test <- dplyr::filter(resights, DaysSinceRinging < 0)
  if (nrow(test) > 0) {
    sink(output, append = T)
    cat("\n\n")
    cat("ID recorded before ringing:\n")
    cat("=======================================================================================")
    cat("\n")
    print(test, row.names = F, right = F, max.print = 999)
    cat("=======================================================================================")
    sink()
  }

  ### Check for ambiguity in resights
  # ===========================================================================
  test <- lapply(1:nrow(resights), function(case) {
    ## focal case
    df <- resights[case,]
    ## same id
    ref <- dplyr::filter(buzzard_db$ring_db,
                         ID == df$ID,
                         Date <= df$Date)[,c("Ring", "ID", "Territory","DateDeath")] %>%
      unique.data.frame()
    if (any(!is.na(ref$DateDeath))) {
      ref <- dplyr::filter(ref, DateDeath >= df$Date)
    }

    if (length(unique(ref$Ring)) > 1) {
      return(data.frame(df[,c("Ring", "ID", "Date", "Remarks")], Ind_with_same_ID = paste(ref$Ring, collapse = ";")))
    }

  }) %>%
    do.call("rbind",.)

  sink(output, append = T)
  cat("\n\n")
  cat("Ambigious wingtag resightings:\n")
  cat("=======================================================================================")
  cat("\n")
  print(test, row.names = F, right = F, max.print = 999)
  cat("=======================================================================================")
  sink()
  results$ambigious.resights <- test
  return(results)

}# end check.buzzard_db
# rm(list = ls())
# library(magrittr)
# input <- ("../../01-PhD/00-Raw/RData/buzzard_db.RData")
# load(input)
# output = tempfile()
