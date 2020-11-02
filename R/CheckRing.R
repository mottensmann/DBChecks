#' Check the format of a RING database
#'
#' @param Lookup access database with variable definitions
#' @param DNS1 RING Database used as standard
#' @param DNS2 RING Database to check
#' @import RODBC magrittr
#' @export
#'
CheckRing <- function(DNS1 = "Ring2017", DNS2 = "Ring2018", Lookup = "Lookup") {

  ## connect to database
  ch1 <- RODBC::odbcConnect(DNS1)
  cat('Connect to DNS2 ... \n')

  ch2 <- RODBC::odbcConnect(DNS2)
  cat('Connect to DNS2 ... \n')

  ## retrieve data
  tblRinging.ref <- sqlFetch(ch1, "tblRinging", as.is = T)
  tblRefer.ref <- sqlFetch(ch1, "tblRefer", as.is = T)
  tblGeoTab.ref <- sqlFetch(ch1, "tblGeoTab", as.is = T)
  tblOpen.ref <- sqlFetch(ch1, "tblOpen", as.is = T)

  tblRinging.new <- sqlFetch(ch2, "tblRinging", as.is = T)
  tblRefer.new <- sqlFetch(ch2, "tblRefer", as.is = T)
  tblGeoTab.new <- sqlFetch(ch2, "tblGeoTab", as.is = T)
  tblOpen.new <- sqlFetch(ch2, "tblOpen", as.is = T)

  ## 1. check all fields have the same names
  if (any(names(tblRinging.new) == names(tblRinging.ref) %>% isFALSE)) {
    warning("Colum names of tblRinging differ")
  }
  if (any(names(tblRefer.new) == names(tblRefer.ref) %>% isFALSE)) {
    warning("Colum names of tblRefer differ")
  }
  if (any(names(tblGeoTab.new) == names(tblGeoTab.ref) %>% isFALSE)) {
    warning("Colum names of tblGeoTab differ")
  }
  if (any(names(tblOpen.new) == names(tblOpen.ref) %>% isFALSE)) {
    warning("Colum names of tblOpen differ")
  }

  ## 2. Check all fields are of the same class
  if (any(apply(tblRinging.new,2, class) == apply(tblRinging.ref,2, class) %>% isFALSE)) {
    warning("Colum names of tblRinging differ")
  }
  if (any(apply(tblRefer.new,2, class) == apply(tblRefer.ref,2, class) %>% isFALSE)) {
    warning("Colum names of tblRefer differ")
  }
  if (any(apply(tblOpen.new,2, class) == apply(tblOpen.ref,2, class) %>% isFALSE)) {
    warning("Colum names of tblOpen differ")
  }
  if (any(apply(tblGeoTab.new,2, class) == apply(tblGeoTab.ref,2, class) %>% isFALSE)) {
    warning("Colum names of tblGeotab differ")
  }

  ## 3. Check fields
  ## 1
  t1 <- c(is.character(tblRinging.new[["strRingScheme"]]),
             !any(tblRinging.new[["strRingScheme"]] %>% as.character() %>% na.omit() %>% nchar() != 3))
  if (any(t1 == FALSE)) {
    warning("strRingScheme is wrong")
  }
  ## 2
  t2 <- c(is.character(tblRinging.new[["strRingNr"]]),
             !any(tblRinging.new[["strRingNr"]] %>% as.character() %>% na.omit() %>% nchar() != 10))
  if (any(t2 == FALSE)) {
    warning("strRingNr is wrong")
  }
   ## 3
  t3 <- c(is.character(tblRinging.new[["strSpecies"]]),
             !any(tblRinging.new[["strSpecies"]] %>% as.character() %>% na.omit() %>% nchar() != 5))
  if (any(t3 == FALSE)) {
    warning("strSpecies is wrong")
  }
  ## 4
  t4 <- c(is.character(tblRinging.new[["strRecoveryChances"]]),
             !any(tblRinging.new[["strRecoveryChances"]] %>% as.character() %>% na.omit() %>% nchar() != 1))
  if (any(t4 == FALSE)) {
    warning("strRecoveryChances is wrong")
  }
  ## 5
  t5 <- c(is.character(tblRinging.new[["strSex"]]),
             !any(tblRinging.new[["strSex"]] %>% as.character() %>% na.omit() %>% nchar() != 1))
  if (any(t5 == FALSE)) {
    warning("strSex is wrong")
  }
  ## 6
  t6 <- c(is.character(tblRinging.new[["strAge"]]),
             !any(tblRinging.new[["strAge"]] %>% as.character() %>% na.omit() %>% nchar() != 1))
  if (any(t6 == FALSE)) {
    warning("strAge is wrong")
  }
  ## 7
  t7 <- c(is.character(tblRinging.new[["strStatusBroodsize"]]),
             !any(tblRinging.new[["strStatusBroodsize"]] %>% na.omit() %>% nchar() != 1))
  if (any(t7 == FALSE)) {
    warning("strStatusBroodsize is wrong")
  }
  ## 8
  if (!is.integer(tblRinging.new[["lngPullusAge"]])) {
    warning("lngPullusAge is wrong")
  }
  ## 9
  if (!is.integer(tblRinging.new[["lngAccPullusAge"]])) {
    warning("lngAccPullusAge is wrong")
  }
  ## 10
  # if (class(tblRinging.new[["dtmDate"]])[1] != "POSIXct") {
  #   warning("dtmDate is wrong")
  # }
  ## 11
  t11 <- c(is.character(tblRinging.new[["strAccuracyDate"]]),
             !any(tblRinging.new[["strAccuracyDate"]] %>% as.character() %>% na.omit() %>% nchar() != 1))
  if (any(t11 == FALSE)) {
    warning("strAccuracyDate is wrong")
  }
  ## 12
  t12 <- c(is.character(tblRinging.new[["strPlaceCode"]]),
             !any(tblRinging.new[["strPlaceCode"]] %>% as.character() %>% na.omit() %>% nchar() != 4))
  if (any(t12 == FALSE)) {
    warning("strPlaceCode is wrong")
  }
  ## 13
  if (!is.integer(tblRinging.new[["idGeoTab"]])) {
    warning("idGeoTab is wrong")
  }
  ## 14
  t14 <- c(is.character(tblRinging.new[["strAccuracyPlace"]]),
             !any(tblRinging.new[["strAccuracyPlace"]] %>% as.character() %>% na.omit() %>% nchar() != 1))
  if (any(t14 == FALSE)) {
    warning("strAccuracyPlace is wrong")
  }
  # 15
  t15 <- c(is.character(tblRinging.new[["strTK25"]]),
             !any(tblRinging.new[["strTK25"]] %>% as.character() %>% na.omit() %>% nchar() != 4))
  if (any(t15 == FALSE)) {
    warning("strTK25 is wrong")
  }
  # 16
  t16 <- c(is.character(tblRinging.new[["strRingerNr"]]),
             !any(tblRinging.new[["strRingerNr"]] %>% as.character() %>% na.omit() %>% nchar() != 4))
  if (any(t16 == FALSE)) {
    warning("strRingerNr is wrong")
  }
  # 17
  t17 <- c(is.character(tblRinging.new[["strCatchingMethods"]]),
             !any(tblRinging.new[["strCatchingMethods"]] %>% as.character() %>% na.omit() %>% nchar() != 1))
  if (any(t17 == FALSE)) {
    warning("strCatchingMethods is wrong")
  }
  # 18
  t17 <- c(is.character(tblRinging.new[["strCatchingLures"]]),
             !any(tblRinging.new[["strCatchingLures"]] %>% as.character() %>% na.omit() %>% nchar() != 1))
  if (any(t17 == FALSE)) {
    warning("strCatchingLures is wrong")
  }
  # 19
  if (!is.numeric(tblRinging.new[["lngWingLength"]])) {
    warning("lngWingLength is wrong")
  }
  # 20
  if (!is.numeric(tblRinging.new[["lngLengthP8"]])) {
    warning("lngLengthP8 is wrong")
  }
  # 21
  if (!is.numeric(tblRinging.new[["lngWeight"]])) {
    warning("lngWeight is wrong")
  }
  # 22
  if (!is.numeric(tblRinging.new[["lngTarsus"]])) {
    warning("lngTarsus is wrong")
  }
  # 23
  # if (class(tblRinging.new[["dtmLastUpdateDate"]])[1] != "POSIXct") {
  #   warning("dtmLastUpdateDate is wrong")
  # }
  # 24
  t24 <- c(is.character(tblRinging.new[["strRemarks"]]),
             !any(tblRinging.new[["strRemarks"]] %>% as.character() %>% na.omit() %>% nchar() > 100))
  if (any(t24 == FALSE)) {
    warning("strRemarks is wrong")
  }
  # 27
  if (!is.numeric(tblRinging.new[["lngLong"]])) {
    warning("lngLong is wrong")
  }
  # 28
  if (!is.numeric(tblRinging.new[["lngLat"]])) {
    warning("lngLat is wrong")
  }

  ## open lookup

  ch3 <- odbcConnect('Lookup')

    ## Tables of the DNS
  lookup <- sqlTables(ch3, tableType = "TABLE")[["TABLE_NAME"]] %>%
    lapply(., function(x) sqlFetch(ch3, as.character(x), as.is = T)) %>%
    set_names(., sqlTables(ch3, tableType = "TABLE")[["TABLE_NAME"]])


  ## Check that if brood size was given, all all juveniles

  ## summarise exported data
  print("Broodsize by species")

  ## Brood size
  out <- table(tblRinging.new[["strSpecies"]], tblRinging.new[["strStatusBroodsize"]])
  rownames(out) <-
    lookup[["TLKPSPECIES"]][["STRSPECIESLAT"]][lookup[["TLKPSPECIES"]][["STRCODE"]] %in% rownames(out)]
  colnames(out) <-
    lookup[["TLKPSTATUSBROODSIZE"]] [["STRTEXT"]][lookup[["TLKPSTATUSBROODSIZE"]][["STRCODE"]] %in% colnames(out)]
  print(out)

  ## Recovery chances
  out <- table(tblRinging.new[["strSpecies"]], tblRinging.new[["strRecoveryChances"]])
  rownames(out) <-
    lookup[["TLKPSPECIES"]][["STRSPECIESLAT"]][lookup[["TLKPSPECIES"]][["STRCODE"]] %in% rownames(out)]
  colnames(out) <-
    lookup[["TLKPRECOVERYCHANCES"]][["STRTEXT"]][lookup[["TLKPRECOVERYCHANCES"]][["STRCODE"]] %in% colnames(out)]
  print(out)

  ## Age
  out <- table(tblRinging.new[["strSpecies"]], tblRinging.new[["strAge"]])
  rownames(out) <-
    lookup[["TLKPSPECIES"]][["STRSPECIESLAT"]][lookup[["TLKPSPECIES"]][["STRCODE"]] %in% rownames(out)]
  colnames(out) <-
    lookup[["TLKPAGE"]][["STRTEXT"]][lookup[["TLKPAGE"]][["STRCODE"]] %in% colnames(out)]
  print(out)

  ## Catching Methods
  out <- table(tblRinging.new[["strSpecies"]], tblRinging.new[["strCatchingMethods"]])
  rownames(out) <-
    lookup[["TLKPSPECIES"]][["STRSPECIESLAT"]][lookup[["TLKPSPECIES"]][["STRCODE"]] %in% rownames(out)]
  colnames(out) <-
    lookup[["TLKPCATCHINGMETHODS"]][["STRTEXT"]][lookup[["TLKPCATCHINGMETHODS"]][["STRCODE"]] %in% colnames(out)]
  print(out)

  ## Catching lures
  out <- table(tblRinging.new[["strSpecies"]], tblRinging.new[["strCatchingLures"]])
  rownames(out) <-
    lookup[["TLKPSPECIES"]][["STRSPECIESLAT"]][lookup[["TLKPSPECIES"]][["STRCODE"]] %in% rownames(out)]
  colnames(out) <-
    lookup[["TLKPCATCHINGLURES"]][["STRTEXT"]][lookup[["TLKPCATCHINGLURES"]][["STRCODE"]] %in% colnames(out)]
  print(out)

  ## overview of rings
  x <- tblRinging.new[["strRingNr"]]
  x <- gsub(pattern = ".", replacement = "", x =  x, fixed = TRUE)
  x <- gsub(pattern = "J", replacement = "9999999999", x =  x, fixed = TRUE)
  x <- gsub(pattern = "A", replacement = "8888888888", x =  x, fixed = TRUE)

  x <- unique(x)
  x <- as.numeric(x)
  x <- sort(x)
  breaks <- which(diff(x) != 1)
  n <- c(1, (breaks + 1))

  out <- data.frame(Series = (1:(length(breaks) + 1)),
                    FirstRing = x[n],
                    LastRing = x[c(breaks, length(x))])
  out[["Number"]] <- (out[["LastRing"]] - out[["FirstRing"]]) + 1
  out[["FirstRing"]] <- gsub(pattern = "9999999999", replacement = "J", out[["FirstRing"]])
  out[["LastRing"]] <- gsub(pattern = "9999999999", replacement = "J", out[["LastRing"]])
  out[["FirstRing"]] <- gsub(pattern = "8888888888", replacement = "A", out[["FirstRing"]])
  out[["LastRing"]] <- gsub(pattern = "8888888888", replacement = "A", out[["LastRing"]])
  cat("\nOverview ring series:\n\n")
  print(out)


  close(ch1)
  close(ch2)
  close(ch3)
}

