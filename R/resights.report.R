#' Report of resightings
#'
#' @param digits number of decimals
#' @param lang language
#' @param brood Default False. If True, returns resights of siblings
#' @inheritParams birdinfo
#' @import rtf
#' @import magrittr
#' @export
#'
resight.report <- function(x = NULL, buzzard_db = "RData/buzzard_db.RData", out.file = NULL, digits = 2, lang = c("German", "English"), brood = FALSE) {
  #options(width = 1e4, digits = 2)
  ## checks
  ## ======================================================================================================================================
  lang = match.arg(lang)
  if (!file.exists(buzzard_db)) stop(paste(buzzard_db, "does not exist"))
  load(buzzard_db)
  if (!x %in% buzzard_db[["ring_db"]][["Ring"]]) stop(paste(x), "does not exists")
  print("Working on report ...")

  if (is.null(out.file)) out.file <- paste0("resights/Buteo_buteo_", x, "_", Sys.Date(),".rtf")

  ring <- dplyr::filter(buzzard_db[["ring_db"]], Ring == x)[,]
  ring[["Brood_ID"]] <- as.character(ring[["Brood_ID"]][1])
  repro <- dplyr::filter(buzzard_db[["repro_fledge_db"]],
                         Brood_ID %in% ring[["Brood_ID"]][!is.na(ring[["Brood_ID"]])])
  resights <- dplyr::filter(buzzard_db[["resights"]], Ring == x)

  ring[["Sex"]][is.na(ring[["Sex"]])] <- ring[["Sex_guess"]][is.na(ring[["Sex"]])]

  if (!is.na(ring[["Sex"]])) {
    if (lang == "German") {
      ring[["Sex"]] <- ifelse(ring[["Sex"]] == 1, "Männchen", "Weibchen")
    } else {
      ring[["Sex"]] <- ifelse(ring[["Sex"]] == 1, "Male", "Female")
    }

  }

  if (lang == "German") {
    for (i in 1:nrow(ring)) {
      if (ring[["Morph"]][i] == "g2") ring[["Morph"]][i] <- "Dunkel (2)"
      if (ring[["Morph"]][i] == "g2+") ring[["Morph"]][i] <- "Dunkel (2+)"
      if (ring[["Morph"]][i] == "g2-") ring[["Morph"]][i] <- "Dunkel (2-)"
      if (ring[["Morph"]][i] == "g2?") ring[["Morph"]][i] <- "Dunkel (2)"
      if (ring[["Morph"]][i] == 2) ring[["Morph"]][i] <- "Dunkel (2)"
      if (ring[["Morph"]][i] == "2+") ring[["Morph"]][i] <- "Dunkel (2+)"
      if (ring[["Morph"]][i] == 3) ring[["Morph"]][i] <- "Intermediär (3)"
      if (ring[["Morph"]][i] == "3+") ring[["Morph"]][i] <- "Intermediär (3+)"
      if (ring[["Morph"]][i] == "3-") ring[["Morph"]][i] <- "Intermediär (3-)"
      if (ring[["Morph"]][i] == 4) ring[["Morph"]][i] <- "Hell (4)"
      if (ring[["Morph"]][i] == "4+") ring[["Morph"]][i] <- "Hell (4+)"
      if (ring[["Morph"]][i] == "4-") ring[["Morph"]][i] <- "Hell (4-)"
      if (ring[["Morph"]][i] == "g3") ring[["Morph"]][i] <- "Intermediär (3)"
      if (ring[["Morph"]][i] == "g3+") ring[["Morph"]][i] <- "Intermediär (3+)"
      if (ring[["Morph"]][i] == "w3+") ring[["Morph"]][i] <- "Intermediär (3+)"
      if (ring[["Morph"]][i] == "w3") ring[["Morph"]][i] <- "Intermediär (3)"
      if (ring[["Morph"]][i] == "g3-") ring[["Morph"]][i] <- "Intermediär (3-)"
      if (ring[["Morph"]][i] == "w4") ring[["Morph"]][i] <- "Hell (4)"
      if (ring[["Morph"]][i] == "w4-") ring[["Morph"]][i] <- "Hell (4-)"
      if (ring[["Morph"]][i] == "w4+") ring[["Morph"]][i] <- "Hell (4+)"
    }
    } else {
      for (i in 1:nrow(ring)) {
        if (ring[["Morph"]][i] == "g2") ring[["Morph"]][i] <- "Dark (2)"
        if (ring[["Morph"]][i] == "g2+") ring[["Morph"]][i] <- "Dark (2+)"
        if (ring[["Morph"]][i] == "g2-") ring[["Morph"]][i] <- "Dark (2-)"
        if (ring[["Morph"]][i] == 2) ring[["Morph"]][i] <- "Dark (2)"
        if (ring[["Morph"]][i] == "2+") ring[["Morph"]][i] <- "Dark (2+)"
        if (ring[["Morph"]][i] == 3) ring[["Morph"]][i] <- "Intermediate (3)"
        if (ring[["Morph"]][i] == "3+") ring[["Morph"]][i] <- "Intermediate (3+)"
        if (ring[["Morph"]][i] == "3-") ring[["Morph"]][i] <- "Intermediate (3-)"
        if (ring[["Morph"]][i] == 4) ring[["Morph"]][i] <- "Light (4)"
        if (ring[["Morph"]][i] == "4+") ring[["Morph"]][i] <- "Light (4+)"
        if (ring[["Morph"]][i] == "4-") ring[["Morph"]][i] <- "Light (4-)"
        if (ring[["Morph"]][i] == "g3") ring[["Morph"]][i] <- "Intermediate (3)"
        if (ring[["Morph"]][i] == "g3+") ring[["Morph"]][i] <- "Intermediate (3+)"
        if (ring[["Morph"]][i] == "w3+") ring[["Morph"]][i] <- "Intermediate (3+)"
        if (ring[["Morph"]][i] == "w3") ring[["Morph"]][i] <- "Intermediate (3)"
        if (ring[["Morph"]][i] == "g3-") ring[["Morph"]][i] <- "Intermediate (3-)"
        if (ring[["Morph"]][i] == "w4") ring[["Morph"]][i] <- "Light (4)"
        if (ring[["Morph"]][i] == "w4-") ring[["Morph"]][i] <- "Light (4-)"
        if (ring[["Morph"]][i] == "w4+") ring[["Morph"]][i] <- "Light (4+)"
      }
    }

  ring <- ring[order(ring[["Date"]], decreasing = F),]

  ring[["ID"]] <-
    stringr::str_replace(ring[["ID"]], "Metal right,", "") %>%
    stringr::str_replace(., "Metal left,", "") %>%
    stringr::str_replace(., "wingtag", "")

  if (lang == "German") {
    ring[["ID"]] <-
      stringr::str_replace(ring[["ID"]], "yellow", "Gelb") %>%
      stringr::str_replace(., "white", "Weiss") %>%
      stringr::str_replace(., "blue", "Blau")
  }


  ## set up rtf document
  rtf <- rtf::RTF(file = out.file, height = 8.27, width = 11.69, omi = c(0.5,.3,.3,0.5), font.size = 10)
  rtf::addPng.RTF(rtf, system.file("extdata", "logo.png", package = "DBChecks"), width = 4, height = 1.19)
  rtf::addNewLine(rtf)
  if (lang == "German") {
    rtf::addHeader(rtf,title = "Ablesungen von Flügelmarken Mäusebussard und Rotmilan")
  } else {
    rtf::addHeader(rtf,title = "Resightings of wing-tagged Common Buzzards and Red Kites")
  }
  rtf::startParagraph(rtf)
  if (lang == "English") {
    rtf::addText(rtf, "Many thanks for submitting resightings of wing-tagged individuals from our project.")
    rtf::addText(rtf, "Below you find ringing details and a list of previous resightings of the individual you observed. If you spot any mistakes, please let us know (bussarde@uni-bielefeld.de).\n\n")
    rtf::addText(rtf, "These data are intended for your personal information only and may not be shared without prior confirmation.", italic = T)
  } else {
    rtf::addText(rtf, "Wir bedanken uns herzlichst für die Mitteilung von markierten Greifvögeln aus unserem Projekt. ")
    rtf::addText(rtf, "Nachfolgend finden Sie Informationen über die Beringung und bisheringen Wiederfunde. Sollten Sie Fehler darin finden teilen Sie diese bitte mit (bussarde@uni-bielefeld.de).\n\n")
    rtf::addText(rtf, "Die Beringungs- und Wiederfunddaten sind ausschließlich als persönliche Information für die Melder bestimmt und dürfen ohne vorherige Absprache nicht weitergegeben werden.", italic = T)

  }
  rtf::addNewLine(rtf)
  rtf::endParagraph(rtf)

  ## Check species here!

  rtf::startParagraph(rtf)
  if (lang == "German") {
    rtf::addText(rtf, "Mäusebussard\t", bold = T)
    rtf::addText(rtf, "Buteo buteo", italic = T)
    rtf::addNewLine(rtf)
    rtf::addText(rtf, "Ring-Nr.: ")
    rtf::addText(rtf, "Helgoland ", x, "\t", bold = T)
    rtf::addText(rtf, "Flügelmarke: ")
    rtf::addText(rtf, ring[["ID"]][nrow(ring)], "\t", bold = T)
    rtf::addText(rtf, "Alter: ")
    rtf::addText(rtf, ifelse(ring[["Age"]] == "juvenile", "Nicht-flügge", "Adult"), bold = T)
  } else {
    rtf::addText(rtf, "Common Buzzard\t", bold = T)
    rtf::addText(rtf, "Buteo buteo", italic = T)
    rtf::addNewLine(rtf)
    rtf::addText(rtf, "Ring-No.: ")
    rtf::addText(rtf, "Helgoland ", x, "\t", bold = T)
    rtf::addText(rtf, "Wing-tag: ")
    rtf::addText(rtf, ring[["ID"]][nrow(ring)], "\t", bold = T)
    rtf::addText(rtf, "Age: ")
    rtf::addText(rtf, ifelse(ring[["Age"]] == "juvenile", "Nestling", "Adult"), bold = T)

  }
  rtf::addNewLine(rtf)
  rtf::endParagraph(rtf)

  rtf::startParagraph(rtf)
  if (lang == "German") {
    rtf::addText(rtf, "Beringungsdaten:", bold = T)
  } else {
    rtf::addText(rtf, "Ringing data:", bold = T)
  }

  rtf::addNewLine(rtf)
  ring <- dplyr::left_join(ring, repro[,c("Brood_ID", "N", "E")], by = "Brood_ID")
  ring <-
    cbind(ring,
          lapply(1:nrow(ring), function(x) {
            DBChecks::PlaceInfo(lat = ring[['N']][x], long = ring[['E']][x])
          }) %>%
            do.call("rbind",.))

  ring <- ring[,c("Date", "N", "E", "Territory", "State", "District", "Municipality", "Brood_Size", "Rank", "Morph", "Sex")]
  if (lang == "German") {
    names(ring) <- c("Datum", "Lat", "Long", "Revier", "Bundesland", "Kreis/Kreisfreie Stadt", "Gemeinde", "Brutgröße", "Brutrang", "Morphe", "Geschlecht")
  } else {
    names(ring) <- c("Date", "Lat", "Long", "Territory", "Federal state", "District", "Municipality", "Brood size", "Brood rank", "Morph", "Sex")
  }

  ring[["Lat"]] <- round(ring[["Lat"]], digits)
  ring[["Long"]] <- round(ring[["Long"]], digits)
  rtf::addTable(rtf, ring[1,], row.names = F)
  rtf::addNewLine(rtf)
  rtf::endParagraph(rtf)

  resights <- resights[,c("Date", "lat", "long", "Observer", "Dist2Nest", "Dir2Nest")]
  resights[["Observer"]][is.na(resights[["Observer"]])] <- ""

  names(resights) <- c("Date", "Lat", "Long", "Observer", "Distance_from_Nest", "Direction_from_Nest")

  resights <-
    cbind(resights,
          lapply(1:nrow(resights), function(x) {
            DBChecks::PlaceInfo(lat = resights[['Lat']][x], long = resights[['Long']][x])
          }) %>%
            do.call("rbind",.))
  resights <- resights[, c("Date", "Lat", "Long", "Country", "State", "District", "Municipality", "Observer", "Distance_from_Nest")]
  resights[["Lat"]] <- round(resights[["Lat"]], digits)
  resights[["Long"]] <- round(resights[["Long"]], digits)
  resights[["Distance_from_Nest"]] <- round(resights[["Distance_from_Nest"]], digits)

  if (lang == "German") {
    names(resights) <- c("Datum", "Lat", "Long", "Land", "Bundesland", "Kreis/Kreisfreie Stadt", "Gemeinde", "Melder", "Entfernung zum Geburtsort [km]")
  } else {
    names(resights) <- c("Date", "Lat", "Long", "Country", "State/Province", "District", "Municipality", "Observer", "Distance to place of birth [km]")
  }


  rtf::startParagraph(rtf)
  if (lang == "German") {
    rtf::addText(rtf, "Wiederfunde:", bold = T)
  } else {
    rtf::addText(rtf, "Resightings:", bold = T)
  }
  rtf::addNewLine(rtf)
  rtf::addTable(rtf, resights, row.names = FALSE)
  rtf::addNewLine(rtf)
  rtf::endParagraph(rtf)

  if (lang == "German") {
    if (ring[["Geschlecht"]][1] %in% c("Männchen", "Male")) {
      repro <- dplyr::filter(buzzard_db[["repro_fledge_db"]],
                             Male_ID == paste0("R", x))

    } else if (ring[["Geschlecht"]][1] %in% c("Weibchen", "Female")) {
      repro <- dplyr::filter(buzzard_db[["repro_fledge_db"]],
                             Fem_ID == paste0("R", x))
    } else {
      repro <- data.frame()
    }

  } else {
    if (ring[["Sex"]][1] %in% c("Männchen", "Male")) {
      repro <- dplyr::filter(buzzard_db[["repro_fledge_db"]],
                             Male_ID == paste0("R", x))

    } else if (ring[["Sex"]][1] %in% c("Weibchen", "Female")) {
      repro <- dplyr::filter(buzzard_db[["repro_fledge_db"]],
                             Fem_ID == paste0("R", x))
    } else {
      repro <- data.frame()
    }

  }


  if (nrow(repro) >= 1) {
    rtf::startParagraph(rtf)
    if (lang == "German") {
      rtf::addText(rtf, "Brutversuche im Untersuchungsgebiet:", bold = T)
    } else {
      rtf::addText(rtf, "Breeding attempts within our study area:", bold = T)

    }
    rtf::addNewLine(rtf)
    repro <- repro[order(repro[["Year"]], decreasing = F),]

    repro <- repro[,c("Year", "Territory", "N", "E", "Fem_ID", "Male_ID", "Repro")]
    if (lang == "German") {
    names(repro) <- c("Jahr", "Revier", "Lat", "Long", "Weibchen", "Männchen", "Flügge Junge")
    } else {
      names(repro) <- c("Year", "Territory", "Lat", "Long", "Female ID", "Male ID", "Fledglings")
    }
    repro[["Lat"]] <- round(repro[["Lat"]], digits)
    repro[["Long"]] <- round(repro[["Long"]], digits)
    rtf::addTable(rtf, repro, row.names = F)
    rtf::endParagraph(rtf)
  }
  rtf::done(rtf)
  rm(buzzard_db)
  print("done")
}

