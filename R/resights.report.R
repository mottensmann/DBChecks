#' Report of resightings
#'
#' @param digits number of decimals
#' @inheritParams birdinfo
#' @import rtf
#' @import magrittr
#' @export
#'
resight.report <- function(x = NULL, buzzard_db = "RData/buzzard_db.RData", out.file = NULL, digits = 2) {
  #options(width = 1e4, digits = 2)
  ## checks
  ## ======================================================================================================================================
  if (!file.exists(buzzard_db)) stop(paste(buzzard_db, "does not exist"))
  load(buzzard_db)
  if (!x %in% buzzard_db[["ring_db"]][["Ring"]]) stop(paste(x), "does not exists")
  print("Working on report ...")

  if (is.null(out.file)) out.file <- paste0("resights/Buteo_buteo_", x, "_", Sys.Date(),".doc")

  ring <- dplyr::filter(buzzard_db[["ring_db"]], Ring == x)[,]
  ring[["Brood_ID"]] <- as.character(ring[["Brood_ID"]][1])
  repro <- dplyr::filter(buzzard_db[["repro_fledge_db"]],
                         Brood_ID %in% ring[["Brood_ID"]][!is.na(ring[["Brood_ID"]])])
  resights <- dplyr::filter(buzzard_db[["resights"]], Ring == x)

  ring[["Sex"]][is.na(ring[["Sex"]])] <- ring[["Sex_guess"]][is.na(ring[["Sex"]])]

  if (!is.na(ring[["Sex"]])) {
    ring[["Sex"]] <- ifelse(ring[["Sex"]] == 1, "Männchen", "Weibchen")
  }

  for (i in 1:nrow(ring)) {
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

  ring <- ring[order(ring[["Date"]], decreasing = F),]

  ring[["ID"]] <-
    stringr::str_replace(ring[["ID"]], "Metal right,", "") %>%
    stringr::str_replace(., "Metal left,", "") %>%
    stringr::str_replace(., "yellow", "Gelb") %>%
    stringr::str_replace(., "white", "Weiss") %>%
    stringr::str_replace(., "blue", "Blau") %>%
    stringr::str_replace(., "wingtag", "")


  ## set up rtf document
  rtf <- rtf::RTF(file = out.file, height = 8.27, width = 11.69, omi = c(0.5,.3,.3,0.5), font.size = 10)
  rtf::addPng.RTF(rtf, system.file("extdata", "logo.png", package = "DBChecks"), width = 4, height = 1.19)
  rtf::addNewLine(rtf)
  rtf::addHeader(rtf,title = "Ablesungen von Flügelmarken Mäusebussard und Rotmilan")
  rtf::startParagraph(rtf)
  rtf::addText(rtf, "Wir bedanken uns herzlichst für die Mitteilung von markierten Greifvögeln aus unserem Projekt. ")
  rtf::addText(rtf, "Nachfolgend finden Sie Informationen über die Beringung und bisheringen Wiederfunde. Sollten Sie Fehler darin finden teilen Sie diese bitte mit (bussarde@uni-bielefeld.de).\n\n")
  rtf::addText(rtf, "Die Beringungs- und Wiederfunddaten sind ausschließlich als persönliche Information für die Melder bestimmt und dürfen ohne vorherige Absprache nicht weitergegeben werden.", italic = T)
  rtf::addNewLine(rtf)
  rtf::endParagraph(rtf)

  ## Check species here!

  rtf::startParagraph(rtf)
  rtf::addText(rtf, "Mäusebussard\t", bold = T)
  rtf::addText(rtf, "Buteo buteo", italic = T)
  rtf::addNewLine(rtf)
  rtf::addText(rtf, "Ring-Nr.: ")
  rtf::addText(rtf, "Helgoland ", x, "\t", bold = T)
  rtf::addText(rtf, "Flügelmarke: ")
  rtf::addText(rtf, ring[["ID"]][nrow(ring)], "\t", bold = T)
  rtf::addText(rtf, "Alter: ")
  rtf::addText(rtf, ifelse(ring[["Age"]] == "juvenile", "Nicht-flügge", "Adult"), bold = T)
  rtf::addNewLine(rtf)
  rtf::endParagraph(rtf)

  rtf::startParagraph(rtf)
  rtf::addText(rtf, "Beringungsdaten:", bold = T)
  rtf::addNewLine(rtf)
  ring <- dplyr::left_join(ring, repro[,c("Brood_ID", "N", "E")], by = "Brood_ID")
  ring <-
    cbind(ring,
          lapply(1:nrow(ring), function(x) {
            DBChecks::PlaceInfo(lat = ring[['N']][x], long = ring[['E']][x])
          }) %>%
            do.call("rbind",.))

  ring <- ring[,c("Date", "N", "E", "Territory", "State", "District", "Municipality", "Brood_Size", "Rank", "Morph", "Sex")]
  names(ring) <- c("Datum", "Lat", "Long", "Revier", "Land", "Kreis", "Gemeinde", "Brutgröße", "Brutrang", "Morphe", "Geschlecht")
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

  names(resights) <- c("Datum", "Lat", "Long", "Land", "Bundesland", "Kreis", "Gemeinde", "Melder", "Entfernung zum Geburtsort [km]")

  rtf::startParagraph(rtf)
  rtf::addText(rtf, "Wiederfunde:", bold = T)
  rtf::addNewLine(rtf)
  rtf::addTable(rtf, resights, row.names = FALSE)
  rtf::addNewLine(rtf)
  rtf::endParagraph(rtf)

  if (ring[["Geschlecht"]][1] == "Männchen") {
    repro <- dplyr::filter(buzzard_db[["repro_fledge_db"]],
                           Male_ID == paste0("R", x))

  } else if (ring[["Geschlecht"]][1] == "Weibchen") {
    repro <- dplyr::filter(buzzard_db[["repro_fledge_db"]],
                           Fem_ID == paste0("R", x))
  } else {
    repro <- data.frame()
  }

  if (nrow(repro) >= 1) {
    rtf::startParagraph(rtf)
    rtf::addText(rtf, "Brutversuche im Untersuchungsgebiet:", bold = T)
    rtf::addNewLine(rtf)
    repro <- repro[,c("Year", "Territory", "N", "E", "Fem_ID", "Male_ID", "Repro")]
    names(repro) <- c("Jahr", "Revier", "Lat", "Long", "Weibchen", "Männchen", "Flügge Junge")
    repro[["Lat"]] <- round(repro[["Lat"]], digits)
    repro[["Long"]] <- round(repro[["Long"]], digits)
    repro <- repro[order(repro[["Jahr"]], decreasing = F),]
    rtf::addTable(rtf, repro, row.names = F)
    rtf::endParagraph(rtf)
  }
  rtf::done(rtf)
  print("done")
}

