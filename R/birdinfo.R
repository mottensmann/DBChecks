#' Summarise all data from specific individual
#'
#' @param x Ring
#' @param buzzard_db path to buzzard_db
#' @param out.file valid path to write to text. By default temporary only
#' @import magrittr
#' @export
#'
birdinfo <- function(x = NULL, buzzard_db = "RData/buzzard_db.RData", out.file = NULL) {
  options(width = 1e4)
  ## checks
  ## ======================================================================================================================================
  if (!file.exists(buzzard_db)) stop(paste(buzzard_db, "does not exist"))
  load(buzzard_db)
  if (!x %in% buzzard_db[["ring_db"]][["Ring"]]) stop(paste(x), "does not exists")

  ring <- dplyr::filter(buzzard_db[["ring_db"]], Ring == x)[1,]
  ring[["Brood_ID"]] <- as.character(ring[["Brood_ID"]])
  repro <- dplyr::filter(buzzard_db[["repro_fledge_db"]],
                         Brood_ID %in% ring[["Brood_ID"]][!is.na(ring[["Brood_ID"]])])
  resights <- dplyr::filter(buzzard_db[["resights"]], Ring == x)
  genotype <- buzzard_db[["genotypes"]][rownames(buzzard_db[["genotypes"]]) == x,]

  ring[["Sex"]] <- ifelse(ring[["Sex"]] == 1, "Male", "Female")

  if (ring[["Morph"]] == 2) ring[["Morph"]] <- "Dark"
  if (ring[["Morph"]] == 3) ring[["Morph"]] <- "Intermediate"
  if (ring[["Morph"]] == 4) ring[["Morph"]] <- "Light"


  delete <- FALSE
  if (is.null(out.file)) {
    out.file <- tempfile()
    delete <- TRUE
  }

  sink(out.file)
  cat("Helgoland Ring:", x, "\t\tID:", ring[["ID"]], "\n")
  cat("================================================================================================================================================")
  cat("\n\n")
  cat("Ringing details:\n")
  cat("================================================================================================================================================")
  cat("\n")
  ring <- ring[,c("Date", "Territory", "Brood_ID", "Brood_Size", "Morph", "Sex")]
  names(ring) <- c("Date", "Territory", "Brood ID", "Brood Size", "Morph", "Sex")
  print(ring, row.names = FALSE)
  cat("\n")
  cat("================================================================================================================================================")
  cat("\n\n")
  cat("Resightings:\n")
  resights <- resights[,c("Date", "lat", "long", "Observer", "Dist2Nest", "Dir2Nest")]
  resights[["Observer"]][is.na(resights[["Observer"]])] <- ""
  names(resights) <- c("Date", "Lat", "Long", "Observer", "Distance_from_Nest", "Direction_from_Nest")

  # resights <- lapply(1:nrow(resights), function(x) {
  #   DBChecks::PlaceInfo(lat = resights[['Lat']][1], long = resights[['Long']][1],
  #                       gadm36 = "~/" )
  #})

  print(resights, row.names = FALSE)
  cat("================================================================================================================================================")
  cat("\n\n")
  if (ring[["Sex"]][1] == "Male") {
    repro <- dplyr::filter(buzzard_db[["repro_fledge_db"]],
                           Male_ID == paste0("R", x))

  } else if (ring[["Sex"]][1] == "Female") {
    repro <- dplyr::filter(buzzard_db[["repro_fledge_db"]],
                           Fem_ID == paste0("R", x))
  }

  if (nrow(repro) >= 1) {
    cat("Previous Breeding attempts:\n")
    cat("================================================================================================================================================")
    cat("\n")
    repro <- repro[,c("Year", "Territory", "N", "E", "Fem_ID", "Male_ID", "Repro")]
    names(repro) <- c("Year", "Territory", "Lat", "Long", "Female", "Male", "Chicks")
    print(repro, row.names = F)
    cat("================================================================================================================================================")
    cat("\n\n")
  }
  if (nrow(genotype) >= 1) {
    cat("Genotype:\n")
    cat("================================================================================================================================================")
    cat("\n")
    print(genotype, row.names = F)
    cat("================================================================================================================================================")
    cat("\n\n")
  }

  sink()
  file.show(out.file)
  if (isTRUE(delete)) unlink(out.file)
}

