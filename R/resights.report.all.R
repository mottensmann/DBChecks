#' Report on all individuals ever reported by a observer
#'
#' @param digits number of decimals
#' @param lang language
#' @param brood Default False. If True, returns resights of siblings
#' @param observer name
#' @inheritParams birdinfo
#' @import rtf
#' @import magrittr
#' @export
#'
resights.report.all <- function(buzzard_db = "RData/buzzard_db.RData", digits = 4, lang = c("German", "English"), brood = F,
                                observer = NULL) {
  lang = match.arg(lang)
  buzzard_db1 <- buzzard_db

  if (!file.exists(buzzard_db)) stop(paste(buzzard_db, "does not exist"))
  load(buzzard_db1)

  ## list all ind for observer
  x <- buzzard_db$resights$Ring[stringr::str_detect(buzzard_db$resights$Observer, observer)] %>%
    unique() %>%
    na.omit()
  rm(buzzard_db)
  buzzard_db <- buzzard_db1

  out.files <- paste0("resights/Buteo_buteo_", x, "_", observer, "_", Sys.Date(),".rtf")

  ## query all one by one
  out <- lapply(1:length(x), function(y) {
    pb <- utils::txtProgressBar(min = 1, max = length(x), style = 3, char = "+", width = 80)
    utils::setTxtProgressBar(pb, y)
    resight.report(x = x[y], buzzard_db = buzzard_db1, out.file = out.files[y], digits = digits, lang = lang, brood = brood)
  })

}

# rm(list = ls())
# library(DBChecks)
# library(magrittr)
# buzzard_db = "../../01-PhD/00-Raw/RData/buzzard_db.RData"
# lang <- "German"
# observer <- "Meinolf Ottensmann"
#
