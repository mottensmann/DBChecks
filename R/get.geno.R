#' Get genotype table for focal individual
#'
#' @description
#' For a focal individual (ie known Ring) collect all genotypes for presumed
#' offspring and if applicable all chicks from the same territories to check
#' for switches of territory owners.
#'
#' @param focal Ring number
#' @param path to buzzard_db
#' @param add.unrelated logical
#' @param add.neighbours logical
#' @import magrittr
#' @export
#'
get.geno <- function(focal = NULL, path = NULL, add.unrelated = T, add.neighbour = T) {

  ## get buzzard_db
  load(path)

  ## get ringing data for the focal
  df <- dplyr::filter(buzzard_db$ring_db, Ring == focal)[,c("Ring", "Sex")] %>%
    unique.data.frame()
  ## get breeding attempts
  if (df$Sex == 1) {
    broods <- dplyr::filter(buzzard_db$repro_fledge_db, Male_ID == paste0("R", focal))[["Brood_ID"]] %>%
      as.character()
  } else {
    broods <- dplyr::filter(buzzard_db$repro_fledge_db, Fem_ID == paste0("R", focal))[["Brood_ID"]] %>%
      as.character()
  }

  ## check for genoype of the focal individual
  focal.geno <- data.frame()
  if (focal %in% buzzard_db$genotypes.meta$Ring) {
    focal.geno <- dplyr::filter(buzzard_db$genotypes, rownames(buzzard_db$genotypes) %in% focal)
    focal.geno$focal <- focal
    rownames(focal.geno) <- focal
  }

  ## check for genotypes of the chicks
  chicks.geno <- data.frame()
  ## get chicks
  if (length(broods) > 0) {
    chicks <- dplyr::filter(buzzard_db$ring_db, Brood_ID %in% broods)[["Ring"]] %>%
      unique()

    if (length(chicks) >= 1 & any(chicks %in% buzzard_db$genotypes.meta$Ring)) {
      chicks.geno <- buzzard_db$genotypes[rownames(buzzard_db$genotypes) %in% chicks,]
      rownames(chicks.geno) <- chicks[chicks %in% buzzard_db$genotypes.meta$Ring]
      chicks.geno$focal <- focal
    }
  }

  out <- rbind(focal.geno, chicks.geno)
  ## remove new markers
  #out <- out[,c(1:26,43)]

  ## add Ring number and brood_id as columns
  out$Ring <- rownames(out)
  out <- dplyr::left_join(
    out,
    dplyr::filter(buzzard_db$ring_db[,c("Ring", "Brood_ID", "Terr_ID")] %>% unique.data.frame(), Ring %in% out$Ring),
    by = "Ring")

  if (isTRUE(add.unrelated)) {
    non.kin.chicks <- dplyr::filter(buzzard_db$ring_db[,c("Ring", "Brood_ID", "Terr_ID")]) %>%
      unique.data.frame()

    non.kin.geno <-
      dplyr::filter(buzzard_db$genotypes, rownames(buzzard_db$genotypes) %in% non.kin.chicks$Ring) %>%
      dplyr::mutate(focal = "unknown") %>%
      dplyr::mutate(Ring = buzzard_db$genotypes.meta$Ring[rownames(buzzard_db$genotypes) %in% non.kin.chicks$Ring]) %>%
      dplyr::left_join(., non.kin.chicks, by = "Ring")

    ## which are not yet in the table
    sub <- dplyr::filter(non.kin.geno,
                         !Ring %in% out$Ring,
                         Terr_ID %in% out$Terr_ID[out$Ring != out$focal])#[,c(1:26,43:46)]

    ## append
    out <- rbind(out, sub)

  }

  if (isTRUE(add.neighbour)) {
    ## get neighbouring territories
    focal.terrs <- out$Terr_ID[out$focal != out$Ring] %>% unique()
    if (length(focal.terrs) > 1) {
      terrs <- sapply(focal.terrs %>% unique,
                      DBChecks::nearest.neighbour,
                      df = buzzard_db$repro_fledge_db) %>%
        unique()
    } else if (length(focal.terrs) == 1) {
      terrs <- DBChecks::nearest.neighbour(df = buzzard_db$repro_fledge_db, terr.id = focal.terrs) %>%
        unique()
    }

    if (isTRUE(exists("terrs"))) {

      neighbour.geno <-
        dplyr::filter(buzzard_db$genotypes, rownames(buzzard_db$genotypes) %in% non.kin.chicks$Ring) %>%
        dplyr::mutate(focal = "neighbours") %>%
        dplyr::mutate(Ring = buzzard_db$genotypes.meta$Ring[rownames(buzzard_db$genotypes) %in% non.kin.chicks$Ring]) %>%
        dplyr::left_join(., non.kin.chicks, by = "Ring")

      sub <- dplyr::filter(neighbour.geno,
                           !Ring %in% out$Ring,
                           Terr_ID %in% terrs)#[,c(1:26,43:46)]
      ## append
      out <- rbind(out, sub)
    }

  }

  ## code as numeric
  l <- ncol(out)
  if (nrow(out) > 1) {
    out[,1:(l - 4)] <- apply(out[,1:(l - 4)],2, as.character)
    out[,1:(l - 4)] <- apply(out[,1:(l - 4)],2, as.numeric)
  }

  return(out)
}#end get_geno
# #
# rm(list = ls())
# add.unrelated = T
# focal <- 3109012
# path <- "../../01-PhD/00-Raw/RData/buzzard_db.RData"
# library(magrittr)
# #
# #get.geno(focal = 3406216, path = "../../01-PhD/00-Raw/RData/buzzard_db.RData" )
