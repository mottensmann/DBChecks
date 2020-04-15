#' Filter GPX formatted waypoint data
#'
#' @description
#' Removes duplicates from gpx files based on identical coordinates and
#' elevation. Additional arguments allow to extract gpx points within a
#' user-defined grid. Requires packages magrittr and readOGR to be installed.
#'
#' @param input input file (*.gpx)
#' @param output output file (*.gpx). By default adds. ".filt".
#' @param region optional argument defining a geographic grid. data frame with
#' arguments: "north", "east", "west" & "south"
#' @param add.territory optional argument, allows to add territory name of last
#' breeding attempt to every point. Currently only Buteo buteo.
#' @param buzzard_db path to database file
#'
#' @param reduced logical
#'
#' @examples
#' ## Not run
#' # test <- gpx.filt(input = "../../../Dropbox/Fieldsaison_2019/GPX data/Pooled_GPX_2019-07-11.gpx",                       region = data.frame(north = 52.22, east = 8.52, south = 51.98, west = 8.30))
#' @export
#'
gpx.filt <- function(input = NULL,
                     output = NULL,
                     region = NULL,
                     add.territory = T,
                     buzzard_db = "RData/buzzard_db.RData",
                     reduced = F) {

  ## checks
  if (is.null(input)) stop("Define input file!")
  if (is.null(output)) output <- stringr::str_replace(input, ".gpx", ".filt.gpx")

  ## packages
  library(magrittr)

  ## 1. read input, ie gpx waypoints
  data.raw <- rgdal::readOGR(dsn = input, verbose = F, layer = "waypoints")

  ## 2. indentify uniques. Based on coordinates. time, creation time elevation
  ## are not the same even for identical points!
  rows.keep <- data.raw@coords %>%
    as.data.frame() %>%
    unique.data.frame %>%
    rownames %>%
    as.numeric

  ## 3. Subset data
  data.filt <- data.raw[rows.keep,]

  ## 4. filter based on coordinates
  if (!is.null(region)) {
    df <- data.filt@coords %>%
      as.data.frame %>%
      set_colnames(., c("easting", "northing"))
    df$rowname <- rownames(df) %>% as.numeric
    df <- dplyr::filter(df,
                        easting > region[["west"]],
                        easting < region[["east"]],
                        northing > region[["south"]],
                        northing < region[["north"]])
    # ## subset data again
    data.filt <- data.filt[df$rowname,]
  }

  ## 5. Add  territory names
  if (!is.null(add.territory)) {
    df <- data.filt@coords %>%
      as.data.frame %>%
      set_colnames(., c("E", "N"))
    df[,c("N", "E")] <- apply(df[,c("N", "E")], 2, round, digits = 5)
    df$Nest <- paste0(df$N, "-", df$E)
    ## add information from repro_fledge
    load(buzzard_db)

    ## buteo buteo
    repro <- cbind(
      data.frame(Species = "Buzzard"),
      buzzard_db$repro_fledge_db[, c("Territory", "Terr_ID", "Year", "N", "E")]
    )

    ## accipiter
    accipiter <-
      buzzard_db$accipiter_ring_db[,c(1:3, 15:16)] %>%
      dplyr::mutate(Year = substr(Date, 1,4)) %>%
      dplyr::mutate(Terr_ID = NA) %>%
      subset(., select = -c(Date))
    accipiter <- accipiter[,c(1:2,6,5,3,4)] %>%
      set_names(names(repro))

    ## milvus
    milvus <-
      buzzard_db$milvus_ring_db[,c("Species", "Territory", "Date", "Lat", "Long")] %>%
      dplyr::mutate(Year = substr(Date,1,4)) %>%
      dplyr::mutate(Terr_ID = NA) %>%
      subset(., select = -c(Date))
    milvus <- milvus[,c(1:2,6,5,3,4)] %>%
      set_names(names(repro))

    ## merge
    repro <- rbind(repro, accipiter, milvus)

    ## for each nest, find last brood
    repro[,c("N", "E")] <- apply(repro[,c("N", "E")], 2, round, digits = 5)
    repro$nest <- paste0(repro$N, "-", repro$E)

    repro <-
      lapply(repro$nest %>% unique, function(x) {
        temp <- dplyr::filter(repro, nest == x)
        temp[nrow(temp),]
      }) %>%
      do.call("rbind",.)

    ## set names
    names(repro) <- c("spec", "terr", "terrid", "lastbr", "N", "E", "Nest")
    repro$spec <- as.character(repro$spec)

    ## match columns
    df.extd <- dplyr::left_join(df, repro, by = "Nest")

    ## create filed cmt
    data.filt@data[["cmt"]] <- lapply(1:nrow(df.extd), function(x) {
      paste(df.extd[x, c("spec", "terr", "terrid", "lastbr")], collapse = "; ")
    }) %>%
      unlist

    data.filt@data[["cmt"]][data.filt@data[["cmt"]] == "NA; NA; NA; NA"] <- NA

    ## replace umlaute
    data.filt@data[["cmt"]] <- stringi::stri_replace_all_fixed(
      str = data.filt@data[["cmt"]],
      pattern = c("ä", "ö", "ü", "Ä", "Ö", "Ü", "ß"),
      replacement = c("ae", "oe", "ue", "Ae", "Oe", "Ue", "ss"),
      vectorize_all = FALSE
    )
  }

  ## 6. export to output file
  data.filt@data <- data.filt@data[,1:23]
  rgdal::writeOGR(obj = data.filt, dsn = output,
                  driver = "GPX", overwrite_layer = T, layer = "waypoints", encoding = "UTF-8")

  ## 7. Output reduced set of points, only nests used at least once or entered
  ## within the last four years
  if (isTRUE(reduced)) {
    reduced <- stringr::str_replace(output, ".filt.gpx", ".filt.reduced.gpx")

    broods <- which(!is.na(data.filt@data$cmt))
    past.years <- substr(data.filt@data$time, 1, 10) %>%
      as.Date()
    past.years <- which(past.years > Sys.Date() - (365*3))

    keep <- c(broods, past.years) %>%
      unique()

    data.reduced <- data.filt[keep,]
    rgdal::writeOGR(obj = data.reduced, dsn = reduced,
                    driver = "GPX", overwrite_layer = T, layer = "waypoints", encoding = "UTF-8")
  }

  output <- data.filt
  return(output)
}
# rm(list = ls())
# output = NULL
# region = NULL
# add.territory = T
# buzzard_db = "RData/buzzard_db.RData"
# input = "../../../Dropbox/Fieldsaison_2020/GPX/GPX_2020-03-02.gpx"
