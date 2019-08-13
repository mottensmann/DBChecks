#' Pool shapes
#'
#' @param folder character vector giving directories
#' @param outdir output directory
#' @export
#'
pool.shapes <- function(folder = c("../../01-PhD/GIS/basis-dlm/basis-dlm_EPSG25832_Shape/",
                                   "../../01-PhD/GIS/D21638_UNI_Bielefeld/",
                                   "../../01-PhD/GIS/D21966_UNI_Bielefeld/3716_3815/",
                                   "../../01-PhD/GIS/D21966_UNI_Bielefeld/3817/"),
                        outdir = "../../01-PhD/GIS/DLM/") {

  ## list files
  shapes <- lapply(folder, list.files, pattern = ".shp", recursive = T) %>%
    do.call("c",.) %>%
    unique() %>%
    stringr::str_remove_all(., ".shp")

  ## create dirs
  silent <- lapply(shapes, function(x) {
    ## check if dir exist
    if (!dir.exists(file.path(outdir, x))) {
      dir.create(file.path(outdir, x))
    }
  })

  ## copy files
  silent <- lapply(shapes, function(x) {
    ## list file with full names for each folder
    files <- lapply(folder, list.files, pattern = x, full.names = T)

    ## for each folder
    silent <- lapply(1:length(files), function(y) {
      ## create subfolder
      if (!dir.exists(file.path(outdir, x, LETTERS[y]))) {
        dir.create(file.path(outdir, x, LETTERS[y]))
      }
      ## move files
      silent <- lapply(files[[y]], filesstrings::file.move, destinations = file.path(outdir, x, LETTERS[y]))
      })
    })

}


