#' K-nearest neighbour distribution
#'
#' @description
#' Computes k-nearest neighbour values within a year
#'
#'
#' @import magrittr
#' @param dat data frame with columns Id, Year, Lat & Lon
#' @param k integer or integer vector denoting k.nearest neighbour to compute

#' @export
#'
k.nearest.distr <- function(da = data.frame(Id = buzzard_db$repro_fledge_db$Territory,
                                            Year = buzzard_db$repro_fledge_db$Year,
                                            Lat = buzzard_db$repro_fledge_db$N,
                                            Lon = buzzard_db$repro_fledge_db$E),
                            k = 1) {

  out <- lapply(unique(buzzard_db$repro_fledge_db$Year), function(year) {

    ## sample all nest within a year
    df <- filter(dat, Year == year, !is.na(Lat), !is.na(Lon))
    ## compute distance (unit = metres) matrix
    distm <- geosphere::distm(x = df[,c("Lon", "Lat")], fun = geosphere::distGeo) %>%
      set_colnames(df$Territory) %>%
      set_rownames(df$Territory)
    ## get k-nearest per row
    k.nearest <- apply(distm, 1, function(x) sort(x[x > 0])[k])

    return(data.frame(Year = year,
                      median = median(k.nearest),
                      mean = mean(k.nearest),
                      min = min(k.nearest),
                      max = max(k.nearest)))
  }) %>%
    do.call("rbind",.)

}#end function
