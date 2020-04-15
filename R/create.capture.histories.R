#' Create Dead and live encounter History (LDLD ...) for Burnham mddel
#'
#' @param start first year
#' @param end: last year
#' @param ringing_data data frame
#' @param resights: data frame
#' @return capture history in LDLD...LD format
#' @import magrittr dplyr
#' @export
create.dl <- function(start = 2007, end = 2019,
                      ringing_data = ringing_data, resights = resights) {
  ## list all unique individuals
  individuals <- ringing_data$Ring %>%
    unique()

  ## define periods
  years <- c(start, end)
  periods <- rep(seq(start, end), each = 1)

  ## create matrix
  ## rows: Individuals
  ## cols: Periods (1 or 2 per year)
  mat <- matrix(0, nrow = length(individuals), ncol = length(periods)*2) %>%
    set_colnames(paste0(rep(periods, each = 2), c("L", "D"))) %>%
    set_rownames(individuals)

  ## code year of ringing as 1,
  ## then add resightings
  for (x in individuals) {
    year.ringed <- filter(ringing_data, Ring == x)[["Year"]][1]
    ## code birth year in leaving encounters
    mat[rownames(mat) == x, colnames(mat) == paste0(year.ringed, "L")] <- 1
    ## get resightings

    ## get resights but ignore all till end of first winter
    year.resighted <-
      filter(resights, Ring == x)[,c("Year", "Month")] %>%
      mutate(period = paste0(Year,"L")) %>%
      mutate(year.month = paste0(Year,"+", Month)) %>%
      filter(!year.month %in% paste0(year.ringed + 1, paste0("+", c("01")))) %>%
      subset(., select = -c(year.month))

    ## write resights to matrix
    for (col in year.resighted$period) {
      mat[rownames(mat) == x, colnames(mat) == col] <- 1
    }

    ## check if reported as dead
    death <- filter(ringing_data, Ring == x)[,c("Dead", "DateDeath")][1,]
    if (!is.na(death$Dead)) {
      col <- paste0(substr(death$DateDeath, 1, 4),"D")
      mat[rownames(mat) == x, colnames(mat) == col] <- 1
    }

  }
  if ((test <- any(rowSums(mat[,seq(1, ncol(mat), 2)]) == 0))) warning("Empty capture histories")

  ## any resight following dead encounter
  test <- lapply(1:nrow(mat), function(row) {
    x <- mat[row, ]
    live <- x[seq(1, length(x), 2)]
    death <- x[seq(2, length(x), 2)]
    death.period <- which(death == 1)
    if (length(death.period == 1)) {
      if (death.period < length(death))
        if (any(live[(death.period + 1):length(live)] == 1)) {
          warning("Live encounters after death recorded at index ", row)
        }
    }

  })

  ## coerce to character vector
  return(apply(mat[,], 1, function(x) paste0(x, collapse = "")) %>%
           as.character() %>%
           set_names(individuals))

}

#' Create live encounter History for CJS model
#'
#' @param start first year
#' @param end: last year
#' @param ringing_data data frame
#' @param resights: data frame
#' @param interval 1 = annual resights; 2 = seasonal resights
#' @return capture history in LDLD...LD format
#' @import magrittr dplyr
#' @export
create.ch <- function(start =  2007, end = 2019, interval = c("1", "2"),
                      ringing_data = ringing_data, resights = resights) {

  ## distinguish intervals
  interval <- match.arg(interval) %>%
    as.numeric()

  ## list all unique individuals
  individuals <- ringing_data$Ring %>%
    unique()

  ## define periods
  years <- c(start, end)
  periods <- rep(seq(start, end), each = interval)

  ## if two events per year, distinguish between spring & winter
  if (interval == 2) {
    periods[seq(1, length(periods), 2)] <- paste0(periods[seq(1, length(periods), 2)], ".spring")
    periods[seq(2, length(periods), 2)] <- paste0(periods[seq(2, length(periods), 2)], ".winter")
  }

  ## create matrix
  ## rows: Individuals
  ## cols: Periods (1 or 2 per year)
  mat <- matrix(0, nrow = length(individuals), ncol = length(periods)) %>%
    set_colnames(periods) %>%
    set_rownames(individuals)

  ## code year of ringing as 1,
  ## then add resightings
  for (x in individuals) {
    year.ringed <- filter(ringing_data, Ring == x)[["Year"]][1]
    if (interval == 2) {
      ## code birth year
      mat[rownames(mat) == x, colnames(mat) == paste0(year.ringed, ".spring")] <- 1
      ## get resightings
      year.resighted <- filter(resights, Ring == x)[,c("Year", "Month")] %>%
        mutate(season = ifelse(Month %in% c("03", "04", "05", "06", "07", "08"), ".spring", ".winter")) %>%
        mutate(period = paste0(Year, season))
    } else if (interval == 1) {
      ## code birth year
      mat[rownames(mat) == x, colnames(mat) == year.ringed] <- 1
      ## get resights but ignore all till end of first winter
      year.resighted <-
        filter(resights, Ring == x)[,c("Year", "Month")] %>%
        mutate(period = Year) %>%
        mutate(year.month = paste0(Year,"+", Month)) %>%
        filter(!year.month %in% paste0(year.ringed + 1, paste0("+", c("01")))) %>%
        subset(., select = -c(year.month))
    }

    ## write resights to matrix
    for (col in year.resighted$period) {
      mat[rownames(mat) == x, colnames(mat) == col] <- 1
    }

  }
  ## cohort sizes
  (cohorts <- colSums(mat[,seq(1, ncol(mat), interval)]))
  if ((test <- any(rowSums(mat[,seq(1, ncol(mat), interval)]) == 0))) warning("Empty capture histories")

  ## coerce to character vector
  return(apply(mat[,], 1, function(x) paste0(x, collapse = "")) %>%
           as.character() %>%
           set_names(individuals))
}
