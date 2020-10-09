#' Grouping dates into seasons
#'
#' @param dates date of the form 'YYYY-MM-DD'
#'
#' @export
#'
seasons <- function(dates = Sys.Date()) {
  # library(dplyr); library(magrittr)
  # dates <- c("2020-01-01", "2001-06-25", "2001-06-25", "2011-06-25")

  ##  formatting input
  dates <- as.Date(dates)
  years = format(dates, "%Y") %>%
    unique() %>%
    sort()

  ## set all dates
  df <- lapply(years, function(year) {
    data.frame(
      Date =  lubridate::as_date(
        lubridate::make_date(year = year, month = 1, day = 1):
          lubridate::make_date(year = year, month = 12, day = 31))
    ) %>%
      mutate(Month = lubridate::month(Date)) %>%
      mutate(Day = lubridate::day(Date)) %>%
      mutate(Year = lubridate::year(Date)) %>%
      mutate(Decade = NA) %>%
      mutate(Season = NA)
  }) %>%
    do.call("rbind",.)

  definition <- data.frame(
    Month = 1:12,
    d1 = seq(from = 1, to = 34, 3),
    d2 = seq(from = 2, to = 35, 3),
    d3 = seq(from = 3, to = 36, 3),
    season = c(
      rep("Winter", 2),
      rep("Spring", 3),
      rep("Summer", 3),
      rep("Autumn", 3),
      "Winter"
    ))


  for (i in 1:nrow(df)) {
    ## get d1:d3

    # first decade of the month
    if (df[i, "Day"] %in% 1:10) {
      df[i, "Decade"] <- definition[df[i, "Month"], "d1"]
      # second decade of the month
    } else if (df[i, "Day"] %in% 11:20) {
      df[i, "Decade"] <- definition[df[i, "Month"], "d2"]
      # third decade of the month
    } else {
      df[i, "Decade"] <- definition[df[i, "Month"], "d3"]
    }
    df[i, "Season"] <- definition[df[i, "Month"], "season"]

  }


  out <- data.frame(Date = dates) %>%
    left_join(df[, c("Date", "Decade", "Season")], by = "Date")

  return(out)
}
# rm(list = ls())
# year = 2020
# library(lubridate)
# library(magrittr)
# library(dplyr)
