#' Summarise data frames
#'
#' @description
#'  Summarizes data by giving count, mean, standard deviation, standard error of the mean, and confidence interval
#'
#' @param data
#' a data frame
#'
#' @param measurevar
#' the name of a column that contains the variable to be summarised
#'
#' @param groupvars
#' a vector containing names of columns that contain grouping variables
#'
#' @param na.rm
#' a boolean that indicates whether to ignore NAs
#'
#' @param conf.interval
#' the percent range of the confidence interval (default is 95%)
#'
#' @param drop
#' Boolean
#'
#' @source
#' Taken from the R cookbook (cookbook-r.com/Manipulating_data/Summarizing_data/)
#'
#' @export
#'
summary_stats <- function(data = NULL, measurevar = NULL, groupvars = NULL, na.rm = FALSE, conf.interval = 0.95, drop = TRUE) {

length2 <- function(x, na.rm = FALSE) {
        if (na.rm) {
            sum(!is.na(x))
        } else {
            length(x)
}
}
# This does the summary. For each group's data frame, return a vector with
# N, mean, and sd
    datac <- plyr::ddply(data, groupvars, .drop = drop,
                   .fun = function(xx, col) {
                       c(N = length2(xx[[col]], na.rm = na.rm),
                         mean = mean(xx[[col]], na.rm = na.rm),
                         sd = sd(xx[[col]], na.rm = na.rm),
                         q1 = as.vector(stats::quantile(xx[[col]], .25, na.rm = T)),
                         q3 = as.vector(stats::quantile(xx[[col]], .75, na.rm = T)),
                         lci = as.vector(stats::quantile(xx[[col]], (1 - conf.interval)/2, na.rm = T)),
                         uci = as.vector(stats::quantile(xx[[col]], 1 - (1 - conf.interval)/2, na.rm = T))
                       )
                   },
                   measurevar
    )

    # Rename the "mean" column
    datac <- plyr::rename(datac, c("mean" = measurevar))
    datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean

    return(datac)
}
