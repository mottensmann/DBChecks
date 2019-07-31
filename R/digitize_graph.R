#' Digitize graphs using package digitize
#'
#' @description
#' Digitzes one or multiple datadata_sets within a single figure. One or both axes might be logarithmic. The user needs to define to values on both axes for calibration purposes. It´s not necessary to specify the global minimum or maximum values but coordinates that are possible to define precisely.
#'
#' @references
#' This functions is adopted from a blog post by Bart Rogiers:
#' http://rogiersbart.blogspot.de/2012/06/digitize-linear-and-semi-log-scale.html
#'
#' @param fname
#' Filename/ path of the graphic to read
#'
#' @param x_min
#' Minimum value on the horizontal axis
#'
#' @param x_max
#' Maximum value on the horizontal axis
#'
#' @param y_min
#' Minimum value on the vertical axis
#'
#' @param y_max
#' Maximum value on the vertical axis
#'
#' @param data_sets
#' defines how many data_sets will be extracted from the graph
#'
#' @param labels
#' character vector containing labels for data_sets
#'
#' @param log
#' defines logarithmic scales. By default scaling is assumed to be linear
#'
#' @param plot
#' If TRUE, the extracted data is plotted
#'
#' @return a data frame
#'
#' @author Meinolf Ottensmann
#' @export
#'
digitize_graph <- function(fname, x_min, x_max, y_min, y_max, data_sets = 1, labels= 1:data_sets, log = c("","x","y","xy"), plot = TRUE) {
log <- match.arg(log)

    dataset <- data.frame(x = NULL,y = NULL,lab = NULL)
    cat('Mark axes on the graph in the order \n')
    cat('x_min, x_max, y_min, y_max')
    axes.points <- digitize::ReadAndCal(fname)
    if (log == 'x') {
        x_min <- log10(x_min)
        x_max <- log10(x_max)
        }
    if (log == 'y') {
        y_min <- log10(y_min)
        y_max <- log10(y_max)
        }
    if (log == 'xy') {
        x_min <- log10(x_min)
        x_max <- log10(x_max)
        y_min <- log10(y_min)
        y_max <- log10(y_max)
        }
    for (i in 1:data_sets) {
        cat(paste('Mark point set "',labels[i],'"\n',sep = ''))
        data.points <- digitize::DigitData(col = 'red')
        dat <- digitize::Calibrate(data.points, axes.points, x_min, x_max, y_min, y_max)
        dat[["lab"]] <- rep(labels[i], nrow(dat))
        dataset <- rbind(dat, dataset)
    }
    if (log == 'x') dataset[["x"]] <- 10^(dataset[["x"]])
    if (log == 'y') {dataset$y <- 10^(dataset[["y"]])}
    if (log == 'xy') {dataset$x <- 10^(dataset[["x"]]); dataset[["y"]] <- 10^(dataset[["y"]])}
    if (plot == TRUE) {
        plot(dataset[["x"]],dataset[["y"]], log = log, pch = as.numeric(as.factor(dataset[["lab"]])),col = as.numeric(as.factor(dataset[["lab"]])),xlab = "x", ylab = "y")
        graphics::legend('topleft',pch = 1:data_sets,col = 1:data_sets, bty = 'o',legend = labels)
    }
    return(dataset)
}
