#' Estimate the age of juvenile goshawks based on wing length
#'
#' @description Northern Goshawk age is estimated based on a standard growth curve modeled from published data by
#' Bijlsma, G. 1997: Handleiding veldonderzoek roofvogels.  KNNV Uitgeverij
#'
#' @details Estimations are based on a four degree polynomial fit
#'
#' @param df data frame containing data
#' @param wing column name for wing measurements
#' @param sex column name giving the sex of individuals or NULL
#' @param unit unit of values. By default in mm
#' @param .plot logical
#' @param .show_model logical
#' @param decimals decimals
#'
#' @references
#' Bijlsma, G. 1997: Handleiding veldonderzoek roofvogels. KNNV Uitgeverij
#'
#' @examples
#' ## 240 mm wing length
#' # accipiter_age(data.frame(wing = c(240,240), sex = c("male","female")))
#' # accipiter_age(data.frame(wing = c(240,240), sex = c(1,0)))
#'
#' @import ggplot2
#'
#' @export
#'
accipiter_age <- function(df = NULL, wing = "wing", sex = "sex", unit = c("mm", "cm"), .plot = F, decimals = 2,
                      .show_model = T) {
  unit <- match.arg(unit)

  ## translate binary sex
  if (!is.null(sex)) {
    if (class(type.convert(df[[sex]])) != 'factor') {
      df[[sex]][df[[sex]] == 1]  <- 'male'
      df[[sex]][df[[sex]] == 0] <- 'female'
    }
  }
  ## get standard growth data
  path <- system.file("extdata", "goshawk_wing.txt", package = "DBChecks")
  data <- read.table(path, header = T, skip = 1,
                     colClasses = c("integer", "factor", "factor",
                                    "numeric", "numeric", "integer",
                                    "factor", "factor"))

  if (unit == "cm") {
    data[["mean"]] <- data[["mean"]]/10
    data[["sd"]] <- data[["sd"]]/10
  }

  ## build model
  if (is.null(sex)) {
    # model <- stats::lm(age ~ mean, data)
    model <-  stats::lm(data$age ~ poly(data$mean, 4, raw = T))
    fit.val <- data.frame(age = stats::predict(model, data), mean = data[["mean"]])
  } else {
    # model <- stats::lm(age ~ mean + sex, data)
     model <-  stats::lm(data$age ~ poly(data$mean, 4, raw = T) + data$sex)
     fit.val <- data.frame(age = stats::predict(model, data), mean = data[["mean"]], sex = data[["sex"]])
  }
  if (isTRUE(.show_model)) print(summary(model))

  ## plot
  if (.plot == T) {
    plot <- ggplot(data, aes(y = age, x = mean, col = sex)) +
      geom_point(alpha = .8) +
      geom_line(data = fit.val, size = 1,
                aes(y = age, x = mean, col = sex)) +
      theme_classic(base_size = 12) +
      ylab("Age [days since hatching]") +
      xlab(paste0("Wing length [", unit, "]")) +
      scale_color_discrete(name = "",
                           breaks = c("1", "0"),
                           labels = c("Male", "Female")) +
      ggtitle("Northern Goshawk growth curve",
              subtitle = "data: Bijlsma, RG (1997)")

    if (!is.null(sex)) {
      plot <- plot +
        annotate("text", y = 50, x = ifelse(unit == "mm", 70, 7), colour = "red",
                 label = paste0("f(wing)=",
                                "4.957e-01*x-",
                                "4.349e-03*x^2+",
                                "1.919e-05*x^3-",
                                "2.796e-08*x^4-",
                                "6.484e+00")) +

        annotate("text", y = 47, x = ifelse(unit == "mm", 70, 7), colour = "blue",
                 label = paste0("f(wing)=",
                                "4.957e-01*x-",
                                "4.349e-03*x^2+",
                                "1.919e-05*x^3-",
                                "2.796e-08*x^4-",
                                "5.5223"))
    } else {
      plot <- plot +
        annotate("text", y = 50, x = ifelse(unit == "mm", 70, 7), colour = "red",
                 label = paste0("f(wing)=",
                                "5.025e-01*x-",
                                "4.468e-03*x^2+",
                                "1.995e-05*x^3-",
                                "2.951e-08*x^4-",
                                "6.104e+00"))
    }



    out <- plot
  } else {
    ## build model
    if (is.null(sex)) {
      model <- stats::lm(age ~ mean, data)
      fit.val <- data.frame(age = stats::predict(model, data), mean = data[["mean"]])
      ## format data
      to_predict <- data.frame(mean = df[[wing]])
    } else {
      model <- stats::lm(age ~ mean + sex, data)
      fit.val <- data.frame(age = stats::predict(model, data),
                            mean = data[["mean"]], sex = data[["sex"]])
      ## format data
      to_predict <- data.frame(mean = df[[wing]],
                               sex = df[[sex]])
    }
    out <- stats::predict.lm(model, to_predict, se.fit = T)
  }

  return(out)
}

