#' Estimate the age of juvenile buzzards based on wing length
#'
#' @description Buzzard age is estimated based on a standard growth curve modeled from published data by
#' Bijlsma (Limosa 72, 1999)
#'
#' @param df data frame containing data
#' @param wing column name for wing measurements
#' @param sex column name giving the sex of individuals or NULL
#' @param unit unit of values. By default in mm
#' @param .plot logical
#'
#' @references
#' Bijlsma, RG 1999: Sex determination of nestling Common Buzzards Buteo buteo. Limosa 72, 1.10
#'
#' @examples
#' ## 240 mm wing length
#' # buteo_age(data.frame(wing = c(240,240), sex = c("male","female")))
#' # buteo_age(data.frame(wing = c(240,240), sex = c(1,0)))
#'
#' @import ggplot2
#'
#' @export
#'
buteo_age <- function(df = NULL, wing = "wing", sex = "sex", unit = c("mm", "cm"), .plot = F) {
  unit <- match.arg(unit)

  ## translate binary sex
  if (!is.null(sex)) {
    if (class(type.convert(df[[sex]])) != 'factor') {
      df[[sex]][df[[sex]] == 1]  <- 'male'
      df[[sex]][df[[sex]] == 0] <- 'female'
    }
  }
  ## get standard growth data
  path <- system.file("extdata", "buzzard_wing.txt", package = "DBChecks")
  data <- read.table(path, header = T, skip = 1,
                     colClasses = c("integer", "factor", "factor",
                                    "numeric", "numeric", "integer",
                                    "factor", "factor", "factor"))

  if (unit == "cm") {
    data[["mean"]] <- data[["mean"]]/10
    data[["sd"]] <- data[["sd"]]/10
  }

  ## build model
  if (is.null(sex)) {
    model <- stats::lm(age ~ mean, data)
    fit.val <- data.frame(age = stats::predict(model, data), mean = data[["mean"]])
  } else {
    model <- stats::lm(age ~ mean + sex, data)
    fit.val <- data.frame(age = stats::predict(model, data), mean = data[["mean"]], sex = data[["sex"]])
  }

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
    ggtitle("Common Buzzard growth curve",
            subtitle = "data: Bijlsma, RG (1999): Limosa 72") +
    annotate("text", y = 50, x = ifelse(unit == "mm", 70, 7), colour = "red",
             label = paste0("y=", round(coefficients(model)[[2]],2), "*x+",
                            round(coefficients(model)[[1]] - coefficients(model)[[3]],2),
                            " , R²=0.99")) +
    annotate("text", y = 47, x = ifelse(unit == "mm", 70, 7), colour = "blue",
             label = paste0("y=", round(coefficients(model)[[2]],2), "*x+",
                            round(coefficients(model)[[1]],2),
                            " , R²=0.99"))

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

