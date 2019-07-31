#' residual weight with respect to a regression of wing length against weight
#'
#' @param df data frame containing data
#' @param wing name of variable giving wing length
#' @param weight name of variable giving weight [g]
#' @param sex name of variable giving sex
#' @param unit unit of wing measurements
#' @param .plot logical
#'
#' @references
#' Bijlsma, RG 1999: Sex determination of nestling Common Buzzards Buteo buteo. Limosa 72, 1.10
#'
#' @examples
#' # NOT RUN
#' # buteo_condition(data.frame(wing = c(240,240), sex = c(0,1), weight = c(700,700)))
#'
#' @import ggplot2 magrittr
#'
#' @export
#'
buteo_condition <- function(df = NULL, wing = "wing", weight = "weight", sex = "sex", unit = c("cm", "mm"), .plot = F) {
  ## get standard growth data
  path <- system.file("extdata", "buzzard_wing_weight.txt", package = "DBChecks")
  data <- read.table(path, header = T, skip = 1,
                     colClasses = c("integer", "factor", "numeric", "numeric"))

  df <- data.frame(wing = df[[wing]],
                   weight = df[[weight]],
                   sex = df[[sex]])

  unit <- match.arg(unit)
  if (unit == "cm") data[["wing"]] <- data[["wing"]]/10

  # regression of weight on wing length
  # males
  data.male <- dplyr::filter(data, sex == "1")
  male.lm <- lm(weight ~ log10(wing), data = data.male)

  male.fit <- data.frame(weight = predict(male.lm, data.male), wing = data.male[["wing"]])

  # females
  data.female <- dplyr::filter(data, sex == "0")
  female.lm <- lm(weight ~  log10(wing), data = data.female)

  female.fit <- data.frame(weight = predict(female.lm, data.female), wing = data.female[["wing"]])

  if (.plot == T) {
    plot <- ggplot(data, aes(x = wing, y = weight, col = sex)) +
      geom_point(alpha = .8, na.rm = T) +
      scale_x_log10(breaks = seq(0, 35, 5),
                    minor_breaks = seq(0, 35, 1)) +
      scale_y_continuous(breaks = seq(0, 1000, 150)) +
      theme_classic() +
      theme(text = element_text(family = 'Arial'),
            axis.title = element_text(size = 16),
            axis.text  = element_text(size = 12),
            axis.ticks = element_line(size = .8, colour = 'grey40'),
            plot.margin = grid::unit(c(2,2,2,2), "mm"),
            panel.grid.minor = element_line(colour = "grey40", size = .25),
            legend.position = 'bottom',
            aspect.ratio = 1) +
      xlab(paste0("\nLog Wing length [", unit, "]")) +
      ylab(paste0("Weight [g]\n")) +
      ggtitle("Common Buzzard growth curve",
              subtitle = "data: Bijlsma, RG (1999): Limosa 72") +
      scale_color_discrete(name = "",
                           breaks = c("1", "0"),
                           labels = c("Male", "Female")) +
      geom_line(data = female.fit, size = 1, colour = "red",
                aes(x = wing, y = weight)) +
      geom_line(data = male.fit, size = 1, colour = "blue",
                aes(x = wing, y = weight))
      # annotate("text", x = 4.5, y = 850, colour = "blue",
      #          label = paste0("y=", round(coefficients(male.lm)[[2]],1), "*log(x)",
      #                         round(coefficients(male.lm)[[1]],1),
      #                         " , R²=0.99")) +
      # annotate("text", x = 4.5, y = 800, colour = "red",
      #          label = paste0("y=", round(coefficients(female.lm)[[2]],1), "*log(x)",
      #                         round(coefficients(female.lm)[[1]],1),
      #                         " , R²=0.99"))
    out <- plot
  } else {
    # calculate residuals
    out <- lapply(1:nrow(df), function(x) {

      # check that information is present if not return NA
      if (any(is.na(df[x, c("sex", "weight", "wing")]))) {
        return(NA)
      } else if (df[x, "sex"] == 1) {
        return(df[x, "weight"] - predict.lm(male.lm, df[x,], se.fit = F)[[1]])
      } else {
        return(df[x, "weight"] - predict.lm(female.lm, df[x,], se.fit = F)[[1]])
      }

    }) %>%
      unlist()
  }
  return(out)
}
