#' Compare DFs
#'
#' @param df.new data frame
#' @param df.old data frame
#' @param by grouping column
#' @export
#'
compare.df <- function(df.new = NULL, df.old = NULL, by = NULL) {
  df.new <- readxl::read_xlsx(df.new)
  df.old <- readxl::read_xlsx(df.old)
  df.new <- df.old[, names(df.old)]
  out <- compareDF::compare_df(df_old = df_old,
                               df_new =  df_new,
                               group_col = by, limit_html = 5000)
  knitr::knit_print(out$html_output)
}


