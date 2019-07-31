#' Checks for overdispersion in model fits
#'
#' @param model fitted model
#' @source https://ase.tufts.edu/gsc/gradresources/guidetomixedmodelsinr/mixed%20model%20guide.html
#' @return p.value for the null hypothesis that data are not overdispersed
#' @import utils
#' @export
#'
check.overdisp <- function(model) {
  ## number of variance parameters in an n-by-n variance-covariance matrix
  vpars <- function(m) {
    nrow(m) * (nrow(m) + 1)/2
  }
  # The next two lines calculate the residual degrees of freedom
  model.df <- sum(sapply(lme4::VarCorr(model), vpars)) + length(lme4::fixef(model))
  rdf <- nrow(stats::model.frame(model)) - model.df
  # extracts the Pearson residuals
  rp <- stats::residuals(model, type = "pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  # Generates a p-value. If less than 0.05, the data are overdispersed.
  pval <- stats::pchisq(Pearson.chisq, df = rdf, lower.tail = FALSE)
  c(chisq = Pearson.chisq, ratio = prat, rdf = rdf, p = pval)
}
