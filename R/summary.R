#' summary method for class "rifreg"
#'
#' @param object an object of class "rifreg", usually, a result of a call to \link[rifreg]{rifreg}.
#' @param vcov Function to estimate covariance matrix of rifreg coefficients if covariance matrix has not been bootstrapped.
#'             Per default, heteroscedasticity-consistent (HC) standard errors are calculated using \link[sandwich]{sandwich}.
#'             Note: These standard errors do not take the variance introduced by estimating RIF into account.
#' @param ... other parameters to be passed to summary functions.
#'
#' @return the function \code{summary.rifreg()} returns a list of summary statistics derived from
#'         the rifreg object given in \code{object}. For further details see \link[stats]{summary.lm}.
#' @export
#'
#' @examples
#'
#' rifreg <- rifreg(
#'   formula = log(wage) ~ union +
#'     nonwhite +
#'     married +
#'     education +
#'     experience,
#'   data = men8385,
#'   statistic = "quantiles",
#'   probs = seq(0.1, 0.9, 0.1),
#'   weights = weights
#' )
#' summary(rifreg)
#'
summary.rifreg <- function(object, vcov = sandwich::sandwich, ...) {
  estimates <- object$estimates
  standard_errors <- object$bootstrap_se
  standard_errors_type <- "Bootstrap s.e."
  if (is.null(standard_errors)) {
    standard_errors <- do.call("cbind", lapply(lapply(object$rif_lm, vcov, ...), function(x) sqrt(diag(x))))
    standard_errors_type <- "Analytical s.e."
  }

  r.squared <- unlist(do.call("c", lapply(object$rif_lm, function(z) summary(z)[c("r.squared")])))
  adj.r.squared <- unlist(do.call("c", lapply(object$rif_lm, function(z) summary(z)[c("adj.r.squared")])))
  df <- do.call("c", lapply(object$rif_lm, function(z) z$df.residual))
  sigma <- unlist(do.call("c", lapply(object$rif_lm, function(z) summary(z)[c("sigma")])))

  if (standard_errors_type != "Bootstrap s.e.") {
    cat("\n")
    cat("WARNING: Standard errors have not been bootstrapped!\n         Analytical s.e. do not take variance introduced by\n         estimating the RIF into account.")
    cat("\n")
  }

  for (i in 1:ncol(estimates)) {
    cat("\n")
    cat("RIF regression coefficients for ", gsub("rif_", "", colnames(estimates)[i]), "\n", sep = "")
    cat("\n")
    res <- cbind(estimates[, i], standard_errors[, i])
    res <- cbind(res, res[, 1] - res[, 2] * 1.96, res[, 1] + res[, 2] * 1.96)
    colnames(res) <- c("Estimate", standard_errors_type, "Lower bound", "Upper bound")
    print(res)
    cat("\n")
    cat("Residual standard error: ", sigma[i], " on ", df[i], " degrees of freedom\n", sep = "")
    cat("Multiple R-squared: ", r.squared[i], ",	Adjusted R-squared: ", adj.r.squared[i], "\n", sep = "")
    cat("\n")
  }

  invisible(list(
    estimates = estimates,
    standard_errors = standard_errors,
    r.squared = r.squared,
    adj.r.squared = adj.r.squared,
    df = df,
    sigma = sigma
  ))
}
