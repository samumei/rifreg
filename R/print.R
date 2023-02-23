#' Print method for class "rifreg"
#'
#' @param x an object of class "rifreg", usually, a result of a call to \link[rifreg]{rifreg}.
#' @param ... other parameters to be passed to printing function.
#'
#' @return the function \code{print.rifreg()} returns the the covariates' coefficients
#'         of the RIF regressions derived from  the fitted linear model given in object \code{x}.
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
#'
#' print(rifreg)
#'
print.rifreg <- function(x, ...) {
  estimates <- x$estimates
  cat("Rifreg coefficients:\n")
  print(estimates)
}
