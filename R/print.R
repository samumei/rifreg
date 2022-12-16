#' print method for class "rifreg"
#'
#' @param x an object of class "rifreg", usually , a result of a call to [rifreg()].
#' @param ... other parameters to be passed through to printing functions.
#'
#' @return the function \code{print.rifreg()} returns the the covariates' coefficients
#'         of the RIF regressions derived from  the fitted linear model given in object \code{x}.
#' @export
#'
#' @examples
#'
#' data <- CPSmen8305[1:300,]
#' rifreg <- est_rifreg(formula = log(wage) ~ union + age,
#'                      data = data,
#'                      functional = "quantiles",
#'                      custom_functional = NULL,
#'                      quantiles = 0.5,
#'                      weights = weights,
#'                      bootstrap = FALSE,
#'                      bootstrap_iterations = 100,
#'                      cores = 1,
#'                      model = TRUE)
#'
#' print(rifreg)
#'
print.rifreg <- function(x, ...){
  estimates <- x$estimates
  cat("Rifreg coefficients:\n")
  print(estimates)
}



