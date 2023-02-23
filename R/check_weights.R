#' Check weights
#'
#' Helper function to check a weights vector. Makes sure the weights
#' are positive numeric values (not all zeros) and of the same length as the
#' dependent variable \code{dep_var}. Replaces all \code{NA}s with 0 and sets
#' all weights to 1 if weights is set to NULL.
#'
#' @param dep_var dependent variable of distributional function.
#'                Can be any discrete or continuous vector of length 1 or more.
#' @param weights positive numeric vector of \code{length(dep_var)}
#'                containing the weights or \code{NULL}.
#'
#' @return positive numeric vector of \code{length(dep_var)}
#'         containing the checked weights. If \code{weights = NULL}, all weights are set to 1.
#' @export
#'
#' @examples
#'
#' dep_var <- c(1, 3, 9, 16, 3, 7, 4, 9)
#' weights <- c(2, 1, 3, 4, 4, 1, 6, 3)
#' check_weights(dep_var, weights)
#'
check_weights <- function(dep_var, weights) {
  if (is.null(weights)) weights <- rep(1, length(dep_var))
  if (!is.numeric(weights)) {
    stop(msg = "\"Weights\" must be a numeric vector!")
  } else {
    weights[is.na(weights)] <- 0
  }
  if (!length(dep_var) == length(weights)) stop(msg = "The input vector \"weights\" must be of equal length as the input vector \"dep_var\"!")
  if (!all(weights >= 0)) stop(msg = "Weights cannot contain negative values!")
  if (all(weights == 0)) stop(msg = "Not all weights can be set to zero or NA!")
  if (sum(weights) < length(dep_var)) {
    weights * length(dep_var) / sum(weights)
    warning("The sum of the weights is less than the number of observations.
Weigths were automatically refactored to as
weights = weights * length(dep_var) / sum(weights)")
  }
  return(weights)
}
