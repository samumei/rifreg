#' Estimate Recentered Influence Functions
#'
#' This function estimates the recentered influence function (RIF) of a chosen functional
#' (e.g. quantiles, variance or gini).
#'
#' @references
#' Firpo, Sergio P., Nicole M. Fortin, and Thomas Lemieux. 2009. “Unconditional Quantile
#' Regressions.” \emph{Econometrica} 77(3): 953–73.
#'
#' Cowell, Frank A., and Emmanuel Flachaire. 2015. “Statistical Methods for Distributional Analysis.”
#' In Anthony B. Atkinson and François Bourguignon (eds.), \emph{Handbook of Income Distribution}. Amsterdam: Elsevier.
#'
#' @param functional string containing the functional for which to compute the RIF. Can be one of
#'                   "mean", "variance", "quantiles", "gini", or "custom". If "custom"
#'                   is selected a \code{custom_rif_function} needs to be provided.
#' @param dep_var dependent variable of distributional function. Discrete or continuous numeric vector.
#' @param weights numeric vector of non-negative observation weights, hence of same length as \code{dep_var}.
#'                The default (\code{NULL)} is equivalent to \code{weights = rep(1/nx, nx)},
#'                where nx is the length of (the finite entries of) \code{dep_var}.
#' @param probs a vector of length 1 or more with quantile positions to calculate the RIF.
#'                  Each quantile is indicated with value between 0 and 1. Only required if \code{functional = "quantiles"}.
#' @param custom_rif_function the RIF function to compute the RIF of the custom functional. Default is NULL.
#' @param ... additional parameters passed to the \code{custom_rif_function}.
#'            Apart from \code{dep_var} they must have a different name than the the ones of
#'            \code{est_rif}. For instance, if you want to pass weights to the
#'            \code{custom_rif_function}, name them \code{custom_weights}.
#'
#' @return a data frame with the RIF value for each observation and in the case of several quantiles
#'         a column for each quantile.
#' @export
#'
#' @examples
#'
#' dep_var <- c(1, 3, 9, 16, 3, 7, 4, 9)
#' probs <- seq(1:9)/10
#' weights <- c(2, 1, 3, 4, 4, 1, 6, 3)
#' rif <- est_rif(functional = "quantiles",
#'                dep_var = dep_var,
#'                probs = probs,
#'                weights = weights)
#'
#' # custom function
#' custom_variance_function <- function(dep_var, custom_weights){
#'   weighted_mean <- weighted.mean(x = dep_var, w = weights)
#'   rif <- (dep_var - weighted_mean)^2
#'   rif <- data.frame(rif, weights)
#'   names(rif) <- c("rif_variance", "weights")
#'   return(rif)
#' }
#'
#' rif_custom <-  est_rif(functional = "custom",
#'                        dep_var = dep_var,
#'                        custom_rif_function = custom_variance_function,
#'                        custom_weights = weights)
#'
est_rif <- function(functional,
                    dep_var,
                    weights = NULL,
                    probs = NULL,
                    custom_rif_function = NULL,
                    ...) {

  # SHOULD WEIGHTS BE CHECKED HERE, IF est_rif bzw. rif Funktion alleine aufgerufen wird (public function)
  weights <- check_weights(dep_var, weights)

  if(!(functional == "mean" | functional == "variance" |
       functional == "quantiles" | functional == "gini" |
       functional == "custom")) {
    stop("Unknown functional! The functional must be string \"mean\", \"variance\", \"quantiles\", \"gini\", or \"custom\".")
  }

  if(functional == "quantiles") {
    if(is.null(probs)) stop(msg = "Parameter \"probs\" needs to have at least one value between 0 and 1.")
    if(!all(probs > 0 & probs < 1)) stop(msg = "All \"probs\" need to be larger than 0 and smaller than 1.")
  }
  if(functional == "custom") {
    if(is.null(custom_rif_function)) stop(msg = "If parameter \"functional\" is \"custom\", parameter \"custom_rif_function\" cannot be NULL, but has to be the custom RIF function!")
    if(!is.null(probs)) stop("Parameter \"probs\" cannot be evaluated with custom functions and has to be NULL! To pass probs to a custom function give them a new name (e.g. \"custom_quantiles\").
                            SEE HELP FOR DETAILS.")
    if(is.null(dep_var)) stop("A custom function needs to contain the parameter \"dep_var\".
                              It can be set to NULL if not required.")
    # if(!is.null(weights)) stop("Parameter \"weights\" cannot be evaluated with custom functions and has to be NULL! To pass weights to a custom function give the parameter a new name (e.g. \"custom_weights\").
    #                         SEEE HELP FOR DETAILS.")
  }

  rif <- switch(functional,
                mean = est_rif_mean(dep_var = dep_var),
                variance = est_rif_variance(dep_var = dep_var, weights = weights),
                quantiles = est_rif_quantiles(probs = probs, dep_var = dep_var, weights = weights, ... = ...),
                gini = stop("GINI NOT YET IMPLEMENTED!"),
                custom = custom_rif_function(dep_var = dep_var, ...))

  return(rif)
}


#'#' Estimate RIF of the Mean
#'
#' Function to estimate the recentered influence function (RIF) of the mean
#' of a weighted distribution of a dependent variable.
#'
#' @param dep_var dependent variable of distributional function. Discrete or continuous numeric vector.
#'
#' @return A data frame with the number of columns equaling the length of vector \code{probs}. Each column contains the RIF values of the probs.
#' @export
#'
#' @examples
#'
#' dep_var <- c(1, 3, 9, 16, 3, 7, 4, 9)
#' est_rif_mean(dep_var)
#'
est_rif_mean <- function(dep_var) {
  rif <- as.data.frame(dep_var)
  names(rif) <- "rif_mean"
  return(rif)
}


#' Estimate RIF of Quantiles
#'
#' Function to estimate the recentered influence function (RIF) of one or several specified quantiles
#' of a weighted distribution of a dependent variable.
#'
#' @param dep_var dependent variable of distributional function. Discrete or continuous numeric vector.
#' @param probs a vector of length 1 or more with quantile positions to calculate the RIF.
#'                  Each quantile is indicated with value between 0 and 1.
#' @param weights numeric vector of non-negative observation weights, hence of same length as \code{dep_var}.
#'                The default (\code{NULL)} is equivalent to \code{weights = rep(1/nx, nx)},
#'                where nx is the length of (the finite entries of) \code{dep_var}.
#' @param ... further arguments passed on to \code{stats::density()} function.
#'
#' @return A data frame with the number of columns equaling the length of vector \code{probs}. Each column contains the RIF values of the quantile's probabilities.
#' @export
#'
#' @examples
#' dep_var <- c(1, 3, 9, 16, 3, 7, 4, 9)
#' probs <- seq(1:9)/10
#' weights <- c(2, 1, 3, 4, 4, 1, 6, 3)
#' est_rif_quantiles(dep_var, probs, weights = weights)
#'
est_rif_quantiles <- function(dep_var, probs, weights, ...){
  density <- stats::density(x = dep_var, weights = weights/sum(weights, na.rm = TRUE), ...)
  rif <- sapply(X = probs, FUN = est_rif_quantile, dep_var = dep_var, weights = weights, density = density)
  rif <- data.frame(rif, weights)
  names(rif) <- c(paste0("rif_quantile_", probs), "weights")
  return(rif)
}


#' @describeIn est_rif_quantiles
#' Helper function to estimate the RIF values of a specific quantile.
#'
#' @param quantile the specific quantile for which to estimate the RIF.
#' @param density the kernel density estimation of \code{dep_var}.
#'                For further information see \code{?stats::density}.
est_rif_quantile <- function(quantile, dep_var, weights, density) {
  weighted_quantile <- Hmisc::wtd.quantile(x = dep_var,  weights = weights, probs = quantile)
  density_at_quantile <- approx(x = density$x, y = density$y, xout = weighted_quantile)$y
  rif <- weighted_quantile + (quantile - as.numeric(dep_var <= weighted_quantile)) / density_at_quantile
  return(rif)
}


#'#' Estimate RIF of Variance
#'
#' Function to estimate the recentered influence function (RIF) of the variance
#' of a weighted distribution of a dependent variable.
#'
#' @param dep_var dependent variable of distributional function. Discrete or continuous numeric vector.
#' @param weights numeric vector of non-negative observation weights, hence of same length as \code{dep_var}.
#'                The default (\code{NULL)} is equivalent to \code{weights = rep(1/nx, nx)},
#'                where nx is the length of (the finite entries of) \code{dep_var}.
#'
#' @return A data frame with one column containing the RIF of the variance for each observation.
#' @export
#'
#' @examples
#'
#' dep_var <- c(1, 3, 9, 16, 3, 7, 4, 9)
#' weights <- c(2, 1, 3, 4, 4, 1, 6, 3)
#' est_rif_variance(dep_var, weights = weights)
#'
est_rif_variance <- function(dep_var, weights){
  weighted_mean <- weighted.mean(x = dep_var, w = weights)
  rif <- (dep_var - weighted_mean)^2
  rif <- data.frame(rif, weights)
  names(rif) <- c("rif_variance", "weights")
  return(rif)
}


# GINI

# IQR


