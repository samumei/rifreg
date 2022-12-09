est_rif <- function(functional, dep_var, weights = NULL, quantiles = NULL, custom_functional = NULL, ...) {

  if(!(functional == "mean" | functional == "variance" |
       functional == "quantiles" | functional == "gini" |
       functional == "custom")) {
    stop("Unknown functional! The functional must be string \"mean\", \"variance\", \"quantiles\", \"gini\", or \"custom\".")
  }

  if(functional == "quantiles") {
    if(is.null(quantiles)) stop(msg = "Parameter \"quantiles\" needs to have at least one value between 0 and 1.")
    if(!all(quantiles > 0 & quantiles < 1)) stop(msg = "All \"quantiles\" need to be larger than 0 and smaller than 1.")
  }
  if(functional == "custom") {
    if(is.null(custom_functional)) stop(msg = "If parameter \"functional\" is \"custom\", parameter \"custom_functional\" cannot be NULL, but has to be custom RIF function!")
    if(!is.null(quantiles)) stop("Parameter \"quantiles\" cannot be evaluated with custom functions and has to be NULL! To pass quantiles to a custom function give them a new name (e.g. \"custom_quantiles\").
                            SEEE HELP FOR DETAILS ")
    if(!is.null(weights)) stop("Parameter \"weights\" cannot be evaluated with custom functions and has to be NULL! To pass weights to a custom function give the parameter a new name (e.g. \"custom_weights\").
                            SEEE HELP FOR DETAILS ")
  }

  rif <- switch(functional,
                mean = est_rif_mean(dep_var = dep_var),
                variance = est_rif_variance(dep_var = dep_var, weights = weights),
                quantiles = est_rif_quantiles(quantiles = quantiles, dep_var = dep_var, weights = weights, ... = ...),
                gini = stop("GINI NOT YET IMPLEMENTED!"),
                custom = custom_functional(dep_var = dep_var, ...))

  rif <- as.data.frame(rif)

  return(rif)
}


#'#' Estimate RIF of the Mean
#'
#' Function to estimate the recentered influence function (RIF) of the mean
#' of a weighted distribution of a dependent variable.
#'
#' @param dep_var dependent variable of distributional function. Discrete or continuous numeric vector.
#'
#' @return A data frame with the number of columns equaling the length of vector \code{quantiles}. Each column contains the RIF values of the quantiles.
#' @export
#'
#' @examples
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
#' @param quantiles quantile positions to calculate the RIF. A Double with value between 0 and 1.
#' @param weights numeric vector of non-negative observation weights, hence of same length as \code{dep_var}.
#' The default NULL is equivalent to weights = rep(1/nx, nx) where nx is the length of (the finite entries of) dep_var[].
#' @inheritParams est_rif_at_mean
#' @param ... further arguments passed on to \code{stats::density()} function.
#'
#' @return A data frame with the number of columns equaling the length of vector \code{quantiles}. Each column contains the RIF values of the quantiles.
#' @export
#'
#' @examples
#' dep_var <- c(1, 3, 9, 16, 3, 7, 4, 9)
#' quantiles <- seq(1:9)/10
#' weights <- c(2, 1, 3, 4, 4, 1, 6, 3)
#' est_rif_at_quantiles(dep_var, quantiles, weights = NULL)
#'
est_rif_quantiles <- function(dep_var, quantiles, weights = NULL, ...){
  weights <- check_weights(dep_var, weights)
  density <- density(x = dep_var, weights = weights/sum(weights, na.rm = TRUE), ...)
  rif <- sapply(X = quantiles, FUN = est_rif_quantile, dep_var = dep_var, weights = weights, density = density)
  rif <- as.data.frame(rif)
  names(rif) <- paste0("rif_quantile_", quantiles)
  return(rif)
}


#' @describeIn est_rif_at_quantiles
#' Helper function to estimate the RIF values of a specific quantile.
#'
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
#' The default NULL is equivalent to weights = rep(1/nx, nx) where nx is the length of (the finite entries of) \code{dep_var}.
#'
#' @return A data frame with the number of columns equaling the length of vector \code{quantiles}. Each column contains the RIF values of the quantiles.
#' @export
#'
#' @examples
#'
est_rif_variance <- function(dep_var, weights = NULL){
  weights <- check_weights(dep_var, weights)
  weighted_mean <- weighted.mean(x = dep_var, w = weights)
  rif <- (dep_var - weighted_mean)^2
  rif <- as.data.frame(rif)
  names(rif) <- "rif_variance"
  return(rif)
}

# est_rif_interquantile_range <- function(dep_var, weights = NULL){
#   weights <- check_weights(dep_var, weights)
#   weighted_mean <- weighted.mean(x = dep_var, w = weights)
#   rif <- (dep_var - weighted_mean)^2
#   rif <- as.data.frame(rif)
#   names(rif) <- "rif_variance"
#   return(rif)
# }




#### GINI -------

# est_rif_gini <- function(dep_var, weights = NULL){
#   weights <- check_weights(dep_var, weights)
#   weighted_mean <- weighted.mean(x = dep_var, w = weights)
#
#   cumulative_distribution_function <- ecdf(dep_var)
#   cumulative_distribution_of_dep_vars <- cumulative_distribution_function(dep_var)
#
#   dep_var_sorted <- sort(dep_var)
#   n <- length(dep_var_sorted)
#
#   gini <- 2 * sum(dep_var_sorted * 1:n) / (n * sum(dep_var_sorted)) - 1 - (1/n)
#
#
#   rif <- 2 * dep_var / weighted_mean * (Fy(y) - ((1 + vG) / 2)) + 2 * ((1 - vG / 2) - GL(p;Fy)) + vG
#
#
#
#
#   rif <- as.data.frame(rif)
#   names(rif) <- "rif_gini"
#   return(rif)
# }
#
#
# dep_vars_on_cumulative_distribution
#
#
#
# vG
# Fy(y) => cdf(dep_var)
# GL(p;FY)











# if (missing(formula) || (length(formula) != 3L) || (length(attr(terms(formula[-2L]),
#                                                                 "term.labels")) != 1L))
#   stop("'formula' missing or incorrect")
#
# x <- as.numeric(x)
#
# x <- rep(x, n)    # same handling as Lc
# if(na.rm) x <- na.omit(x)
# if (any(is.na(x)) || any(x < 0)) return(NA_real_)


