#################################################################################
################# Recentered Influence Function Regression ######################
#################################################################################

# Author: David Gallusser & Samuel Meier
# Date:   10.08.2021

# Introduction ------------------------------

# This script provides and describes functions to calculate recentered influence
# function regressions.


# Data -------------------------------------

# The following data is stored in the /data-directory

# data('skala44_2015', envir = environment()) ### WHICH DATA DO YOU FINALLY WANT HERE?
# data('skala44_2019', envir = environment())
# data('skala44_2021', envir = environment())


# Recentered Influence Functions -----------------------------

#' Estimate RIF of Mean
#'
#' Function to estimate the recentered influence function (RIF) of the mean
#' of a weighted distribution of a dependent variable.
#'
#' @param dep_var dependent variable of distributional function. Discrete or continous numeric vector.
#' @param weights numeric vector of non-negative observation weights, hence of same length as \code{dep_var}.
#' The default NULL is equivalent to weights = rep(1/nx, nx) where nx is the length of (the finite entries of) dep_var[].
#'
#' @return A data frame with a single column containing the RIF values of the mean.
#' @export
#'
est_rif_of_mean <- function(dep_var, weights) {
  rif <- as.data.frame(dep_var)
  names(rif) <- "rif_of_mean"
  return(rif)
} ##### ---> WHY NOT WEIGHTED????


#' Estimate RIF of Quantiles
#'
#' Function to estimate the recentered influence function (RIF) of one or several specified quantiles
#' of a weighted distribution of a dependent variable.
#'
#' @param quantiles quantile position to calculate the RIF. Vector of length 1 or more containing positive doubles with value < 1.
#' @inheritParams est_rif_of_mean
#' @param ... further arguments passed on to \code{stats::density()} function.
#'
#' @return A data frame with the number of columns equaling the length of quantities[]. Each column contains the RIF values at the quantiles.
#' @export
#'
#' @examples
#' dep_var <- c(1, 3, 9, 16, 3, 7, 4, 9)
#' quantiles <- seq(1:9)/10
#' weights <- c(2, 1, 3, 4, 4, 1, 6, 3)
#' est_rif_of_quantiles(quantiles, dep_var, weights)
#'
est_rif_of_quantiles <- function(quantiles, dep_var, weights = NULL, ...){
  weights <- check_weights(dep_var, weights)
  density <- stats::density(x = dep_var, weights = weights/sum(weights, na.rm = TRUE), ...)
  rif <- sapply(X = quantiles, FUN = est_rif_of_quantile, dep_var = dep_var, weights = weights, density = density)
  rif <- as.data.frame(rif)
  names(rif) <- paste0("rif_of_quantile_", quantiles)
  return(rif)
}


#' @describeIn est_rif_of_quantiles helper function to estimate the RIF values of a specific quantile.
#'
#' @param quantile specific quantile at which to estimate the RIF values. Positive double with value < 1.
#' @param density kernel density approximation of \code{dep_var} calculated using stats::density().
#'
est_rif_of_quantile <- function(quantile, dep_var, weights, density) {
  weighted_quantile <- Hmisc::wtd.quantile(x = dep_var,  weights = weights, probs = quantile)
  density_at_quantile <- stats::approx(x = density$x, y = density$y, xout = weighted_quantile)$y
  rif <- weighted_quantile + (quantile - as.numeric(dep_var <= weighted_quantile)) / density_at_quantile
  return(rif)
}


#' Estimate RIF of Variance
#'
#' Function to estimate the recentered influence function (RIF) of the variance
#' of a weighted distribution of a dependent variable.
#'
#' @inheritParams est_rif_of_mean
#'
#' @return A data frame with a single column containing the RIF values of the variance.
#' @export
#'
#' @examples
est_rif_of_variance <- function(dep_var, weights) {
  weights <- check_weights(dep_var, weights)
  weighted_mean <- Hmisc::wtd.mean(dep_var, weights = weights)
  rif <- (dep_var - weighted_mean)^2
  rif <- as.data.frame(rif)
  names(rif) <- "rif_of_variance"
  return(rif)
}


###########################
# INSERT HERE: rif_of_gini
##########################


# Helper function to check the weights (MAYBE ADD SOME MORE COMMENTS)
check_weights <- function(dep_var, weights) {
  if (is.null(weights)){
    weights <- rep(1, length(dep_var))
  }
  weights[is.na(weights)] <- 0
  assertthat::assert_that(length(dep_var) == length(weights), msg = "The vector weights must be of equal length as the vector dep_var")
  assertthat::assert_that(all(weights >= 0), msg = "Weights cannot contain negative values!")
  assertthat::assert_that(!all(weights == 0), msg = "Not all weights can be set to zero or NA!")
  return(weights)
}


# TODO:

# Examples for mean and variance
# BETTER DOCUMENT THE WEIGHTS AS DEFINED IN FFL 2007 p.19 or else, why not at the mean??


