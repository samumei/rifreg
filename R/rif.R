#' Estimate Recentered Influence Functions
#'
#' This function estimates the recentered influence function (RIF) of a chosen distributional statistic
#' (e.g. quantiles, variance or gini).
#'
#' @references
#' Firpo, Sergio P., Nicole M. Fortin, and Thomas Lemieux. 2009. “Unconditional Quantile
#' Regressions.” \emph{Econometrica} 77(3): 953–73.
#'
#' Cowell, Frank A., and Emmanuel Flachaire. 2015. “Statistical Methods for Distributional Analysis.”
#' In Anthony B. Atkinson and François Bourguignon (eds.), \emph{Handbook of Income Distribution}. Amsterdam: Elsevier.
#'
#' @param statistic string containing the distributional statistic for which to compute the RIF. Can be one of
#'                   "mean", "variance", "quantiles", "gini", "interquantile_range", "interquantile_ratio", or "custom". If "custom"
#'                   is selected a \code{custom_rif_function} needs to be provided.
#' @param dep_var dependent variable of distributional function. Discrete or continuous numeric vector.
#' @param weights numeric vector of non-negative observation weights, hence of same length as \code{dep_var}.
#'                The default (\code{NULL)} is equivalent to \code{weights = rep(1/nx, nx)},
#'                where nx is the length of (the finite entries of) \code{dep_var}.
#' @param probs a vector of length 1 or more with quantile positions to calculate the RIF.
#'                  Each quantile is indicated with value between 0 and 1. Only required if \code{statistic = "quantiles"}.
#' @param custom_rif_function the RIF function to compute the RIF of the custom statistic.
#'                            Default is NULL. Only needs to provided if \code{statistic = "custom"}.
#'                            Every custom_rif_function needs the parameters \code{dep_var} and \code{weights}.
#'                            If they are not needed they can be set to NULL in the function definition (e.g. \code{weights = NULL}.
#'                            See examples for further details.
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
#' rif <- est_rif(dep_var = dep_var,
#'                weights = weights,
#'                statistic = "quantiles",
#'                probs = probs)
#'
#' # custom function
#' custom_variance_function <- function(dep_var, weights){
#'   weighted_mean <- weighted.mean(x = dep_var, w = weights)
#'   rif <- (dep_var - weighted_mean)^2
#'   return(rif)
#' }
#'
#' set.seed(123)
#' dep_var <- rlnorm(100)
#' weights <- rep(1, 100)
#'
#' # custom function top 10% percent income share
#' # (see Essam-Nassah & Lambert, 2012, and Rios-Avila, 2020)
#' custum_top_income_share_function <- function(dep_var, weights, probs=0.1){
#'   probs <- 1-probs
#'   weighted_mean <- weighted.mean(x = dep_var, w = weights)
#'   weighted_quantile <- Hmisc::wtd.quantile(x = dep_var,  weights = weights, probs = probs)
#'   lorenz_ordinate <-
#'     sum(dep_var[which(dep_var<=weighted_quantile)] *
#'       weights[which(dep_var<=weighted_quantile)]) / sum(dep_var*weights)
#'   if_lorenz_ordinate <- -(dep_var/weighted_mean) * lorenz_ordinate +
#'                           ifelse(dep_var < weighted_quantile,
#'                                  dep_var - (1-probs)*weighted_quantile,
#'                                  probs*weighted_quantile)/weighted_mean
#'   rif_top_income_share <-  (1-lorenz_ordinate) - if_lorenz_ordinate
#'   return(rif_top_income_share)
#' }
#'
#' rif_custom <-  est_rif(dep_var = dep_var,
#'                        weights = weights,
#'                        statistic = "custom",
#'                        custom_rif_function = custom_variance_function)
#'
est_rif <- function(dep_var,
                    weights = NULL,
                    statistic,
                    probs = NULL,
                    custom_rif_function = NULL,
                    ...) {
  # SHOULD WEIGHTS BE CHECKED HERE, IF est_rif bzw. rif Funktion alleine aufgerufen wird (public function)
  weights <- check_weights(dep_var, weights)

  if(!(statistic == "mean" | statistic == "variance" |
       statistic == "quantiles" | statistic == "gini" |
       statistic == "interquantile_range" | statistic == "interquantile_ratio" |
       statistic == "custom")) {
    stop("Unknown statistic! The statistic must be string \"mean\", \"variance\", \"quantiles\", \"gini\", \"interquantile_range\", \"interquantile_ratio\", or \"custom\".")
  }

  if(statistic == "quantiles") {
    if(is.null(probs)) stop(msg = "Parameter \"probs\" needs to have at least one value between 0 and 1.")
    if(!all(probs > 0 & probs < 1)) stop(msg = "All \"probs\" need to be larger than 0 and smaller than 1.")
  }
  if(statistic == "custom") {
    if(is.null(custom_rif_function)) stop(msg = "If parameter \"statistic\" is \"custom\", parameter \"custom_rif_function\" cannot be NULL, but has to be the custom RIF function!")
    if(!is.null(probs)) stop("Parameter \"probs\" cannot be evaluated with custom functions and has to be NULL! To pass probs to a custom function give them a new name (e.g. \"custom_quantiles\").
                            SEE HELP FOR DETAILS.")

  }

  rif <- switch(statistic,
                mean = est_rif_mean(dep_var = dep_var),
                variance = est_rif_variance(dep_var = dep_var, weights = weights),
                quantiles = est_rif_quantiles(probs = probs, dep_var = dep_var, weights = weights, ... = ...),
                gini = est_rif_gini(dep_var = dep_var, weights = weights),
                interquantile_range = est_rif_interquantile_range(dep_var = dep_var, weights = weights, probs = probs, ... = ...),
                interquantile_ratio = est_rif_interquantile_ratio(dep_var = dep_var, weights = weights, probs = probs, ... = ...),
                custom = custom_rif_function(dep_var = dep_var, weights, ...))

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
est_rif_quantiles <- function(dep_var, weights, probs, ...){
  density <- stats::density(x = dep_var, weights = weights/sum(weights, na.rm = TRUE), ...)
  rif <- sapply(X = probs, FUN = est_rif_quantile, dep_var = dep_var, weights = weights, density = density)
  rif <- data.frame(rif, weights)
  names(rif) <- c(paste0("rif_quantile_", probs), "weights")
  return(rif)
}


#' @describeIn est_rif_quantiles
#' Helper function to estimate the RIF values of a specific quantile.
#'
#' @param probs the specific quantile for which to estimate the RIF.
#' @param density the kernel density estimation of \code{dep_var}.
#'                For further information see \code{?stats::density}.
est_rif_quantile <- function(dep_var, weights, probs, density) {
  weighted_quantile <- Hmisc::wtd.quantile(x = dep_var,  weights = weights, probs = probs)
  density_at_quantile <- approx(x = density$x, y = density$y, xout = weighted_quantile)$y
  rif <- weighted_quantile + (probs - as.numeric(dep_var <= weighted_quantile)) / density_at_quantile
  return(rif)
}


#'#' Estimate RIF of variance
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


# read gini formula


# GINI

#' Estimate Gini coefficient (slightly adjusted code from Rothe (2015))
# gini <- function (dep_var, weights) {
#   n <- length(dep_var)
#   weights <- weights/sum(weights)
#   gini_coef <- sum(dep_var[order(dep_var)] * 1:n *  weights[order(dep_var)])
#   gini_coef <- 2 *  gini_coef/(n * sum(dep_var[order(dep_var)]  *  weights[order(dep_var)]))
#   gini_coef <- gini_coef - 1 - (1/n)
#   return(gini_coef)
# }

#' Integrate generalized Lorenz curve
integrate_generalized_lorenz_curve <- function (dep_var, weights) {
  weights <- weights / sum(weights)
  weighted_ecdf <- cumsum(weights[order(dep_var)])
  generalized_lorenz_ordinates <- cumsum(dep_var[order(dep_var)] * weights[order(dep_var)])
  lorenz_curve <- approxfun(c(0, weighted_ecdf),  c(0, generalized_lorenz_ordinates))
  integrated_lorenz_curve <- integrate(lorenz_curve, 0, 1)$value
  return(integrated_lorenz_curve )
}

#' Compute Gini coefficient
#'
#' Compute a weighted Gini coefficient by integrating the
#' generalized Lorenz curve.
#'
#' @param dep_var values of a non-negative continuous variable
#' @param weights numeric vector of non-negative observation weights, hence of same length as \code{dep_var}.
#'                The default (\code{NULL)} is equivalent to \code{weights = rep(1/nx, nx)},
#'                where nx is the length of (the finite entries of) \code{dep_var}.
#'
#' @references
#' Firpo, Sergio P., Nicole M. Fortin, and Thomas Lemieux. 2018. “Decomposing Wage Distributions Using Recentered
#' Influence Function Regressions.” \emph{Econometrics} 6(2), 28.
#'
#' @export
#'
#' @examples
#'
#' set.seed(123)
#' dep_var <- rlnorm(100)
#' weights <- rep(1, 100)
#' compute_gini(dep_var, weights)
#'
compute_gini <- function (dep_var, weights) {
  weighted_mean <- weighted.mean(x = dep_var, w = weights)
  integrated_generalized_lorenz_curve <- integrate_generalized_lorenz_curve(dep_var, weights)
  gini_coef <- 1 - (2 / weighted_mean) * integrated_generalized_lorenz_curve
  return(gini_coef)
}

#'#' Estimate RIF of Gini coefficient
#'
#' Compute the recentered influence function (RIF) of a weighted
#' Gini coefficient.
#'
#' @param dep_var values of a non-negative continuous dependent variable
#' @param weights numeric vector of non-negative observation weights, hence of same length as \code{dep_var}.
#'                The default (\code{NULL)} is equivalent to \code{weights = rep(1/nx, nx)},
#'                where nx is the length of (the finite entries of) \code{dep_var}.
#'
#' @return A data frame with one column containing the RIF of the Gini coefficient for each observation.
#' @references
#' Firpo, Sergio P., Nicole M. Fortin, and Thomas Lemieux. 2018. “Decomposing Wage Distributions Using Recentered
#' Influence Function Regressions.” \emph{Econometrics} 6(2), 28.
#'
#' Cowell, Frank A., and Emmanuel Flachaire. 2007. "Income distribution and inequality measurement: The problem of extreme values."
#' \emph{Journal of Econometrics}, 141(2), 1044-1072.
#'
#' Monti, Anna Clara. 1991. "The study of the Gini concentration ratio by means of the influence function."  \emph{Statistica} 51(4),
#' 561–577.
#'
#' @export
#'
#' @examples
#'
#' set.seed(123)
#' dep_var <- rlnorm(100)
#' weights <- rep(1, 100)
#' est_rif_gini(dep_var, weights = weights)
#'
est_rif_gini <- function(dep_var, weights){
  gini_coef <- compute_gini(dep_var, weights)
  weighted_mean <- weighted.mean(x = dep_var, w = weights)
  weighted_ecdf <- sapply(dep_var, function(x) sum(weights[which(dep_var<=x)])) / sum(weights)
  generalized_lorenz_ordinates <- sapply(dep_var, function(x) sum(dep_var[which(dep_var<=x)]
                                                                  * weights[which(dep_var<=x)])) / sum(weights)
  integrated_generalized_lorenz_curve <- integrate_generalized_lorenz_curve(dep_var, weights)
  integrated_lorenz_curve <- integrated_generalized_lorenz_curve/weighted_mean

  # Formula (52) of Cowell/Flachaire (2007): RIF = G + 2* (R - C + (y/mu)*(R - (1 - F)))
  rif <- gini_coef +
    2*(integrated_lorenz_curve - generalized_lorenz_ordinates +
         (dep_var / weighted_mean) * (integrated_lorenz_curve - (1 - weighted_ecdf)))
  rif <- data.frame(rif, weights)
  names(rif) <- c("rif_gini", "weights")
  return(rif)
}

#'#' Estimate RIF of interquantile range
#'
#' Compute the recentered influence function (RIF) of a weighted
#' interquantile range.
#'
#' @param dep_var dependent variable of distributional function. Discrete or continuous numeric vector.
#' @param probs a vector of length 2 with probabilities corresponding to the limits of the interquantile range of interest.
#'              The interquantile range is defined as difference between the quantile with the larger probability and the one
#'              with the lower probability.
#' @param weights numeric vector of non-negative observation weights, hence of same length as \code{dep_var}.
#'                The default (\code{NULL)} is equivalent to \code{weights = rep(1/nx, nx)},
#'                where nx is the length of (the finite entries of) \code{dep_var}.
#' @param ... further arguments passed on to \code{stats::density()} function.
#'
#' @return A data frame with one column containing the RIF of the interquantile range for each observation.
#' @references
#' Firpo, Sergio P., Nicole M. Fortin, and Thomas Lemieux. 2018. “Decomposing Wage Distributions Using Recentered
#' Influence Function Regressions.” \emph{Econometrics} 6(2), 28.
#'
#' @export
#'
#' @examples
#'
#' set.seed(123)
#' dep_var <- rlnorm(100)
#' weights <- rep(1, 100)
#' est_rif_interquantile_range(dep_var, probs=c(0.1,0.9), weights = weights)
#'
est_rif_interquantile_range <- function(dep_var, weights, probs, ...){
    probs <- range(probs)
    rif_quantiles <-  est_rif_quantiles(dep_var = dep_var,
                                        weights = weights,
                                        probs = probs, ...)
    rif <- data.frame(rif_quantiles[, ncol(rif_quantiles)] -  rif_quantiles[, 1], weights)
    names(rif) <- c(paste0("rif_iq_range_",max(probs),"_",min(probs)), "weights")
    return(rif)
}

#'#' Estimate RIF of interquantile ratio
#'
#' Compute the recentered influence function (RIF) of a weighted
#' interquantile ratio.
#'
#' @param dep_var dependent variable of distributional function. Discrete or continuous numeric vector.
#' @param probs a vector of length 2 with probabilities corresponding to the quantiles in the ratio's numerator and the denominator.
#'              The function defines the interquantile ratio as the ratio between the quantile with the larger probability (numerator) and
#'              the quantile with the lower probability (denominator).
#' @param weights numeric vector of non-negative observation weights, hence of same length as \code{dep_var}.
#'                The default (\code{NULL)} is equivalent to \code{weights = rep(1/nx, nx)},
#'                where nx is the length of (the finite entries of) \code{dep_var}.
#' @param ... further arguments passed on to \code{stats::density()} function.
#'
#' @return A data frame with one column containing the RIF of the interquantile ratio for each observation.
#' @references
#' Chung, Choe, and Philippe Van Kerm. 2018. "Foreign workers and the wage distribution: What
#' does the infuence function reveal?", \emph{Econometrics} 6(3), 41.
#'
#' @export
#'
#' @examples
#'
#' set.seed(123)
#' dep_var <- rlnorm(100)
#' weights <- rep(1, 100)
#' est_rif_interquantile_ratio(dep_var, probs=c(0.1,0.9), weights = weights)
#'
est_rif_interquantile_ratio <- function(dep_var, weights, probs, ...){
  probs <- range(probs)
  weighted_quantile <- Hmisc::wtd.quantile(x = dep_var,  weights = weights, probs = probs)
  iqratio <-  weighted_quantile[2]/weighted_quantile[1]
  density <- stats::density(x = dep_var, weights = weights/sum(weights, na.rm = TRUE), ...)
  density_at_quantile <- approx(x = density$x, y = density$y, xout = weighted_quantile)$y
  rif <-  iqratio +
    (1/weighted_quantile[1])*((probs[2] - as.numeric(dep_var <= weighted_quantile[2])) / density_at_quantile[2]) -
    iqratio*((probs[1] - as.numeric(dep_var <= weighted_quantile[1])) / density_at_quantile[1])
  rif <- data.frame(rif, weights)
  names(rif) <- c(paste0("rif_iq_ratio_",max(probs),"_",min(probs)), "weights")
  return(rif)
}

