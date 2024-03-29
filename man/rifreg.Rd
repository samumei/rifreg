% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rifreg.R
\name{rifreg}
\alias{rifreg}
\title{RIF regression}
\usage{
rifreg(
  formula,
  data,
  statistic = "quantiles",
  weights = NULL,
  probs = c(1:9)/10,
  custom_rif_function = NULL,
  na.action = na.omit,
  bootstrap = FALSE,
  bootstrap_iterations = 100,
  cores = 1,
  ...
)
}
\arguments{
\item{formula}{an object of class "formula". See \link[stats]{lm} for further details.}

\item{data}{a data frame containing the variables in the model.}

\item{statistic}{string containing the distributional statistic for which to compute the RIF. Can be one of
"quantiles", "mean", "variance", "gini", "interquantile_range", "interquantile_ratio", or "custom".
Default is "quantiles". If "custom" is selected, a \code{custom_rif_function} needs to be provided.}

\item{weights}{numeric vector of non-negative observation weights, hence of same length as \code{dep_var}.
The default (\code{NULL}) is equivalent to \code{weights = rep(1, length(dep_var))}.}

\item{probs}{a vector of length 1 or more with probabilities of quantiles. Each quantile is indicated with a value between 0 and 1.
Default is \code{c(1:9)/10}. If \code{statistic = "quantiles"}, a single RIF regression for every quantile in \code{probs}
is estimated. An interquantile ratio (range) is defined by the ratio (difference) between the \code{max(probs)}-quantile and
the \code{min(probs)}-quantile.}

\item{custom_rif_function}{the RIF function to compute the RIF of the custom distributional statistic.
Default is NULL. Only needs to be provided if \code{statistic = "custom"}.
Every custom_rif_function needs the parameters \code{dep_var}, \code{weights} and \code{probs}.
If they are not needed, they must be set to NULL in the function definition (e.g. \code{probs = NULL}).
A custom function must return a data frame containing at least a "rif" and "weights" column.
See \code{examples} for further details.}

\item{na.action}{generic function that defines how NAs in the data should be handled.
Default is \code{na.omit}, leading to exclusion of observations that contain one or more missings.
See \link[stats]{na.action} for further details.}

\item{bootstrap}{boolean (default = FALSE) indicating if bootstrapped standard errors will be computed}

\item{bootstrap_iterations}{positive integer indicating the number of bootstrap iterations to execute.
Only required if \code{bootstrap = TRUE}.}

\item{cores}{positive integer indicating the number of cores to use when computing bootstrapped standard errors.
Only required if \code{bootstrap = TRUE}.}

\item{...}{additional parameters passed to the \code{custom_rif_function}.
Apart from \code{dep_var}, \code{weights} and \code{probs} they must have a different name than the the ones in
\code{rifreg}. For instance, if you want to pass a parameter \code{statistic} to the
\code{custom_rif_function}, name it \code{custom_statistic}.}
}
\value{
\code{rifreg} returns an object of \code{\link{class}} \code{"rifreg"}.

        A \code{"rifreg"} object is a list containing the following components:

        \item{estimates}{a matrix of RIF regression coefficients for each
                         covariate and the intercept. In case of several quantiles,
                         coefficient estimates for each quantile are provided.
                         Equivalent to \code{coef()} call of an object of class \code{"lm"}.}
        \item{rif_lm}{one or several objects of class \code{"lm"},
                      containing the detailed RIF regression results.}
        \item{rif}{a data frame containing the RIF for each observation. }
        \item{bootstrap_se}{bootstrapped standard errors for each coefficient.
                            Only provided if \code{bootstrap = TRUE}.}
        \item{bootstrap_vcov}{the bootstrapped variance-covariance matrix for each coefficient.
                              Only provided if \code{bootstrap = TRUE}.}
        \item{statistic}{the distributional statistic for which the RIF was computed.}
        \item{custom_rif_function}{The custom RIF function in case it was provided.}
        \item{probs}{the probabilities of the quantiles that were computed, in case the distributional
                     statistic requires quantiles.}
}
\description{
Estimate a recentered influence function (RIF) regression for a
distributional statistic of interest.
}
\examples{

rifreg <- rifreg(
  formula = log(wage) ~ union +
    nonwhite +
    married +
    education +
    experience,
  data = men8385,
  statistic = "quantiles",
  weights = weights,
  probs = seq(0.1, 0.9, 0.1),
  bootstrap = FALSE
)


# custom function
custom_variance_function <- function(dep_var, weights, probs = NULL) {
  weighted_mean <- weighted.mean(x = dep_var, w = weights)
  rif <- (dep_var - weighted_mean)^2
  rif <- data.frame(rif, weights)
  names(rif) <- c("rif_variance", "weights")
  return(rif)
}

rifreg <- rifreg(
  formula = log(wage) ~ union + nonwhite + married + education + experience,
  data = men8385,
  statistic = "custom",
  weights = weights,
  probs = NULL,
  custom_rif_function = custom_variance_function,
  bootstrap = FALSE
)

}
\references{
Firpo, Sergio P., Nicole M. Fortin, and Thomas Lemieux. 2009. “Unconditional Quantile
Regressions.” \emph{Econometrica} 77(3): 953–73.

Cowell, Frank A., and Emmanuel Flachaire. 2015. “Statistical Methods for Distributional Analysis.”
In Anthony B. Atkinson and François Bourguignon (eds.), \emph{Handbook of Income Distribution}. Amsterdam: Elsevier.
}
