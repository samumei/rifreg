#' RIF regression
#'
#' Estimate a recentered influence function (RIF) regression for a
#' distributional statistic of interest.
#'
#' @references
#' Firpo, Sergio P., Nicole M. Fortin, and Thomas Lemieux. 2009. “Unconditional Quantile
#' Regressions.” \emph{Econometrica} 77(3): 953–73.
#'
#' Cowell, Frank A., and Emmanuel Flachaire. 2015. “Statistical Methods for Distributional Analysis.”
#' In Anthony B. Atkinson and François Bourguignon (eds.), \emph{Handbook of Income Distribution}. Amsterdam: Elsevier.
#'
#' @param formula an object of class "formula". See \link[stats]{lm} for further details.
#' @param data a data frame containing the variables in the model.
#' @param statistic string containing the distributional statistic for which to compute the RIF. Can be one of
#'                  "quantiles", "mean", "variance", "gini", "interquantile_range", "interquantile_ratio", or "custom".
#'                  Default is "quantiles". If "custom" is selected, a \code{custom_rif_function} needs to be provided.
#' @param custom_rif_function the RIF function to compute the RIF of the custom distributional statistic.
#'                            Default is NULL. Only needs to be provided if \code{statistic = "custom"}.
#'                            Every custom_rif_function needs the parameters \code{dep_var}, \code{weights} and \code{probs}.
#'                            If they are not needed, they must be set to NULL in the function definition (e.g. \code{probs = NULL}).
#'                            A custom function must return a data frame containing at least a "rif" and "weights" column.
#'                            See \code{examples} for further details.
#' @param probs a vector of length 1 or more with probabilities of quantiles. Each quantile is indicated with a value between 0 and 1.
#'              Default is \code{c(1:9)/10}. If \code{statistic = "quantiles"}, a single RIF regression for every quantile in \code{probs}
#'              is estimated. An interquantile ratio (range) is defined by the ratio (difference) between the \code{max(probs)}-quantile and
#'              the \code{min(probs)}-quantile.
#' @param weights numeric vector of non-negative observation weights, hence of same length as \code{dep_var}.
#'                The default (\code{NULL}) is equivalent to \code{weights = rep(1, length(dep_var))}.
#' @param na.action generic function that defines how NAs in the data should be handled.
#'                  Default is \code{na.omit}, leading to exclusion of observations that contain one or more missings.
#'                  See \link[stats]{na.action} for further details.
#' @param bootstrap boolean (default = FALSE) indicating if bootstrapped standard errors will be computed
#' @param bootstrap_iterations positive integer indicating the number of bootstrap iterations to execute.
#'                             Only required if \code{bootstrap = TRUE}.
#' @param cores positive integer indicating the number of cores to use when computing bootstrapped standard errors.
#'              Only required if \code{bootstrap = TRUE}.
#' @param ... additional parameters passed to the \code{custom_rif_function}.
#'            Apart from \code{dep_var}, \code{weights} and \code{probs} they must have a different name than the the ones in
#'            \code{rifreg}. For instance, if you want to pass a parameter \code{statistic} to the
#'            \code{custom_rif_function}, name it \code{custom_statistic}.
#'
#' @return \code{rifreg} returns an object of \code{\link{class}} \code{"rifreg"}.
#'
#'         A \code{"rifreg"} object is a list containing the following components:
#'
#'         \item{estimates}{a matrix of RIF regression coefficients for each
#'                          covariate and the intercept. In case of several quantiles,
#'                          coefficient estimates for each quantile are provided.
#'                          Equivalent to \code{coef()} call of an object of class \code{"lm"}.}
#'         \item{rif_lm}{one or several objects of class \code{"lm"},
#'                       containing the detailed RIF regression results.}
#'         \item{rif}{a data frame containing the RIF for each observation. }
#'         \item{bootstrap_se}{bootstrapped standard errors for each coefficient.
#'                             Only provided if \code{bootstrap = TRUE}.}
#'         \item{bootstrap_vcov}{the bootstrapped variance-covariance matrix for each coefficient.
#'                               Only provided if \code{bootstrap = TRUE}.}
#'         \item{statistic}{the distributional statistic for which the RIF was computed.}
#'         \item{custom_rif_function}{The custom RIF function in case it was provided.}
#'         \item{probs}{the probabilities of the quantiles that were computed, in case the distributional
#'                      statistic requires quantiles.}
#'
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
#'   weights = weights,
#'   probs = seq(0.1, 0.9, 0.1),
#'   bootstrap = FALSE
#' )
#'
#'
#' # custom function
#' custom_variance_function <- function(dep_var, weights, probs = NULL) {
#'   weighted_mean <- weighted.mean(x = dep_var, w = weights)
#'   rif <- (dep_var - weighted_mean)^2
#'   rif <- data.frame(rif, weights)
#'   names(rif) <- c("rif_variance", "weights")
#'   return(rif)
#' }
#'
#' rifreg <- rifreg(
#'   formula = log(wage) ~ union + nonwhite + married + education + experience,
#'   data = men8385,
#'   statistic = "custom",
#'   weights = weights,
#'   probs = NULL,
#'   custom_rif_function = custom_variance_function,
#'   bootstrap = FALSE
#' )
#'
rifreg <- function(formula,
                   data,
                   statistic = "quantiles",
                   weights = NULL,
                   probs = c(1:9) / 10,
                   custom_rif_function = NULL,
                   na.action = na.omit,
                   bootstrap = FALSE,
                   bootstrap_iterations = 100,
                   cores = 1,
                   ...) {
  # Assertions
  if (is.null(formula)) {
    stop("No formula provided. Please pass an object of class \"formula\". See stats::lm() for further details.")
  } else {
    if (!is(formula, "formula")) {
      stop("Parameter \"formula\" is not of class \"formula\". Please pass an object of class \"formula\". See stats::lm() for further details.")
    }
  }
  if (is.null(data)) {
    stop("No data provided. Please pass a data frame containing the variables in the model.")
  } else {
    if (!is.data.frame(data)) {
      stop("Parameter \"data\" is not of class \"data.frame\". Please pass a data frame.")
    }
  }

  # Use match.call function to call data.vectors
  function_call <- match.call()
  data_arguments_index <- match(c("formula", "data", "weights"), names(function_call), 0)
  data_arguments <- function_call[c(1, data_arguments_index)]
  data_arguments$drop.unused.levels <- TRUE
  data_arguments[[1]] <- as.name("model.frame")
  data_used <- eval.parent(data_arguments)
  data_used <- na.action(data_used)
  function_terms <- attr(data_used, "terms")

  # Extract variables
  dep_var <- model.response(data_used, "numeric")
  intercept_and_covariates <- model.matrix(function_terms, data_used)
  covariate_names <- colnames(intercept_and_covariates)[-1]
  covariates_numeric <- as.matrix(intercept_and_covariates[, -1])

  # Extract and check weights
  weights <- model.weights(data_used)
  weights <- check_weights(
    dep_var = dep_var,
    weights = weights
  )
  # RIF
  rifreg_detail <- est_rifreg(
    formula = formula,
    data_used = data_used,
    statistic = statistic,
    dep_var = dep_var,
    weights = weights,
    probs = probs,
    custom_rif_function = custom_rif_function,
    ...
  )
  rif_lm <- rifreg_detail[-length(rifreg_detail)]
  rif <- rifreg_detail$rif

  # Calculate Bootstrap standard errors
  if (bootstrap) {
    message("Bootstrapping Standard Errors...\n")
    if (cores == 1) {
      bootstrap_estimates <- pbapply::pblapply(
        1:bootstrap_iterations,
        function(x) {
          est_rifreg_bootstrap(
            formula = formula,
            data_used = data_used,
            statistic = statistic,
            dep_var = dep_var,
            weights = weights,
            probs = probs,
            custom_rif_function = custom_rif_function,
            bootstrap_iterations = bootstrap_iterations,
            ...
          )
        }
      )
    } else {
      cores <- min(cores, parallel::detectCores() - 1)
      cluster <- parallel::makeCluster(cores)
      parallel::clusterSetRNGStream(cluster, round(runif(1, 0, 100000)))
      parallel::clusterExport(
        cl = cluster,
        varlist = ls(),
        envir = environment()
      )
      bootstrap_estimates <- pbapply::pblapply(1:bootstrap_iterations,
        function(x) {
          est_rifreg_bootstrap(
            formula = formula,
            data_used = data_used,
            statistic = statistic,
            dep_var = dep_var,
            weights = weights,
            probs = probs,
            custom_rif_function = custom_rif_function,
            bootstrap_iterations = bootstrap_iterations,
            ...
          )
        },
        cl = cluster
      )
      parallel::stopCluster(cluster)
    }
    bootstrap_estimates <- as.data.frame(do.call("cbind", bootstrap_estimates))
    modelnames <- unique(colnames(bootstrap_estimates))
    bootstrap_se <- matrix(rep(NA, length(modelnames) * nrow(bootstrap_estimates)), ncol = length(modelnames))
    rownames(bootstrap_se) <- rownames(bootstrap_estimates)
    colnames(bootstrap_se) <- modelnames
    bootstrap_vcov <- list()
    for (i in 1:length(modelnames)) {
      sel <- which(names(bootstrap_estimates) %in% modelnames[i])
      bootstrap_vcov[[i]] <- var(t(bootstrap_estimates[, sel]))
      bootstrap_se[, i] <- sqrt(diag(bootstrap_vcov[[i]]))
    }
  } else {
    bootstrap_vcov <- bootstrap_se <- NULL
  }

  estimates <- do.call("cbind", lapply(rif_lm, coef))

  results <- list(
    estimates = estimates,
    rif_lm = rif_lm,
    rif = rif,
    bootstrap_se = bootstrap_se,
    bootstrap_vcov = bootstrap_vcov,
    statistic = statistic,
    custom_rif_function = custom_rif_function,
    probs = probs
  )

  class(results) <- c("rifreg", "lm")

  return(results)
}


# detailed rifreg estimation
est_rifreg <- function(formula,
                       data_used,
                       dep_var,
                       weights,
                       statistic,
                       probs,
                       custom_rif_function,
                       ...) {
  # Get RIF of distributional statistic
  rif <- get_rif(
    statistic = statistic,
    dep_var = dep_var,
    weights = weights,
    probs = probs,
    custom_rif_function = custom_rif_function,
    ...
  )

  # estimate RIF regression
  data_and_rif <- cbind(rif, data_used)
  n_rif <- ncol(rif) - 1
  rif_lm <- list()
  for (i in 1:n_rif) {
    rif_formula <- update(Formula::as.Formula(formula), Formula::as.Formula(paste0(names(rif)[i], " ~ .")))
    rif_lm[[i]] <- lm(rif_formula, data = data_and_rif, weights = weights)
    names(rif_lm)[[i]] <- names(rif)[i]
  }

  rif_lm$rif <- rif

  return(rif_lm)
}


# Bootstrap iteration (helper function)
est_rifreg_bootstrap <- function(data_used,
                                 dep_var,
                                 statistic,
                                 weights,
                                 probs,
                                 custom_rif_function,
                                 bootstrap_iterations,
                                 ...) {
  sample <- sample(1:nrow(data_used), nrow(data_used), replace = TRUE)
  rif_lm <- est_rifreg(
    data_used = data_used[sample, ],
    statistic = statistic,
    dep_var = dep_var[sample],
    weights = (weights[sample] / sum(weights[sample], na.rm = TRUE)) * sum(weights, na.rm = TRUE),
    probs = probs,
    custom_rif_function = custom_rif_function,
    ...
  )
  coefs <- do.call("cbind", lapply(rif_lm, coef))
  return(coefs)
}
