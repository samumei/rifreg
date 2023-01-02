#' Estimate RIF Regression
#'
#' Estimate the recentered influence function regression (RIFREG) for a chosen
#' functional of interest.
#'
#' @references
#' Firpo, Sergio P., Nicole M. Fortin, and Thomas Lemieux. 2009. “Unconditional Quantile
#' Regressions.” \emph{Econometrica} 77(3): 953–73.
#'
#' Cowell, Frank A., and Emmanuel Flachaire. 2015. “Statistical Methods for Distributional Analysis.”
#' In Anthony B. Atkinson and François Bourguignon (eds.), \emph{Handbook of Income Distribution}. Amsterdam: Elsevier.
#'
#' @param formula an object of class "formula". See [stats::lm()] for further details.
#' @param data a data frame containing the variables in the model.
#' @param functional string containing the functional for which to compute the RIF. Can be one of
#'                   "mean", "variance", "quantiles", "gini", or "custom". If "custom"
#'                   is selected a \code{custom_rif_function} needs to be provided.
#' @param custom_rif_function the RIF function to compute the RIF of the custom functional.
#'                            Default is NULL. Only needs to provided if \code{functional = "custom"}.
#'                            Every custom_rif_function needs the parameters \code{dep_var} and \code{weights}.
#'                            If they are not needed they can be set to NULL in the function definition (e.g. \code{weights = NULL}.
#'                            See examples for further details.
#' @param probs a vector of length 1 or more with quantile positions to calculate the RIF.
#'                  Each quantile is indicated with value between 0 and 1. Only required if \code{functional = "quantiles"}.
#' @param weights numeric vector of non-negative observation weights, hence of same length as \code{dep_var}.
#'                The default (\code{NULL)} is equivalent to \code{weights = rep(1/nx, nx)},
#'                where nx is the length of (the finite entries of) \code{dep_var}.
#' @param bootstrap boolean (Default = FALSE) indicating if bootstrapped standard errors shall be computed
#' @param bootstrap_iterations positive integer indicating the number of bootstrap iterations to execute.
#'                             Only required if \code{bootstrap = TRUE}.
#' @param cores positive integer indicating the number of cores to use when computing bootstrap standard errors.
#'              Only required if \code{bootstrap = TRUE}.
#' @param ... additional parameters passed to the \code{custom_rif_function}.
#'            Apart from \code{dep_var} they must have a different name than the the ones of
#'            \code{est_rif}. For instance, if you want to pass weights to the
#'            \code{custom_rif_function}, name them \code{custom_weights}.
#'
#' @return an object of class \code{rifreg} containing the RIF regression estimate,
#'         bootstrap standard errors, the RIF values and further information.
#' @export
#'
#' @examples
#'
#' data <- CPSmen8305[1:300,]
#' weights <- CPSmen8305$weights[1:300]
#'
#' rifreg <- est_rifreg(formula = log(wage) ~ union + age,
#'                      data = data,
#'                      functional = "quantiles",
#'                      custom_rif_function = NULL,
#'                      probs = seq(0.1, 0.9, 0.1),
#'                      weights = weights,
#'                      bootstrap = FALSE,
#'                      bootstrap_iterations = 100,
#'                      cores = 1)
#'
#'
#' # custom function
#' custom_variance_function <- function(dep_var, weights){
#'   weighted_mean <- weighted.mean(x = dep_var, w = weights)
#'   rif <- (dep_var - weighted_mean)^2
#'   return(rif)
#' }
#'
#'
#' rifreg <- est_rifreg(
#'   formula = log(wage) ~ union + age,
#'   data = data,
#'   functional = "custom",
#'   custom_rif_function = custom_variance_function,
#'   probs = NULL,
#'   weights = weights,
#'   bootstrap = FALSE,
#'   cores = 1,
#'   custom_weights = example_weights)
#'
est_rifreg <- function(formula,
                       data,
                       functional,
                       custom_rif_function = NULL,
                       probs = NULL,
                       weights = NULL,
                       bootstrap = FALSE,
                       bootstrap_iterations = 100,
                       cores = 1,
                       ...){

  # Use match.call function to call data.vectors
  function_call <- match.call()
  data_arguments_index <- match(c("formula", "data", "weights", "na.action"), names(function_call), 0)
  data_arguments <- function_call[c(1, data_arguments_index)]
  data_arguments$drop.unused.levels <- TRUE
  data_arguments[[1]] <- as.name("model.frame")
  data_used <- eval.parent(data_arguments)
  function_terms <- attr(data_used, "terms")

  # Extract variables
  dep_var <- model.response(data_used, "numeric")
  intercept_and_covariates <- model.matrix(function_terms, data_used)
  covariate_names <- colnames(intercept_and_covariates)[-1]
  covariates_numeric <- as.matrix(intercept_and_covariates[, -1])

  # Extract and check weights
  weights <- model.weights(data_used)
  weights <- check_weights(dep_var = dep_var,
                             weights = weights)
  # RIF
  rifreg_detail <- est_rifreg_detail(formula = formula,
                                     data_used = data_used,
                                     functional = functional,
                                     dep_var = dep_var,
                                     weights = weights,
                                     probs = probs,
                                     custom_rif_function = custom_rif_function,
                                     ...)
  rif_lm <- rifreg_detail[-length(rifreg_detail)]
  rif <- rifreg_detail$rif


  # Calculate Bootstrap standard errors
  if(bootstrap){
    cat("Bootstrapping Standard Errors...\n")
    if(cores == 1) {
      bootstrap_estimates <- pbapply::pblapply(1:bootstrap_iterations,
                                               function(x) est_rifreg_bootstrap(formula = formula,
                                                                                data_used = data_used,
                                                                                functional = functional,
                                                                                dep_var = dep_var,
                                                                                weights = weights,
                                                                                probs = probs,
                                                                                custom_rif_function = custom_rif_function,
                                                                                bootstrap_iterations = bootstrap_iterations,
                                                                                ...))
    }
    else {
      cores <- min(cores, parallel::detectCores() - 1)
      cluster <- parallel::makeCluster(cores)
      parallel::clusterSetRNGStream(cluster, round(runif(1,0,100000)))
      parallel::clusterExport(cl = cluster,
                              varlist = ls(),
                              envir = environment())
      bootstrap_estimates <- pbapply::pblapply(1:bootstrap_iterations,
                                               function(x) est_rifreg_bootstrap(formula = formula,
                                                                                data_used = data_used,
                                                                                functional = functional,
                                                                                dep_var = dep_var,
                                                                                weights = weights,
                                                                                probs = probs,
                                                                                custom_rif_function = custom_rif_function,
                                                                                bootstrap_iterations = bootstrap_iterations,
                                                                                ...),
                                               cl = cluster)
      parallel::stopCluster(cluster)
    }

    bootstrap_estimates <- as.data.frame(do.call("cbind", bootstrap_estimates))
    modelnames <- unique(colnames(bootstrap_estimates))
    bootstrap_se <- matrix(rep(NA, length(modelnames) * nrow(bootstrap_estimates)), ncol = length(modelnames))
    rownames(bootstrap_se) <- rownames(bootstrap_estimates)
    colnames(bootstrap_se) <- modelnames
    bootstrap_vcov <- list()
    for(i in 1:length(modelnames)){
      sel <- which(names(bootstrap_estimates) %in% modelnames[i])
      bootstrap_vcov[[i]] <- var(t(bootstrap_estimates[,sel]))
      bootstrap_se[,i] <- sqrt(diag(bootstrap_vcov[[i]]))
    }
  }
  else {
    bootstrap_vcov <- bootstrap_se <- NULL
  }

  estimates <- do.call("cbind", lapply(rif_lm, coef))

  results <- list(estimates = estimates,
                  bootstrap_se = bootstrap_se,
                  bootstrap_vcov = bootstrap_vcov,
                  rif_lm = rif_lm,
                  rif = rif,
                  functional = functional,
                  custom_rif_function = custom_rif_function,
                  probs = probs)

  class(results) <- c("rifreg", "lm")

  return(results)
}


# specific rifreg function (maybe change name, do not export!)
est_rifreg_detail <- function(formula,
                              data_used,
                              functional,
                              dep_var,
                              weights,
                              probs,
                              custom_rif_function,
                              ...) {

  # Get RIF for functional
  rif <- est_rif(functional = functional,
                 dep_var = dep_var,
                 weights = weights,
                 probs = probs,
                 custom_rif_function = custom_rif_function,
                 ...)

  # combine rif and data
  data_and_rif <- tryCatch(cbind(rif, data_used),
                           error = function(err){
                             data_used$rif <- rif
                             data_used$weights <- weights
                             return(data_used)
  })


  n_rif <- ncol(rif)
  rif_lm <- list()

  for(i in 1:n_rif){
    rif_formula <- update(Formula::as.Formula(formula), Formula::as.Formula(paste0(names(data_and_rif)[i]," ~ .")))
    rif_lm[[i]] <- lm(rif_formula, data = data_and_rif, weights = weights)
    names(rif_lm)[[i]] <- names(rif)[i]
  }

  rif_lm$rif <- rif

  return(rif_lm)
}


# Bootstrap iteration (helper function)
est_rifreg_bootstrap <- function(data_used,
                                 functional,
                                 dep_var,
                                 weights,
                                 probs,
                                 custom_rif_function,
                                 bootstrap_iterations,
                                 ...) {

  sample <- sample(1:nrow(data_used), nrow(data_used), replace = TRUE)
  rif_lm <- est_rifreg_detail(data_used = data_used[sample,],
                              functional = functional,
                              dep_var = dep_var[sample],
                              weights = (weights[sample]/sum(weights[sample], na.rm = TRUE)) * sum(weights, na.rm = TRUE),
                              probs = probs,
                              custom_rif_function = custom_rif_function,
                              ...)
  coefs <- do.call("cbind",lapply(rif_lm, coef))
  return(coefs)
}
