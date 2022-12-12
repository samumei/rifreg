est_rifreg <- function(formula,
                       data,
                       functional,
                       custom_functional = NULL,
                       quantiles = NULL,
                       weights = NULL,
                       bootstrap = FALSE,
                       bootstrap_iterations = 100,
                       cores = 1,
                       model = TRUE,
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
                                     quantiles = quantiles,
                                     custom_functional = custom_functional,
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
                                                                                quantiles = quantiles,
                                                                                custom_functional = custom_functional,
                                                                                bootstrap_iterations = bootstrap_iterations,
                                                                                ...))
    }
    else {
      cores <- min(cores, parallel::detectCores() - 1)
      cluster <- parallel::makeCluster(cores)
      parallel::clusterSetRNGStream(cluster, round(runif(1,0,100000)))
      parallel::clusterEvalQ(cl, {
        library("Hmisc")
        #library("rwdeco")                  # needs to be activated
      })
      # foos <- names(as.list(.GlobalEnv))     # needs to be deleted
      parallel::clusterExport(cl = cluster,
                              #varlist=c(foos,ls()),   # needs to be deleted
                              varlist=ls(),          # needs to be activated
                              envir=environment())
      bootstrap_estimates <- pbapply::pblapply(1:bootstrap_iterations,
                                               function(x) est_rifreg_bootstrap(formula = formula,
                                                                                data_used = data_used,
                                                                                functional = functional,
                                                                                dep_var = dep_var,
                                                                                weights = weights,
                                                                                quantiles = quantiles,
                                                                                custom_functional = custom_functional,
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

  if(!model) {
    rif_lm <- NULL
  }

  results <- list(estimates = estimates,
                  bootstrap_se = bootstrap_se,
                  bootstrap_vcov = bootstrap_vcov,
                  rif_lm = rif_lm,
                  rif = rif,
                  functional = functional,
                  custom_functional = custom_functional,
                  quantiles = quantiles)

  class(results) <- c("rifreg", "lm")

  # Return result
  return(results)
}


# specific rifreg function (maybe change name, do not export!)
est_rifreg_detail <- function(formula,
                              data_used,
                              functional,
                              dep_var,
                              weights,
                              quantiles,
                              custom_functional,
                              ...) {

  # Get RIF for functional
  rif <- est_rif(functional = functional,
                 dep_var = dep_var,
                 weights = weights,
                 quantiles = quantiles,
                 custom_functional = custom_functional,
                 ...)

  # estimate RIF regression
  data_and_rif <- cbind(rif, data_used, weights)
  n_rif <- ncol(rif)
  rif_lm <- list()
  for(i in 1:n_rif){
    rif_formula <- update(Formula::as.Formula(formula), Formula::as.Formula(paste0(names(rif)[i]," ~ .")))
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
                                 quantiles,
                                 custom_functional,
                                 bootstrap_iterations,
                                 ...) {

  sample <- sample(1:nrow(data_used), nrow(data_used), replace = TRUE)
  rif_lm <- est_rifreg_detail(data_used = data_used[sample,],
                              functional = functional,
                              dep_var = dep_var[sample],
                              weights = (weights[sample]/sum(weights[sample], na.rm = TRUE)) * sum(weights, na.rm = TRUE),
                              quantiles = quantiles,
                              custom_functional = custom_functional,
                              ...)
  coefs <- do.call("cbind",lapply(rif_lm, coef))
  return(coefs)
}
