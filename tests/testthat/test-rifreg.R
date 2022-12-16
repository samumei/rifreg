# RIF Regression

testthat::test_that("RIF regression function does not throw an error" , {
  data <- CPSmen8305[1:300,]
  weights <- CPSmen8305$weights[1:300]


expect_error(est_rifreg(formula = log(wage) ~ union + age,
                        data = data,
                        functional = "quantiles",
                        custom_rif_function = NULL,
                        quantiles = 0.5,
                        weights = weights,
                        bootstrap = FALSE,
                        bootstrap_iterations = 100,
                        cores = 1,
                        model = TRUE),
             NA)

# with bootstrap and several quantiles
expect_error(est_rifreg(formula = log(wage) ~ union + age,
                        data = data,
                        functional = "quantiles",
                        custom_rif_function = NULL,
                        quantiles = c(0.1, 0.5, 0.9),
                        weights = weights,
                        bootstrap = TRUE,
                        bootstrap_iterations = 100,
                        cores = 1,
                        model = TRUE),
             NA)
})

testthat::test_that("RIF regression function does not throw an error with custom functions" , {
  test_data <- CPSmen8305[1:300,]
  test_weights <- CPSmen8305$weights[1:300]

  custom_variance_function <- function(dep_var, custom_weights){
    weights <- check_weights(dep_var, weights = custom_weights)
    weighted_mean <- weighted.mean(x = dep_var, w = weights)
    rif <- (dep_var - weighted_mean)^2
    rif <- data.frame(rif, weights)
    names(rif) <- c("rif_variance", "weights")
    return(rif)
  }

  expect_error(est_rifreg(formula = log(wage) ~ union + age,
                          data = test_data,
                          functional = "custom",
                          custom_rif_function = custom_variance_function,
                          quantiles = NULL,
                          weights = NULL,
                          bootstrap = FALSE,
                          cores = 1,
                          model = TRUE,
                          custom_weights = test_weights),
               NA)

  custom_quantiles_function <- function(dep_var, custom_quantiles, custom_weights, ...){
    est_rif_quantile <- function(quantile, dep_var, weights, density) {
      weighted_quantile <- Hmisc::wtd.quantile(x = dep_var,  weights = weights, probs = quantile)
      density_at_quantile <- approx(x = density$x, y = density$y, xout = weighted_quantile)$y
      rif <- weighted_quantile + (quantile - as.numeric(dep_var <= weighted_quantile)) / density_at_quantile
      return(rif)
    }

    weights <- check_weights(dep_var, custom_weights)
    density <- density(x = dep_var, weights = weights/sum(weights, na.rm = TRUE), ...)
    rif <- sapply(X = custom_quantiles, FUN = est_rif_quantile, dep_var = dep_var, weights = weights, density = density)
    rif <- data.frame(rif, weights)
    names(rif) <- c(paste0("rif_quantile_", custom_quantiles), "weights")
    return(rif)
  }

  expect_error(est_rifreg(formula = log(wage) ~ union + age,
                          data = test_data,
                          functional = "custom",
                          custom_rif_function = custom_quantiles_function,
                          custom_quantiles = c(0.1, 0.5, 0.9),
                          weights = NULL,
                          bootstrap = FALSE,
                          cores = 1,
                          model = TRUE,
                          custom_weights = test_weights),
               NA)
})
