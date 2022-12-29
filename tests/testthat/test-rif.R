# RIF of specific quantile
testthat::test_that("RIF for specific quantile correctly calculated" , {

  dep_var <- CPSmen8305$wage[1:300]
  weights <- CPSmen8305$weights[1:300]
  quantile <- 0.5
  density_of_dep_var <- density(x = dep_var, weights = weights/sum(weights, na.rm = TRUE))

  # manual calculation
  weighted_quantile <- Hmisc::wtd.quantile(x = dep_var, weights = weights, probs = quantile)
  density_at_quantile <- approx(x = density_of_dep_var$x, y = density_of_dep_var$y, xout = weighted_quantile)$y
  influence_function <- (quantile - as.numeric(dep_var <= weighted_quantile)) / density_at_quantile
  manual_rif_q <- weighted_quantile + influence_function

  # calculation with function
  rif_q <- est_rif_quantile(dep_var = dep_var, weights = weights, quantile = quantile, density = density_of_dep_var)

  testthat::expect_equal(manual_rif_q, rif_q)
})


# RIF of probs
testthat::test_that("RIF for several quantiles correctly calculated" , {

  dep_var <- CPSmen8305$wage[1:300]
  weights <- CPSmen8305$weights[1:300]
  probs <- seq(1:9)/10
  density_of_dep_var <- density(x = dep_var, weights = weights/sum(weights, na.rm = TRUE))


  # manual calculation
  weighted_quantiles <- Hmisc::wtd.quantile(x = dep_var, weights = weights, probs = probs)
  density_at_quantiles <- approx(x = density_of_dep_var$x, y = density_of_dep_var$y, xout = weighted_quantiles)$y
  manual_rif <- sapply(X = probs, FUN = est_rif_quantile, dep_var = dep_var, weights = weights, density = density_of_dep_var)

  # calculation with function
  rif <- est_rif_quantiles(dep_var = dep_var, weights = weights, probs = probs)

  testthat::expect_equal(names(rif), c(paste0("rif_quantile_", probs)))

  testthat::expect_equal(manual_rif[,1], rif[,1])
  testthat::expect_equal(manual_rif[,2], rif[,2])
  testthat::expect_equal(manual_rif[,3], rif[,3])
  testthat::expect_equal(manual_rif[,4], rif[,4])
  testthat::expect_equal(manual_rif[,5], rif[,5])
  testthat::expect_equal(manual_rif[,6], rif[,6])
  testthat::expect_equal(manual_rif[,7], rif[,7])
  testthat::expect_equal(manual_rif[,8], rif[,8])
  testthat::expect_equal(manual_rif[,9], rif[,9])
})



# RIF of variance
testthat::test_that("RIF for variance correctly calculated" , {
  dep_var <- CPSmen8305$wage[1:300]
  rif_variance <- est_rif_variance(dep_var, weights = rep(1, length(dep_var)))

  testthat::expect_equal(rif_variance, (dep_var - mean(dep_var))^2)
})


# RIF of Gini
# testthat::test_that("RIF for gini correctly calculated" , {
#   dep_var <- CPSmen8305$wage[1:300]
#   rif_gini <- est_rif_gini(dep_var)
#
#   testthat::expect_equal(names(rif_gini), "rif_gini")
#   testthat::expect_equal(rif_gini$rif_gini, (dep_var - mean(dep_var))^2)
# })

# RIF Estimation Wrapper
testthat::test_that("RIF at quantiles correctly calculated with Wrapper" , {
  dep_var <- CPSmen8305$wage[1:300]
  weights <- CPSmen8305$weights[1:300]
  probs <- seq(1:9)/10

  # calculation with function
  rif <- est_rif_quantiles(dep_var = dep_var, weights = weights, probs = probs)
  rif_wrapper <-  est_rif(functional = "quantiles", dep_var = dep_var, weights = weights, probs = probs)

  testthat::expect_equal(rif, rif_wrapper)
})


testthat::test_that("RIF at variance correctly calculated with Wrapper" , {
  dep_var <- CPSmen8305$wage[1:300]
  weights <- CPSmen8305$weights[1:300]

  # calculation with function
  rif <- est_rif_variance(dep_var = dep_var, weights = weights)
  rif_wrapper <-  est_rif(functional = "variance", dep_var = dep_var, weights = weights)

  testthat::expect_equal(rif, rif_wrapper)
})


testthat::test_that("RIF with custom quantiles function correctly calculated" , {
  test_dep_var <- CPSmen8305$wage[1:300]
  test_weights <- CPSmen8305$weights[1:300]

  # calculation with function
  rif <-  est_rif(functional = "quantiles",
                  dep_var = test_dep_var,
                  weights = test_weights,
                  probs = seq(1:9)/10)

  # custom function
  custom_quantiles_function <- function(dep_var, custom_quantiles, weights, ...){
    est_rif_quantile <- function(quantile, dep_var, weights, density) {
      weighted_quantile <- Hmisc::wtd.quantile(x = dep_var,  weights = weights, probs = quantile)
      density_at_quantile <- approx(x = density$x, y = density$y, xout = weighted_quantile)$y
      rif <- weighted_quantile + (quantile - as.numeric(dep_var <= weighted_quantile)) / density_at_quantile
      return(rif)
    }

    weights <- check_weights(dep_var, weights)
    density <- density(x = dep_var, weights = weights/sum(weights, na.rm = TRUE), ...)
    rif <- sapply(X = custom_quantiles, FUN = est_rif_quantile, dep_var = dep_var, weights = weights, density = density)
    rif <- data.frame(rif)
    names(rif) <- paste0("rif_quantile_", custom_quantiles)
    return(rif)
  }
  rif_custom <-  est_rif(functional = "custom",
                         dep_var = test_dep_var,
                         custom_rif_function = custom_quantiles_function,
                         custom_quantiles = seq(1:9)/10,
                         weights = test_weights)
  testthat::expect_equal(rif, rif_custom)
})


testthat::test_that("RIF with custom variance function correctly calculated" , {
  dep_var <- CPSmen8305$wage[1:300]
  weights <- CPSmen8305$weights[1:300]

  # calculation with function
  rif <-  est_rif(functional = "variance", dep_var = dep_var, weights = weights)

  # custom function
  custom_variance_function <- function(dep_var, weights){
    weights <- check_weights(dep_var, weights = weights)
    weighted_mean <- weighted.mean(x = dep_var, w = weights)
    rif <- (dep_var - weighted_mean)^2
    return(rif)
  }
  rif_custom <-  est_rif(functional = "custom",
                         dep_var = dep_var,
                         custom_rif_function = custom_variance_function,
                         weights = weights)

  testthat::expect_equal(rif, rif_custom)
})


