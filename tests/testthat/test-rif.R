# RIF of probs
testthat::test_that("RIF for several quantiles correctly calculated", {
  dep_var <- men8385$wage[1:300]
  weights <- men8385$weights[1:300]
  probs <- seq(1:9) / 10
  density_of_dep_var <- density(x = dep_var, weights = weights / sum(weights, na.rm = TRUE))


  # manual calculation
  weighted_quantiles <- Hmisc::wtd.quantile(x = dep_var, weights = weights, probs = probs)
  density_at_quantiles <- approx(x = density_of_dep_var$x, y = density_of_dep_var$y, xout = weighted_quantiles)$y
  manual_rif <- sapply(X = probs, FUN = get_rif_quantile, dep_var = dep_var, weights = weights)

  # calculation with function
  rif <- get_rif_quantiles(dep_var = dep_var, weights = weights, probs = probs)

  testthat::expect_equal(names(rif), c(paste0("rif_quantile_", probs), "weights"))

  testthat::expect_equal(manual_rif[, 1], rif[, 1])
  testthat::expect_equal(manual_rif[, 2], rif[, 2])
  testthat::expect_equal(manual_rif[, 3], rif[, 3])
  testthat::expect_equal(manual_rif[, 4], rif[, 4])
  testthat::expect_equal(manual_rif[, 5], rif[, 5])
  testthat::expect_equal(manual_rif[, 6], rif[, 6])
  testthat::expect_equal(manual_rif[, 7], rif[, 7])
  testthat::expect_equal(manual_rif[, 8], rif[, 8])
  testthat::expect_equal(manual_rif[, 9], rif[, 9])
})



# RIF of variance
testthat::test_that("RIF for variance correctly calculated", {
  dep_var <- men8385$wage[1:300]
  rif_variance <- get_rif_variance(dep_var, weights = rep(1, length(dep_var)))

  testthat::expect_equal(names(rif_variance), c("rif_variance", "weights"))
  testthat::expect_equal(rif_variance$rif_variance, (dep_var - mean(dep_var))^2)
})


# RIF of Gini

## Helper Functions
testthat::test_that("integrate_generalized_lorenz_curve() does not throw an error", {
  dep_var <- men8385$wage[1:300]
  weights <- men8385$weights[1:300]

  integrate_generalized_lorenz_curve <-
    integrate_generalized_lorenz_curve(
      dep_var = dep_var,
      weights = weights
    )

  testthat::expect_error(integrate_generalized_lorenz_curve, NA)
})

testthat::test_that("compute_gini() does not throw an error", {
  dep_var <- men8385$wage[1:300]
  weights <- men8385$weights[1:300]

  gini_coefficient <- compute_gini(
    dep_var = dep_var,
    weights = weights
  )

  testthat::expect_error(compute_gini, NA)
})

testthat::test_that("RIF for gini correctly calculated", {
  dep_var <- men8385$wage[1:300]
  weights <- men8385$weights[1:300]

  rif_gini <- get_rif_gini(dep_var = dep_var, weights = weights)

  testthat::expect_error(rif_gini, NA)
})

# RIF interquantile range
testthat::test_that("RIF for interquantile range correctly calculated", {
  dep_var <- men8385$wage[1:300]
  weights <- men8385$weights[1:300]

  rif_iq_range <- get_rif_interquantile_range(
    dep_var = dep_var,
    weights = weights,
    probs = c(0.1, 0.9)
  )

  testthat::expect_error(rif_iq_range, NA)
})

# RIF interquantile ratio
testthat::test_that("RIF for interquantile ratio correctly calculated", {
  dep_var <- men8385$wage[1:300]
  weights <- men8385$weights[1:300]

  rif_iq_ratio <- get_rif_interquantile_ratio(
    dep_var = dep_var,
    weights = weights,
    probs = c(0.1, 0.9)
  )

  testthat::expect_error(rif_iq_ratio, NA)
})


# RIF Estimation Wrapper
testthat::test_that("RIF for mean correctly calculated with Wrapper", {
  dep_var <- men8385$wage[1:300]
  rif_mean <- get_rif_mean(dep_var)

  rif_mean_wrapper <- get_rif(statistic = "mean", dep_var = dep_var)

  testthat::expect_equal(names(rif_mean_wrapper), names(rif_mean))
  testthat::expect_equal(rif_mean, rif_mean_wrapper)
})

testthat::test_that("RIF at quantiles correctly calculated with Wrapper", {
  dep_var <- men8385$wage[1:300]
  weights <- men8385$weights[1:300]
  probs <- seq(1:9) / 10

  # calculation with function
  rif <- get_rif_quantiles(dep_var = dep_var, weights = weights, probs = probs)
  rif_wrapper <- get_rif(statistic = "quantiles", dep_var = dep_var, weights = weights, probs = probs)

  testthat::expect_equal(rif, rif_wrapper)
})


testthat::test_that("RIF at variance correctly calculated with Wrapper", {
  dep_var <- men8385$wage[1:300]
  weights <- men8385$weights[1:300]

  # calculation with function
  rif <- get_rif_variance(dep_var = dep_var, weights = weights)
  rif_wrapper <- get_rif(statistic = "variance", dep_var = dep_var, weights = weights)

  testthat::expect_equal(names(rif_wrapper), names(rif))
  testthat::expect_equal(rif, rif_wrapper)
})


testthat::test_that("RIF for gini correctly calculated with Wrapper", {
  dep_var <- men8385$wage[1:300]
  weights <- men8385$weights[1:300]

  # calculation with function
  rif <- get_rif_gini(dep_var = dep_var, weights = weights)
  rif_wrapper <- get_rif(statistic = "gini", dep_var = dep_var, weights = weights)

  testthat::expect_equal(names(rif_wrapper), names(rif))
  testthat::expect_equal(rif, rif_wrapper)
})


testthat::test_that("RIF for interquantile range correctly calculated with Wrapper", {
  dep_var <- men8385$wage[1:300]
  weights <- men8385$weights[1:300]

  # calculation with function
  rif <- get_rif_interquantile_range(dep_var = dep_var, weights = weights, probs = c(0.1, 0.9))
  rif_wrapper <- get_rif(statistic = "interquantile_range", dep_var = dep_var, weights = weights, probs = c(0.1, 0.9))

  testthat::expect_equal(names(rif_wrapper), names(rif))
  testthat::expect_equal(rif, rif_wrapper)
})

testthat::test_that("RIF for interquantile ratio correctly calculated with Wrapper", {
  dep_var <- men8385$wage[1:300]
  weights <- men8385$weights[1:300]

  # calculation with function
  rif <- get_rif_interquantile_ratio(dep_var = dep_var, weights = weights, probs = c(0.1, 0.9))
  rif_wrapper <- get_rif(statistic = "interquantile_ratio", dep_var = dep_var, weights = weights, probs = c(0.1, 0.9))

  testthat::expect_equal(names(rif_wrapper), names(rif))
  testthat::expect_equal(rif, rif_wrapper)
})


# Custom Function
testthat::test_that("RIF with custom mean function correctly calculated", {
  dep_var <- men8385$wage[1:300]

  # calculation with function
  rif <- get_rif_mean(dep_var = dep_var)

  # custom function
  custom_mean <- function(dep_var, weights = NULL, probs = NULL) {
    rif <- as.data.frame(dep_var)
    names(rif) <- "rif_mean"
    return(rif)
  }
  rif_custom <- get_rif(
    statistic = "custom",
    dep_var = dep_var,
    custom_rif_function = custom_mean
  )

  testthat::expect_equal(names(rif_custom), names(rif))
  testthat::expect_equal(rif, rif_custom)
})


testthat::test_that("RIF with custom variance function correctly calculated", {
  dep_var <- men8385$wage[1:300]
  weights <- men8385$weights[1:300]

  # calculation with function
  rif <- get_rif(statistic = "variance", dep_var = dep_var, weights = weights)

  # custom function
  custom_variance_function <- function(dep_var, weights, probs = NULL) {
    weights <- check_weights(dep_var, weights = weights)
    weighted_mean <- weighted.mean(x = dep_var, w = weights)
    rif <- (dep_var - weighted_mean)^2
    rif <- data.frame(rif, weights)
    names(rif) <- c("rif_variance", "weights")
    return(rif)
  }
  rif_custom <- get_rif(
    statistic = "custom",
    dep_var = dep_var,
    custom_rif_function = custom_variance_function,
    weights = weights
  )

  testthat::expect_equal(names(rif_custom), names(rif))
  testthat::expect_equal(rif, rif_custom)
})
