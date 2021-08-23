#################################################################################
############################ Test RIFREG Script #################################
#################################################################################

# Author: Samuel Meier
# Date:   10.08.2021

# Introduction ------------------------------

# This script tests the functions to calculate recentered influence
# function regressions in the rifreg script.


# Tests ------------------------


# Influence functions ----

# RIF at quantiles
testthat::test_that("RIF for specific quantile correctly calculated" , {
  dep_var <- c(1, 3, 9, 16)
  weights <- c(2, 1, 3, 4)
  quantile <- 0.5
  density_of_dep_var <- density(x = dep_var, weights = weights/sum(weights, na.rm = TRUE))

  # manual calculation
  weighted_quantile <- Hmisc::wtd.quantile(x = dep_var, weights = weights, probs = quantile)
  density_at_quantile <- approx(x = density_of_dep_var$x, y = density_of_dep_var$y, xout = weighted_quantile)$y
  influence_function <- (quantile - as.numeric(dep_var <= weighted_quantile)) / density_at_quantile
  manual_rif_q <- weighted_quantile + influence_function

  # calculation with function
  rif_q <- est_rif_of_quantile(dep_var = dep_var, weights = weights, quantile = quantile, density = density_of_dep_var)

  testthat::expect_equal(manual_rif_q, rif_q)
})

testthat::test_that("RIF for several quantiles correctly calculated" , {
  dep_var <- c(1, 3, 9, 16, 3, 7, 4, 9)
  quantiles <- seq(1:9)/10
  weights <- c(2, 1, 3, 4, 4, 1, 6, 3)
  density_of_dep_var <- density(x = dep_var, weights = weights/sum(weights, na.rm = TRUE))

  # manual calculation
  weighted_quantiles <- Hmisc::wtd.quantile(x = dep_var, weights = weights, probs = quantiles)
  density_at_quantiles <- approx(x = density_of_dep_var$x, y = density_of_dep_var$y, xout = weighted_quantiles)$y
  manual_rif <- sapply(X = quantiles, FUN = est_rif_of_quantile, dep_var = dep_var, weights = weights, density = density_of_dep_var)

  # calculation with function
  rif <- est_rif_of_quantiles(dep_var = dep_var, weights = weights, quantiles = quantiles)

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

# Test check_weights

# Test check_mean --> do you even need the weights or is it always the mean?
