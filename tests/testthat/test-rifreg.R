testthat::test_that("RIF regression function does not throw an error", {
  data <- men8385[1:300, -length(colnames(men8385))]

  rifreg <- rifreg(
    formula = log(wage) ~ union +
      nonwhite +
      married +
      education +
      experience,
    data = men8385[1:300, ],
    statistic = "quantiles",
    probs = seq(0.1, 0.9, 0.1),
    weights = NULL,
    bootstrap = T
  )

  expect_error(rifreg, NA)
  expect_equal(rifreg[["rif"]][["weights"]], rep(1, length(data$union)))



  # with bootstrap and several quantiles
  expect_error(
    rifreg(
      formula = log(wage) ~ union + age,
      data = data,
      statistic = "quantiles",
      custom_rif_function = NULL,
      probs = c(0.1, 0.5, 0.9),
      weights = NULL,
      bootstrap = TRUE,
      bootstrap_iterations = 100,
      cores = 1
    ),
    NA
  )
})

testthat::test_that("RIF regression function does not throw an error with weights in df", {
  data <- men8385[1:300, ]

  rifreg <- rifreg(
    formula = log(wage) ~ union + age,
    data = data,
    statistic = "quantiles",
    probs = 0.5,
    weights = weights,
    bootstrap = FALSE,
    bootstrap_iterations = 100,
    cores = 1
  )
  expect_error(rifreg, NA)
  expect_equal(rifreg[["rif"]][["weights"]], data$weights)


  # with bootstrap and several quantiles
  expect_error(
    rifreg(
      formula = log(wage) ~ union + age,
      data = data,
      statistic = "quantiles",
      custom_rif_function = NULL,
      probs = c(0.1, 0.5, 0.9),
      weights = weights,
      bootstrap = TRUE,
      bootstrap_iterations = 100,
      cores = 1
    ),
    NA
  )
})

testthat::test_that("RIF regression function does not throw an error with weights as vector", {
  data <- men8385[1:300, -length(colnames(men8385))]
  test_weights <- men8385$weights[1:300]

  rifreg <- rifreg(
    formula = log(wage) ~ union + age,
    data = data,
    statistic = "quantiles",
    probs = 0.5,
    weights = test_weights,
    bootstrap = FALSE,
    bootstrap_iterations = 100,
    cores = 1
  )
  expect_error(rifreg, NA)
  expect_equal(rifreg[["rif"]][["weights"]], test_weights)


  # with bootstrap and several quantiles
  expect_error(
    rifreg(
      formula = log(wage) ~ union + age,
      data = data,
      statistic = "quantiles",
      custom_rif_function = NULL,
      probs = c(0.1, 0.5, 0.9),
      weights = test_weights,
      bootstrap = TRUE,
      bootstrap_iterations = 100,
      cores = 1
    ),
    NA
  )
})


testthat::test_that("RIF regression function for variance does throw an error", {
  data <- men8385[1:300, ]

  rifreg <- rifreg(
    formula = log(wage) ~ union + age,
    data = data,
    statistic = "variance",
    weights = weights
  )
  expect_error(rifreg, NA)
  expect_equal(rifreg[["rif"]][["weights"]], data$weights)
  expect_equal(class(rifreg), c("rifreg", "lm"))
})

testthat::test_that("RIF regression function for gini does throw an error", {
  data <- men8385[1:300, ]

  rifreg <- rifreg(
    formula = log(wage) ~ union + age,
    data = data,
    statistic = "gini",
    weights = weights
  )
  expect_error(rifreg, NA)
  expect_equal(rifreg[["rif"]][["weights"]], data$weights)
  expect_equal(class(rifreg), c("rifreg", "lm"))
})

testthat::test_that("RIF regression function for interquantile range does throw an error", {
  data <- men8385[1:300, ]
  rifreg <- rifreg(
    formula = log(wage) ~ union + age,
    data = data,
    statistic = "interquantile_range",
    weights = weights,
    probs = c(0.1, 0.9)
  )
  expect_error(rifreg, NA)
  expect_equal(rifreg[["rif"]][["weights"]], data$weights)
  expect_equal(class(rifreg), c("rifreg", "lm"))
})

testthat::test_that("RIF regression function for interquantile ratio does throw an error", {
  data <- men8385[1:300, ]

  rifreg <- rifreg(
    formula = log(wage) ~ union + age,
    data = data,
    statistic = "interquantile_ratio",
    weights = weights,
    probs = c(0.1, 0.9)
  )
  expect_error(rifreg, NA)
  expect_equal(rifreg[["rif"]][["weights"]], data$weights)
  expect_equal(class(rifreg), c("rifreg", "lm"))
})



testthat::test_that("RIF regression function does not throw an error with custom functions", {
  test_data <- men8385[1:300, ]
  test_weights <- men8385$weights[1:300]

  custom_variance_function <- function(dep_var, weights, probs = NULL) {
    weights <- check_weights(dep_var, weights = weights)
    weighted_mean <- weighted.mean(x = dep_var, w = weights)
    rif <- (dep_var - weighted_mean)^2
    rif <- data.frame(rif, weights)
    names(rif) <- c("rif_variance", "weights")
    return(rif)
  }

  expect_error(
    rifreg(
      formula = log(wage) ~ union + age,
      data = test_data,
      statistic = "custom",
      custom_rif_function = custom_variance_function,
      probs = NULL,
      bootstrap = FALSE,
      cores = 1,
      weights = test_weights
    ),
    NA
  )

  custom_quantiles_function <- function(dep_var, probs, weights, ...) {
    get_rif_quantile <- function(quantile, dep_var, weights, density) {
      weighted_quantile <- Hmisc::wtd.quantile(x = dep_var, weights = weights, probs = quantile)
      density_at_quantile <- approx(x = density$x, y = density$y, xout = weighted_quantile)$y
      rif <- weighted_quantile + (quantile - as.numeric(dep_var <= weighted_quantile)) / density_at_quantile
      return(rif)
    }

    density <- density(x = dep_var, weights = weights / sum(weights, na.rm = TRUE), ...)
    rif <- sapply(X = probs, FUN = get_rif_quantile, dep_var = dep_var, weights = weights, density = density)
    rif <- data.frame(rif, weights)
    names(rif) <- c(paste0("rif_quantile_", probs), "weights")
    return(rif)
  }

  expect_error(
    rifreg(
      formula = log(wage) ~ union + age,
      data = test_data,
      statistic = "custom",
      custom_rif_function = custom_quantiles_function,
      probs = c(0.1, 0.5, 0.9),
      bootstrap = FALSE,
      cores = 1,
      weights = test_weights
    ),
    NA
  )
})

testthat::test_that("RIF regression function does not throw an error with missings", {
  data <- men8385[1:300, -length(colnames(men8385))]

  data$wage[sample.int(length(data$wage), 5)] <- NA
  data$union[sample.int(length(data$wage), 5)] <- NA

  rifreg <- rifreg(
    formula = log(wage) ~ union +
      nonwhite +
      married +
      education +
      experience,
    data = data,
    statistic = "quantiles",
    probs = seq(0.1, 0.9, 0.1),
    weights = NULL,
    bootstrap = FALSE
  )
  expect_error(rifreg, NA)
})

testthat::test_that("RIF regression function does not throw an error with different na.action", {
  data <- men8385[1:300, -length(colnames(men8385))]

  rifreg <- rifreg(
    formula = log(wage) ~ union +
      nonwhite +
      married +
      education +
      experience,
    data = men8385[1:300, ],
    statistic = "quantiles",
    probs = seq(0.1, 0.9, 0.1),
    na.action = na.fail,
    weights = NULL,
    bootstrap = FALSE
  )
  expect_error(rifreg, NA)
  expect_equal(rifreg[["rif"]][["weights"]], rep(1, length(data$union)))
})

testthat::test_that("RIF regression function does not throw an error with custom top income share function", {
  ffl_model2 <- wage ~ union + nonwhite + married + education + experience

  # custom RIF function for top 10% percent income share
  custom_top_inc_share <- function(dep_var,
                                   weights,
                                   probs = 0.1) {
    probs <- 1 - probs
    weighted_mean <- weighted.mean(
      x = dep_var,
      w = weights
    )
    weighted_quantile <- Hmisc::wtd.quantile(
      x = dep_var,
      weights = weights,
      probs = probs
    )
    lorenz_ordinate <- sum(dep_var[which(dep_var <= weighted_quantile)] *
      weights[which(dep_var <= weighted_quantile)]) /
      sum(dep_var * weights)
    if_lorenz_ordinate <- -(dep_var / weighted_mean) * lorenz_ordinate +
      ifelse(dep_var < weighted_quantile,
        dep_var - (1 - probs) * weighted_quantile,
        probs * weighted_quantile
      ) / weighted_mean
    rif_top_income_share <- (1 - lorenz_ordinate) - if_lorenz_ordinate
    rif <- data.frame(rif_top_income_share, weights)
    names(rif) <- c("rif_top_income_share", "weights")
    return(rif)
  }

  fit_top_10 <- rifreg(ffl_model2,
    data = men8385,
    weights = weights,
    statistic = "custom",
    custom_rif_function = custom_top_inc_share,
    probs = 0.1
  )
  expect_error(fit_top_10, NA)
})


# # The following test does not work in devtools::check()
# testthat::test_that("RIF regression function does not throw an error with several cores" , {
#   data <- men8385[1:300,]
#   weights <- men8385$weights[1:300]
#
#   # with bootstrap and several quantiles
#   expect_error(rifreg(formula = log(wage) ~ union + age,
#                           data = data,
#                           statistic = "quantiles",
#                           custom_rif_function = NULL,
#                           probs = c(0.1, 0.5, 0.9),
#                           weights = weights,
#                           bootstrap = TRUE,
#                           bootstrap_iterations = 100,
#                           cores = 4),
#                NA)
# })
