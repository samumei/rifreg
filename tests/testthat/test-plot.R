test_that("Generic plot method does not throw an error", {
  data <- men8385[1:300,]
  weights <- men8385$weights[1:300]

  rifreg_object <- est_rifreg(formula = log(wage) ~ union + age,
                              data = data,
                              statistic = "quantiles",
                              custom_rif_function = NULL,
                              probs = seq(0.1, 0.9, 0.1),
                              weights = weights,
                              bootstrap = FALSE,
                              bootstrap_iterations = 0,
                              cores = 1)

  expect_error(plot(rifreg_object), NA)
})


test_that("Generic plot method generates a plot", {
  data <- men8385[1:300,]
  weights <- men8385$weights[1:300]

  rifreg_object <- est_rifreg(formula = log(wage) ~ union + age,
                              data = data,
                              statistic = "quantiles",
                              custom_rif_function = NULL,
                              probs = seq(0.1, 0.9, 0.1),
                              weights = weights,
                              bootstrap = FALSE,
                              bootstrap_iterations = 100,
                              cores = 1)

  rifreg_plot <- plot(rifreg_object)
  expect_equal(class(rifreg_plot), c("gg", "ggplot"))
})

test_that("Generic plot method generates a plot with SE", {
  data <- men8385[1:300,]
  weights <- men8385$weights[1:300]

  rifreg_object <- est_rifreg(formula = log(wage) ~ union + age,
                              data = data,
                              statistic = "quantiles",
                              custom_rif_function = NULL,
                              probs = seq(0.1, 0.9, 0.1),
                              weights = weights,
                              bootstrap = TRUE,
                              bootstrap_iterations = 100,
                              cores = 1)

  rifreg_plot <- plot(rifreg_object)
  expect_equal(class(rifreg_plot), c("gg", "ggplot"))
})


test_that("Generic plot method generates a plot with different alpha", {
  data <- men8385[1:300,]
  weights <- men8385$weights[1:300]

  rifreg_object <- est_rifreg(formula = log(wage) ~ union + age,
                              data = data,
                              statistic = "quantiles",
                              custom_rif_function = NULL,
                              probs = seq(0.1, 0.9, 0.1),
                              weights = weights,
                              bootstrap = TRUE,
                              bootstrap_iterations = 100,
                              cores = 1)

  rifreg_plot <- plot(rifreg_object, alpha = 0.1)
  expect_equal(class(rifreg_plot), c("gg", "ggplot"))
})


test_that("Generic plot method generates a plot with specific variables", {
  data <- men8385[1:300,]
  weights <- men8385$weights[1:300]

  rifreg_object <- est_rifreg(formula = log(wage) ~ union + age,
                              data = data,
                              statistic = "quantiles",
                              custom_rif_function = NULL,
                              probs = seq(0.1, 0.9, 0.1),
                              weights = weights,
                              bootstrap = TRUE,
                              bootstrap_iterations = 100,
                              cores = 1)

  rifreg_plot <- plot(rifreg_object, varselect = c("age", "unionyes"))
  expect_equal(class(rifreg_plot), c("gg", "ggplot"))
})
