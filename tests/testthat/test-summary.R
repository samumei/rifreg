test_that("Generic summary method does not throw an error", {
  data <- men8385[1:300, ]
  weights <- men8385$weights[1:300]

  rifreg_object <- rifreg(
    formula = log(wage) ~ union + age,
    data = data,
    statistic = "quantiles",
    custom_rif_function = NULL,
    probs = seq(0.1, 0.9, 0.1),
    weights = weights,
    bootstrap = FALSE,
    bootstrap_iterations = 100,
    cores = 1
  )

  expect_error(capture.output(summary(rifreg_object)), NA)
})
