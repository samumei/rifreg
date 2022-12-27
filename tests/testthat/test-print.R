test_that("Generic print method does not throw an error", {
  data <- CPSmen8305[1:300,]

  rifreg_object <- est_rifreg(formula = log(wage) ~ union + age,
                          data = data,
                          functional = "quantiles",
                          custom_rif_function = NULL,
                          probs = 0.5,
                          weights = weights,
                          bootstrap = FALSE,
                          bootstrap_iterations = 100,
                          cores = 1)

  expect_error(capture.output(print(rifreg_object)), NA)


})
