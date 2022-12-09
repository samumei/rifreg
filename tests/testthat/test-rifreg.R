# RIF Regression

testthat::test_that("RIF regression function does not throw an error" , {
  data <- CPSmen8305[1:300,]

  expect_error(est_rifreg(formula = log(wage) ~ union + age,
                          data = data,
                          functional = "quantiles",
                          custom_functional = NULL,
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
                          custom_functional = NULL,
                          quantiles = c(0.1, 0.5, 0.9),
                          weights = weights,
                          bootstrap = TRUE,
                          bootstrap_iterations = 100,
                          cores = 1,
                          model = TRUE),
               NA)
})


testthat::test_that("rifreg_detail() works properly" , {
  data <- CPSmen8305[1:300,]



  rifreg_detail <- rifreg_detail(formula = log(wage) ~ union + age,
                          data_used = data_used,
                          functional = "quantiles",
                          custom_functional = NULL,
                          quantiles = c(0.5, 0.75),
                          weights = weights,
                          bootstrap = FALSE,
                          bootstrap_iterations = 100,
                          cores = 1,
                          model = TRUE)

  # with bootstrap and several quantiles
  expect_error(est_rifreg(formula = log(wage) ~ union + age,
                          data = data,
                          functional = "quantiles",
                          custom_functional = NULL,
                          quantiles = c(0.1, 0.5, 0.9),
                          weights = weights,
                          bootstrap = TRUE,
                          bootstrap_iterations = 100,
                          cores = 1,
                          model = TRUE),
               NA)
})
