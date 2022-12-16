test_that("check_weights() does not throw an error", {
  dep_var <- c(1, 3, 9, 16, 3, 7, 4, 9)
  weights <- c(2, 1, 3, 4, 4, 1, 6, 3)

  testthat::expect_error(check_weights(dep_var, weights), NA)
  testthat::expect_equal(check_weights(dep_var, weights), weights)
})


test_that("check_weights() works with weights = NULL", {
  dep_var <- c(1, 3, 9, 16, 3, 7, 4, 9)

  testthat::expect_error(check_weights(dep_var, weights = NULL), NA)
  testthat::expect_equal(check_weights(dep_var, weights = NULL), rep(1, length(dep_var)))
})
