library(stanbreaker)
library(rstan)
library(cmdstanr)

context("Analysing divergences")

# We build and fit this model here so we can use it in all tests
filename <- system.file("extdata", "model_8schools.stan",
  package = "stanbreaker"
)

# Buiild model
tryCatch(
  {
    model <- cmdstanr::cmdstan_model(filename)
  },
  error = function(e) {
    cat(
      " Unable to build model. Probably a problem with cmdstanr",
      "installation. Try to run this code on its own to see the error.\n"
    )
    print(e)
  }
)

schools_fit <- model$sample(data = stanbreaker::data_8schools)
schools_fit_latent <- model$sample(
  data = stanbreaker::data_8schools,
  save_latent_dynamics = TRUE
)

# Example test
test_that("fit_divergences works and returns a CmdStanFit", {
  div_fit <- fit_divergences(schools_fit)
  expect_true("CmdStanFit" %in% class(div_fit))
})

test_that("cmdstan_unconstrained_draws extracts a posterior draws df", {
  udraws <- cmdstan_unconstrained_draws(schools_fit_latent)
  expect_true(posterior::is_draws_df(udraws))
})

test_that("cmdstan_momentum_draws extracts a posterior draws df", {
  mdraws <- cmdstan_momentum_draws(schools_fit_latent)
  expect_true(posterior::is_draws_df(mdraws))
})

test_that("make_rstan_fit makes an rstan fit", {
  fit <- rstan_make_fit(filename, data = stanbreaker::data_8schools)
  expect_true("stanfit" %in% class(fit))
})

rstan_fit <- stanbreaker::rstan_make_fit(filename, data = stanbreaker::data_8schools)

test_that("rstan_hessian computes a hessian", {
  hessian <- rstan_hessian(rstan_fit, 0.1 * c(-5:4))

  hessian_ref <- matrix(c(
    -17.844327, -19.5847602, 2.2255409, 2.225541, 2.225541, 2.225541, 2.225541, 2.225541, 2.225541, 2.225541,
    -19.584760, -12.6770188, 0.8902164, 1.335325, 1.780433, 2.225541, 2.670649, 3.115757, 3.560865, 4.005974,
    2.225541, 0.8902164, -2.2299854, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000,
    2.225541, 1.3353246, 0.0000000, -2.235541, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000,
    2.225541, 1.7804327, 0.0000000, 0.000000, -2.229447, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000,
    2.225541, 2.2255409, 0.0000000, 0.000000, 0.000000, -2.233805, 0.000000, 0.000000, 0.000000, 0.000000,
    2.225541, 2.6706491, 0.0000000, 0.000000, 0.000000, 0.000000, -2.237887, 0.000000, 0.000000, 0.000000,
    2.225541, 3.1157573, 0.0000000, 0.000000, 0.000000, 0.000000, 0.000000, -2.233805, 0.000000, 0.000000,
    2.225541, 3.5608655, 0.0000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, -2.235541, 0.000000,
    2.225541, 4.0059737, 0.0000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, -2.228627
  ),
  nrow = 10, ncol = 10, byrow = TRUE
  )

  expect_true("matrix" %in% class(hessian))
})

test_that("rstan_power_method computes an eigenvalue and an eigenvector", {
  out <- rstan_power_method(rstan_fit, 0.1 * c(-5:4))

  e_ref <- -37.56787
  v_ref <- c(
    0.72000031, 0.64355009, -0.06157088, -0.06967409, -0.07777161,
    -0.08588113, -0.09398174, -0.10206595, -0.11017987, -0.11825300
  )

  expect_true(!is.vector(out$e) && is.numeric(out$e))
  expect_true(is.vector(out$v) && is.numeric(out$v))
})

# Destroy the executables that were created
fn1 <- system.file("extdata", "model_8schools", package = "stanbreaker")
fn2 <- system.file("extdata", "lr", package = "stanbreaker")
file.remove(fn1)
file.remove(fn2)
