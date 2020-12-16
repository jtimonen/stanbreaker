library(stanbreaker)
library(cmdstanr)

context("Analysing divergences")

# We build and fit this model here so we can use it in all tests
filename <- system.file("extdata", "model_8schools.stan",
  package = "stanbreaker"
)

# Build model
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

# Fit model
schools_fit <- model$sample(data = stanbreaker::data_8schools)

# Example test
test_that("fit_divergences works and returns a CmdStanFit", {
  div_fit <- fit_divergences(schools_fit)
  expect_true("CmdStanFit" %in% class(div_fit))
})

# - We could write a new test here that does a different thing with schools_fit
# - We don't need to recompile lr.stan or model_8schools.stan when running them
#
# test_that("fit_divergences works and returns a CmdStanFit", {
#  div_fit <- fit_divergences(schools_fit)
#  expect_true("CmdStanFit" %in% class(div_fit))
# })

# Destroy the executables that were created
fn1 <- system.file("extdata", "model_8schools", package = "stanbreaker")
fn2 <- system.file("extdata", "lr", package = "stanbreaker")
file.remove(fn1)
file.remove(fn2)
