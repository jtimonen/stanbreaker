library(stanbreaker)

context("Code analysis")

# Read example code
fn1 <- system.file("extdata", "model_8schools.stan", package = "stanbreaker")
fn2 <- system.file("extdata", "model_8schools_bad.stan",
                   package = "stanbreaker"
)
code1 <- read_file(file = fn1)
code2 <- read_file(file = fn2)


test_that("parameters can be listed", {
  a <- parameters(code = code1)
  b <- parameters(file = fn2)
  expect_equal(a, b)
  expect_equal(dim(a), c(4, 2))
})

test_that("transformed parameters can be listed", {
  a <- transformed_parameters(code = code1)
  b <- transformed_parameters(file = fn2)
  expect_equal(a, b)
  expect_equal(dim(a), c(1, 2))
  expect_equal(a$Type, "vector[n]")
})

test_that("generated quantities can be listed", {
  a <- generated_quantities(code = code1)
  b <- generated_quantities(file = fn2)
  expect_equal(a, b)
  expect_equal(dim(a), c(0, 2))
})
