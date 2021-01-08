library(stanbreaker)

context("Code formatting")

# Set some file paths
fn1 <- system.file("extdata", "model_8schools.stan", package = "stanbreaker")
fn2 <- system.file("extdata", "model_8schools_bad.stan",
  package = "stanbreaker"
)


test_that("code can be formatted from file or text", {
  original <- read_file(file = fn1)
  a <- format_code(file = fn1)
  b <- format_code(code = original)
  expect_equal(a, b)
})

test_that("code can be formatted without stanc", {
  original <- read_file(file = fn1)
  a <- format_code(file = fn1, place_includes = TRUE, use_stanc = FALSE)
  expect_equal(a, original)
})

test_that("code can be formatted without stanc", {
  original <- read_file(file = fn1)
  a <- format_code(file = fn1, place_includes = TRUE, use_stanc = FALSE)
  expect_equal(a, original)
  b1 <- format_code(file = fn2, place_includes = TRUE, use_stanc = FALSE)
  b2 <- format_code(file = fn2, place_includes = FALSE, use_stanc = FALSE)
  expect_equal(nchar(b1), 521)
  expect_equal(nchar(b2), 453)
})
