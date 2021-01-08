library(stanbreaker)

context("Code formatting")

# Set some file paths
filename <- system.file("extdata", "model_8schools.stan",
  package = "stanbreaker"
)
original <- read_file(file = filename)

test_that("code can be formatted from file or text", {
  a <- format_code(code = filename)
  b <- format_code(code = original)
  expect_equal(a, b)
})

test_that("code can be formatted without stanc", {
  a <- format_code(code = filename, place_includes = TRUE, use_stanc = FALSE)
  expect_equal(a, original)
})
