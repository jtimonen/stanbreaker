library(stanbreaker)

context("Code formatting")

# Read example code
fn1 <- system.file("extdata", "model_8schools.stan", package = "stanbreaker")
fn2 <- system.file("extdata", "model_8schools_bad.stan",
  package = "stanbreaker"
)
code1 <- read_file(file = fn1)
code2 <- read_file(file = fn2)


test_that("code can be formatted from file or text with stanc", {
  a <- format_code(file = fn1, use_stanc = TRUE)
  b <- format_code(code = code1, use_stanc = TRUE)
  expect_equal(a, b)
})

test_that("code can be formatted without stanc", {
  a <- format_code(file = fn1, use_stanc = FALSE)
  expect_equal(a, code1)
})

test_that("code can be formatted without stanc", {
  a <- format_code(file = fn1, place_includes = TRUE, use_stanc = FALSE)
  expect_equal(a, code1)
  b1 <- format_code(file = fn2, place_includes = TRUE, use_stanc = FALSE)
  b2 <- format_code(file = fn2, place_includes = FALSE, use_stanc = FALSE)
  expect_equal(nchar(b1), 518)
  expect_equal(nchar(b2), 449)
})
