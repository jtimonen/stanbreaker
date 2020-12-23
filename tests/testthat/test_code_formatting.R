library(stanbreaker)

context("Code formatting functions")

test_that("additional spaces are removed", {
  code <- "   "
  expect_equal(format_code(""), "\n")
  expect_equal(format_code("   "), "\n")
  expect_equal(format_code(" hi  "), "hi\n")
})

test_that("indenting works", {
  code <- " this { is a \n piece \n} of code \n{{{{with brackets}}}}"
  correct <- paste0(
    "this {\n  is a\n  piece\n} of code {\n  {\n    {\n      ",
    "{\n        with brackets\n      }\n    }\n  }\n}\n"
  )
  expect_equal(format_code(code), correct)
})

test_that("8schools is not changed", {
  code <- model_8schools
  expect_equal(format_code(code), paste0(code, "\n"))
})

test_that("an error is thrown if there is no matching closing bracket", {
  code <- "moi{joo} juu{{j"
  err <- "matching closing bracket not found"
  expect_error(format_code(code), err)
  code <- substr(model_8schools, 1, nchar(model_8schools) - 2)
  expect_error(format_code(code), err)
})
