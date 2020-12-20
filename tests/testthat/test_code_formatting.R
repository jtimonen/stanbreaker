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
    "this {\n  is a\n  piece\n}\nof code\n{\n  {\n    {\n",
    "      {\n        with brackets\n      }\n    }\n  }\n}\n\n"
  )
  expect_equal(format_code(code), correct)
})
