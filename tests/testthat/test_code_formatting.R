library(stanbreaker)

context("Code formatting functions")

test_that("additional spaces are removed", {
  code <- "   "
  expect_equal(format_code(""), "\n")
  expect_equal(format_code("   "), "\n")
  expect_equal(format_code(" hi  "), "hi\n")
})

test_that("indenting works", {
  code <- " this { \nis a \n piece \n} of code {\n{{{with brackets}}}}"
  correct <- "this {\n  is a\n  piece\n} of code {\n  {{{with brackets}}}}\n"
  expect_equal(format_code(code), correct)
})

test_that("8schools is not changed", {
  code <- model_8schools
  expect_equal(format_code(code), paste0(code, "\n"))
})

test_that("can handle a case with no matching closing bracket", {
  code <- "moi{\njoo} juu{\n{\nj"
  correct <- "moi{\n  joo} juu{\n  {\n    j\n"
  expect_equal(format_code(code), correct)
})
