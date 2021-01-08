#' List all parameters of a Stan model
#'
#' @inheritParams format_code
#' @inheritParams stanc3
#' @return program code as a formatted string
list_params <- function(code = "", file = NULL, args = c("--debug-parse"),
                        verbose = FALSE) {
  code <- full_stan_code(code, file)
  b1 <- get_block(code, "functions")
  b2 <- get_block(code, "data")
  b3 <- get_block(code, "transformed data")
  b4 <- get_block(code, "parameters")
  code <- paste(b1, b2, b3, b4, sep = "\n")
  if (verbose) cat(code)
  a <- stanc3(code, print_stdout = FALSE, args = args)
  return(a$stdout)
}

#' Get names of all user-defined functions
#'
#' @inheritParams format_code
#' @param ... additional arguments to \code{\link{full_stan_code}}
#' @family code analysis functions
#' @return a data frame
list_functions <- function(code, ...) {
  code <- full_stan_code(code)
  r <- find_functions(code)
  df <- count_functions_usage(r, code)
  return(df)
}
