#' List output variables of a Stan model
#'
#' @description Works with transformed and prettified MIR code.
#' @inheritParams format_code
#' @inheritParams stanc3
#' @return A data frame with two columns: Name and Type.
#' @name list_output
NULL

#' @export
#' @describeIn list_output Lists all parameters.
parameters <- function(code = "", file = NULL, verbose = FALSE) {
  ov <- mir_block("output_vars", code, file, verbose)
  split_mir(ov, "\\sparameters")
}

#' @export
#' @describeIn list_output Lists all transformed parameters.
transformed_parameters <- function(code = "", file = NULL, verbose = FALSE) {
  ov <- mir_block("output_vars", code, file, verbose)
  split_mir(ov, "transformed_parameters")
}

#' @export
#' @describeIn list_output Lists all generated quantities.
generated_quantities <- function(code = "", file = NULL, verbose = FALSE) {
  ov <- mir_block("output_vars", code, file, verbose)
  split_mir(ov, "generated_quantities")
}

split_mir <- function(mir, opening, closing = "[//]") {
  df <- matrix(nrow = 0, ncol = 3)
  mir <- indent_code(mir, spaces = 0)
  rem <- gsub("\n", " ", mir)
  i1 <- 0
  while (!is.na(i1)) {
    i1 <- stringr::str_locate(rem, pattern = opening)[1]
    i2 <- stringr::str_locate(rem, pattern = closing)[1]
    line <- substr(rem, i1, i2 - 3)
    if (!is.na(line)) {
      if (nchar(line) > 0) {
        parts <- strsplit(line, split = " ")[[1]]
        P <- length(parts)
        categ <- parts[1]
        type <- paste(parts[2:(P - 1)], collapse = " ")
        name <- parts[P]
        r <- c(categ, name, type)
        df <- rbind(df, r)
      }
    }
    rem <- substr(rem, i2 + 2, nchar(rem))
  }

  # Return
  df <- data.frame(df)
  colnames(df) <- c("Group", "Name", "Type")
  rownames(df) <- NULL
  return(df[, 2:3])
}

#' List all output or input variables of a Stan model
#'
#' @inheritParams format_code
#' @inheritParams stanc3
#' @return transformed and prettified MIR code
#' @name list_vars
NULL

#' @describeIn list_vars Lists all parameters, transformed parameters and
#' generated quantities.
output_vars <- function(code = "", file = NULL, verbose = FALSE) {
  mir_block("output_vars", code, file, verbose)
}

#' @describeIn list_vars Lists all data.
input_vars <- function(code = "", file = NULL, verbose = FALSE) {
  mir_block("input_vars", code, file, verbose)
}

mir_block <- function(block, code = "", file = NULL,
                      verbose = FALSE) {
  code <- full_stan_code(code, file)
  args <- c("--debug-transformed-mir-pretty")
  a <- stanc3(code, print_stdout = FALSE, args = args)$stdout
  blk <- find_block(a, block, FALSE)$middle
  blk <- gsub("\r", "", blk) # remove carriage returns
  return(blk)
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
