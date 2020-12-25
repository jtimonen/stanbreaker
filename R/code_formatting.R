#' Format code
#'
#' @description This formatting function
#' \itemize{
#'   \item indents code recursively based on opening and closing curly brackets
#'   \item removes unnesessacy whitespace and line breaks
#'   \item adds some needed linebreaks when missing
#' }
#' @export
#' @param code code as a string
#' @param spaces number of spaces to add to indenting after each opening
#' curly bracket (default = 2)
#' @return updated code as a string
#' @family code editing functions
format_code <- function(code, spaces = 2) {
  code <- trim_code(code) # trim
  code <- format_part(code, spaces, 0) # indent
  code <- trim_code(code) # trim
  code <- paste0(code, "\n") # empty last line
  return(code)
}

#' Helper functions for code formatting
#'
#' @description
#' \itemize{
#'   \item \code{format_part} is a helper function used recursively by
#'   \code{format_code}
#'   \item \code{justlify_left} is the base case of the recursion, which
#'   justifies the code left at a given number of indentation spaces
#'   \item \code{justify_line} first trims a line by removing leading and
#'   trailing whitespace, and then justifies it correctly
#'   \item \code{locate_opening_bracket} finds the first opening bracket
#'   which is immediately followed by a line break
#'   \item \code{locate_closing_bracket} finds the correct closing
#'   bracket
#' }
#' @inheritParams format_code
#' @param indent base level of indentation
#' @param line line to format
#' @family code formatting helper functions
#' @return updated code as a string
#' @name format_code_helpers

#' @rdname format_code_helpers
format_part <- function(code, spaces, indent) {
  L <- nchar(code)
  if (L == 0) {
    return(code)
  }

  # First part
  idx_op <- locate_opening_bracket(code)
  split <- split_code(code, idx_op + 1)
  part1 <- paste0(split$before, split$middle)
  part1 <- justify_left(part1, indent)
  if (split$middle == "") {
    return(part1)
  }

  # Middle and end parts
  part2 <- split$after
  idx_cl <- locate_closing_bracket(part2)
  split <- split_code(part2, idx_cl)
  part2 <- format_part(split$before, spaces, indent + spaces)
  part3 <- format_part(split$after, spaces, indent)

  # Join the parts
  closing <- justify_line("} ", indent)
  out <- paste0(part1, "\n", part2, "\n", closing, part3)
  return(out)
}

#' @rdname format_code_helpers
justify_left <- function(code, indent) {
  if (nchar(code) == 0) {
    return(code)
  }
  lines <- strsplit(code, split = "\n")[[1]]
  lines <- lapply(lines, justify_line, indent = indent)
  code <- paste(lines, collapse = "\n")
  return(code)
}

#' @rdname format_code_helpers
justify_line <- function(line, indent) {
  spaces <- paste(rep(" ", indent), collapse = "")
  line <- trimws(line, "left", whitespace = "[ \t\r]")
  line <- paste0(spaces, line)
  return(line)
}

#' @rdname format_code_helpers
locate_opening_bracket <- function(code) {
  op <- stringr::str_locate_all(code, "[{]")[[1]][, 1]
  N <- length(op)
  L <- nchar(code)
  for (i in seq_len(N)) {
    idx <- op[i]
    rem <- substr(code, idx + 1, L)
    if (substr(rem, 1, 1) == "\n") {
      return(idx)
    }
  }
  return(NA)
}

#' @rdname format_code_helpers
locate_closing_bracket <- function(code) {
  op <- stringr::str_locate_all(code, "[{]")[[1]][, 1]
  cl <- stringr::str_locate_all(code, "[}]")[[1]][, 1]
  N <- length(cl)
  for (i in seq_len(N)) {
    idx <- cl[i]
    if (sum(op < idx) < i) {
      return(idx)
    }
  }
  stop("matching closing bracket not found")
}

#' Remove trailing whitespace from each line
#'
#' @param code code to trim as a string
#' @return trimmed code
trim_code <- function(code) {
  lines <- strsplit(code, split = "\n")[[1]]
  trimmer <- function(line) {
    trimws(line, "right", whitespace = "[ \t\r]")
  }
  lines <- lapply(lines, trimmer)
  code <- paste(lines, collapse = "\n")
  return(code)
}

#' Split code at given location
#'
#' @param code code to split as a string
#' @param idx index of the character where to split
#' @return A named list with elements
#' \itemize{
#'   \item \code{before} - the part before the pattern
#'   \item \code{middle} - the character at the split location
#'   \item \code{after} - the part after the pattern
#' }
#' @family code formatting helper functions
split_code <- function(code, idx = NA) {
  L <- nchar(code)
  if (is.na(idx)) idx <- L + 1
  before <- substr(code, 1, idx - 1)
  middle <- substr(code, idx, idx)
  after <- substr(code, idx + 1, L)

  # Create the list that will be returned
  out <- list(
    before = before,
    middle = middle,
    after = after
  )

  # Check correct behaviour
  reconst <- paste0(out$before, out$middle, out$after)
  if (reconst != code) {
    msg <- "split_code didn't work correctly!"
    msg <- paste0(msg, " paste0(out$before, out$middle, out$after) doesn't")
    msg <- paste0(msg, " match the original code.")
    warning(msg)
  }
  return(out)
}

#' Get full Stan code from a .stan file, possibly containing #includes
#'
#' @export
#' @param filename name of the main \code{.stan} file
#' @param spaces number of spaces to use when indenting code (default = 2)
#' @param verbose should some messages be printed?
#' @return the full program code as a string, formatted using
#' \code{format_code}
#' @family code editing functions
get_stan_code <- function(filename, spaces = 2, verbose = FALSE) {
  lines <- readLines_info(filename, verbose)
  parent_dir <- normalizePath(dirname(filename))
  code <- place_includes(lines, parent_dir, verbose)
  code <- format_code(code, spaces = spaces)
  return(code)
}

#' Read lines of a file and show an informational message if verbose is true
#'
#' @param file path to file
#' @param verbose should the message be printed
#' @return same as \code{readLines(file)}
readLines_info <- function(file, verbose) {
  if (verbose) {
    cat(paste0("reading lines from '", file, "'\n"))
  }
  readLines(con = file)
}

#' A helper function called recursively by get_stan_code
#'
#' @param lines an array of strings, each representing one additional code line
#' @param parent_dir parent directory for the Stan files
#' @inheritParams get_stan_code
#' @return full code as a string, without \code{#include} statements
place_includes <- function(lines, parent_dir, verbose) {

  # Loop through code lines
  code <- ""
  L <- length(lines)
  for (j in seq_len(L)) {
    # Check if this line begins with #include
    line <- lines[j]
    line <- trimws(line)
    start <- substr(line, 1, 8)
    if (start == "#include") {

      # Get name of file to be included
      fn <- substr(line, 9, nchar(line))
      fn <- trimws(fn)
      fn <- file.path(parent_dir, fn)

      # Get the part to include
      to_add <- readLines_info(fn, verbose)
      to_add <- place_includes(to_add, parent_dir, verbose)
    } else {
      to_add <- lines[j]
    }

    # Add the part to add
    if (j == 1) {
      code <- to_add
    } else {
      code <- paste(code, to_add, sep = "\n")
    }
  }
  return(code)
}

#' Simplify a Stan program
#'
#' @export
#' @param code the Stan code for the program
#' @param data the supplied data list (in \code{rstan} format)
#' @return updated code as a string
#' @family code editing functions
simplify_code <- function(code, data) {
  stop("not implemented yet!")
  code <- remove_empty_forloops(code, data)
  code <- remove_size0_data(code, data)
  code <- remove_unused_functions(code, data)
  return(code)
}

#' Helper functions for Stan code simplification
#'
#' @description NOT IMPLEMENTED YET
#' \itemize{
#'   \item \code{remove_unused_functions} removes user-defined functions that
#'   are not needed
#'   \item \code{remove_empty_forloops} removes for loops that would loop from
#'   1 to 0
#'   \item \code{remove_size0_data} removes data declarations which would have
#'   a dimension of size 0
#' }
#' @inheritParams simplify_code
#' @return updated code as a string
#' @name simplify_code_helpers

#' @rdname simplify_code_helpers
remove_unused_functions <- function(code, data) {
  return(code)
}

#' @rdname simplify_code_helpers
remove_empty_forloops <- function(code, data) {
  return(code)
}

#' @rdname simplify_code_helpers
remove_size0_data <- function(code, data) {
  return(code)
}
