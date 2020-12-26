#' Format Stan code
#'
#' @description This formatting function
#' \itemize{
#'   \item indents code based on opening and closing curly brackets
#'   \item removes unnesessacy whitespace
#' }
#' @export
#' @param code code as a string
#' @param spaces number of spaces to add to indenting after each opening
#' curly bracket (default = 2)
#' @return updated code as a string
#' @family code formatting functions
format_code <- function(code, spaces = 2) {
  code <- trim_code(code) # trim
  code <- indent_code(code, spaces, curly = TRUE) # indent
  code <- indent_code(code, spaces, curly = FALSE) # indent
  L <- nchar(code)
  if (substr(code, L, L) != "\n") {
    code <- paste0(code, "\n") # ensure empty last line
  }
  return(code)
}

#' Remove trailing whitespace from each code line
#'
#' @export
#' @param code code to trim as a string
#' @family code formatting functions
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

#' Indent code based on (curly) brackets
#'
#' @export
#' @inheritParams format_code
#' @param curly is indention done based on curly brackets?
#' @family code formatting functions
#' @return edited code
indent_code <- function(code, spaces, curly = TRUE) {
  L <- nchar(code)
  if (L == 0) {
    return(code)
  }
  lines <- strsplit(code, "\n")[[1]]
  arr <- count_indent_lines(lines, curly)
  lines <- apply_indent(lines, arr, spaces, trim = curly)
  code <- paste(lines, collapse = "\n")
  return(code)
}


#' Get full Stan code from a .stan file, possibly containing #includes
#'
#' @export
#' @param filename name of the main \code{.stan} file
#' @param spaces number of spaces to use when indenting code (default = 2)
#' @param verbose should some messages be printed?
#' @return the full program code as a string, formatted using
#' \code{format_code}
#' @family code formatting functions
get_stan_code <- function(filename, spaces = 2, verbose = FALSE) {
  lines <- readLines_info(filename, verbose)
  parent_dir <- normalizePath(dirname(filename))
  code <- place_includes(lines, parent_dir, verbose)
  code <- format_code(code, spaces = spaces)
  return(code)
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


#' Read lines of a file and show an informational message if verbose is true
#'
#' @param file path to file
#' @param verbose should the message be printed
#' @return same as \code{readLines(file)}
readLines_info <- function(file, verbose) {
  if (verbose) {
    cat(paste0("reading '", file, "'\n"))
  }
  readLines(con = file)
}


#' Count how much each line should be indented
#'
#' @param lines array of code lines
#' @inheritParams indent_code
#' @family code formatting helper functions
#' @return an integer array with same length as \code{lines}
count_indent_lines <- function(lines, curly) {
  char1 <- if (curly) "{" else "("
  char2 <- if (curly) "}" else ")"
  J <- length(lines)
  arr <- rep(0, J)
  for (j in seq_len(J)) {
    ind_curr <- 0
    ind_next <- 0
    s <- lines[j]
    s_trim <- trimws(s, whitespace = "[ \t\r]")
    if (substr(s_trim, 1, 1) == char2) {
      ind_curr <- -1
      ind_next <- 1
    }
    n_op <- stringr::str_count(s, paste0("[", char1, "]"))
    n_cl <- stringr::str_count(s, paste0("[", char2, "]"))

    ind_next <- ind_next + n_op - n_cl
    if (j + 1 <= J) arr[j + 1] <- arr[j + 1] + ind_next
    arr[j] <- arr[j] + ind_curr
  }
  return(arr)
}

#' Apply indention to code lines
#'
#' @param lines array of code lines
#' @inheritParams justify_line
#' @param arr an array returned by \code{\link{count_indent_lines}}
#' @family code formatting helper functions
#' @return an edited array of code lines
apply_indent <- function(lines, arr, spaces, trim) {
  J <- length(lines)
  indent <- 0
  for (j in seq_len(J)) {
    indent <- indent + arr[j] * spaces
    indent <- max(indent, 0)
    lines[j] <- justify_line(lines[j], indent, trim)
  }
  return(lines)
}

#' Param justify a line left
#'
#' @inheritParams format_code
#' @param line code line as a string
#' @param trim if this is true, left trimming is done
#' @family code formatting helper functions
#' @return edited code line
justify_line <- function(line, indent, trim) {
  spaces <- paste(rep(" ", indent), collapse = "")
  if (trim) {
    line <- trimws(line, "left", whitespace = "[ \t\r]")
  }
  line <- paste0(spaces, line)
  return(line)
}
