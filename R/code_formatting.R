#' Format Stan code
#'
#' @export
#' @param code Stan code to be formatted, or name of a file where the
#' code is.
#' @param use_stanc Should the code be formatted using \code{stanc3} with the
#' \code{--auto-format} option? If this is
#' \itemize{
#'   \item TRUE - \code{stanc} must be installed. *Currently, this option
#'   discards all comments from the code*.
#'   \item FALSE - code is formatted by handling strings in R. This option will
#' not discard comments but is otherwise less sophisticated (currently only
#' trims lines and indents based on parentheses and curly brackets).
#' }
#' @param overwrite_file Should the file that was given as input be overwritten
#' by the formatted code?
#' @param place_includes Should \code{#include} statements be replaced by their
#' respective content?
#' @param include_dir Parent directory for possible \code{#include}d files.
#' @param spaces Number of spaces to use for indenting (has no effect if
#' \code{use_stanc} is TRUE).
#' @param verbose Should some informational messages be printed?
#' @inheritParams stanc3
format_code <- function(code = "",
                        use_stanc = TRUE,
                        overwrite_file = FALSE,
                        place_includes = FALSE,
                        include_dir = getwd(),
                        stanc_path = NULL,
                        spaces = 2,
                        verbose = FALSE) {

  # Read code
  if (file.exists(code)) {
    msg("interpreting code as a filename", verbose)
    code <- read_file(file = code)
  } else {
    msg("interpreting code as a string, not filename", verbose)
  }

  # Place possible includes
  if (place_includes) {
    msg("looking for #include statements", verbose)
    code <- place_includes(code, spaces = spaces, verbose = verbose)
  }

  # Format using stanc or R string handling
  if (use_stanc) {
    msg("formatting using stanc3", verbose)
    code <- format_code_stanc(code)
  } else {
    msg("formatting without stanc3", verbose)
    code <- format_code_r(code, spaces)
  }

  # Overwrite old file or return formatted code as string
  msg("formatting complete!", verbose)
  if (overwrite_file) {
    msg("writing to (TODO)", verbose)
    code <- invisible(code)
  }
  return(code)
}

format_code_stanc <- function(code, stanc_path = NULL) {
  args <- c("--auto-format")
  out <- stanc3(
    code = code,
    stanc_path = stanc_path,
    args = args,
    print_stdout = FALSE
  )
  return(out$stdout)
}

format_code_r <- function(code, spaces) {
  code <- trim_code(code)
  code <- indent_code(code, spaces = spaces, curly = TRUE)
  code <- indent_code(code, spaces = spaces, curly = FALSE)
  code <- trim_code(code)
  code <- ensure_trailing_linebreak(code)
  return(code)
}

#' Remove trailing whitespace from each code line
#'
#' @inheritParams format_code
#' @family code formatting functions
#' @return trimmed code
trim_code <- function(code) {
  trimmer <- function(line) {
    trimws(line, "right", whitespace = "[ \t\r]")
  }
  code <- apply_lines(code, trimmer)
  return(code)
}

#' Indent code based on (curly) brackets
#'
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

#' Place possible #includes into Stan code
#'
#' @inheritParams format_code
#' @return the full program code as a string, not formatted or indented
#' @family code formatting functions
place_includes <- function(code, include_dir = getwd(),
                           spaces = 2, verbose = FALSE) {
  lines <- strsplit(code, "\n")[[1]]
  code <- place_includes_lines(lines, include_dir, verbose)
  return(code)
}

#' A helper function called recursively by place_includes
#'
#' @param lines an array of strings, each representing one additional code line
#' @param parent_dir parent directory for the Stan files
#' @inheritParams format_code
#' @return full code as a string, without \code{#include} statements
place_includes_lines <- function(lines, parent_dir, verbose) {

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

      # Get the path to includes
      to_add <- read_file(fn, verbose)
      to_add <- place_includes_lines(to_add, parent_dir, verbose)
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
#' @inheritParams indent_code
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

#' Justify a line left
#'
#' @inheritParams format_code
#' @param line code line as a string
#' @param trim if this is true, left trimming is done
#' @family code formatting helper functions
#' @return edited code line
justify_line <- function(line, spaces, trim) {
  ws <- paste(rep(" ", spaces), collapse = "")
  if (trim) {
    line <- trimws(line, "left", whitespace = "[ \t\r]")
  }
  line <- paste0(ws, line)
  return(line)
}

#' Common string utilities
#'
#' @param txt a character vector
#' @name string_utils
#' @return updated character vector
NULL

#' @describeIn string_utils Apply a function to each line of text
#'
#' @param fun a function that takes one code line (without line break)
#' as its only argument
apply_lines <- function(txt, fun) {
  lines <- strsplit(txt, "\n")[[1]]
  L <- length(lines)
  if (L == 0) {
    return(fun(txt))
  }
  for (j in seq_len(L)) {
    lines[j] <- fun(lines[j])
  }
  code <- paste(lines, collapse = "\n")
  code <- ensure_trailing_linebreak(code)
  return(code)
}

#' @describeIn string_utils Ensure that a character vector ends in a line break
ensure_trailing_linebreak <- function(txt) {
  L <- nchar(txt)
  if (substr(txt, L, L) != "\n") {
    txt <- paste0(txt, "\n")
  }
  return(txt)
}
