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
#' @family code formatting functions
format_code <- function(code, spaces = 2) {

  # Indent and add line breaks
  code <- format_part(code, spaces, 0)

  # Remove trailing whitespace from each line
  lines <- strsplit(code, split = "\n")[[1]]
  trimmer <- function(line) {
    trimws(line, "right", whitespace = "[ \t\r]")
  }
  lines <- lapply(lines, trimmer)
  code <- paste(lines, collapse = "\n")

  # Add empty last line
  code <- paste0(code, "\n")
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
  split <- split_code(code, char = "{")
  part1 <- split$before
  part1 <- justify_left(part1, indent)
  if (split$char == "") {
    return(part1)
  }

  # Middle and end parts
  idx <- locate_closing_bracket(split$after)
  split <- split_code(split$after, idx = idx)
  part2 <- format_part(split$before, spaces, indent + spaces)
  part3 <- format_part(split$after, spaces, indent)

  # Join the parts
  part2 <- ensure_leading_linebreak(part2)
  opening <- justify_line("{", indent)
  closing <- justify_line("} ", indent)
  out <- paste0(part1, opening, part2, "\n", closing, part3)
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

#' Split code at given location or first occurrence of a character
#'
#' @export
#' @param code code to split as a string
#' @param char character where to split (ignored if \code{idx} is not
#' \code{NULL})
#' @param idx index of the character where to split
#' @return A named list with elements
#' \itemize{
#'   \item \code{before} - the part before the character
#'   \item \code{after} - the part after the character
#'   \item \code{char} - the value of \code{char} given as input, or the
#'   character at location \code{idx}
#' }
#' If \code{char} is not found, \code{before} will be the original code
#' and \code{after} and \code{char} will be an empty strings. If \code{char}
#' is the first (last) character of \code{code}, then \code{before}
#' (\code{after}) will be an empty string. The motivation is that
#' \code{paste0(before, char, after)} will equal the original \code{code}.
#' @family code formatting functions
split_code <- function(code, char = "\n", idx = NULL) {
  char_in <- char
  idx_in <- idx
  pattern <- paste0("[", char, "]")
  if (is.null(idx)) idx <- stringr::str_locate(code, pattern)[[1]][1]
  L <- nchar(code)
  if (is.na(idx)) idx <- L + 1
  before <- substr(code, 1, idx - 1)
  char <- substr(code, idx, idx)
  after <- substr(code, idx + 1, L)

  # Create the list that will be returned
  out <- list(before = before, char = char, after = after)

  # Check correct behaviour
  reconst <- paste0(out$before, out$char, out$after)
  if (reconst != code) {
    msg <- "split_code didn't work correctly, please report a bug!\n\n"
    msg <- paste0(msg, "original = {", code, "}\n")
    msg <- paste0(msg, "reconstruction = {", reconst, "}\n\n")
    msg <- paste0(msg, "char_in = {", char_in, "}, idx_in = {", idx_in, "}\n")
    stop(msg)
  }
  return(out)
}

#' Add empty line to beginning of string if it doesn't exist already
#'
#' @param s a string
#' @return modified string
#' @family code formatting helper functions
ensure_leading_linebreak <- function(s) {
  s_trim <- trimws(s, whitespace = "[ \t\r]")
  s1 <- substr(s_trim, 1, 1)
  if (s1 != "\n") s <- paste0("\n", s)
  return(s)
}
