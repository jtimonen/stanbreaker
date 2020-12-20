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
  code <- format_part(code, spaces, 0)
  code <- gsub(x = code, pattern = "\n\n", "\n", fixed = TRUE)
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
#' @family code formatting functions
#' @return updated code as a string
#' @name format_code_helpers

#' @rdname format_code_helpers
format_part <- function(code, spaces, indent) {
  if (is.na(code)) code <- ""
  L <- nchar(code)
  if (L == 0) {
    return(code)
  }

  # The part before the first opening bracket
  idx <- stringr::str_locate(code, "[{]")[1]
  if (is.na(idx)) {
    code <- justify_left(code, indent)
    return(code)
  }
  before <- substr(code, 1, idx)
  before <- justify_left(before, indent)

  # The part between the {brackets}
  tmp <- substr(code, idx + 1, L)
  L <- nchar(tmp)
  idx <- locate_closing_bracket(tmp)
  middle <- substr(tmp, 1, idx - 1)
  middle <- format_part(middle, spaces, indent + spaces)

  # The part after the closing bracket corresponding to first opening bracket
  after <- substr(tmp, idx + 1, L)
  after <- format_part(after, spaces, indent)

  # Return
  closing <- justify_line("}", indent)
  paste0(before, "\n", middle, "\n", closing, "\n", after)
}

#' @rdname format_code_helpers
justify_left <- function(code, indent) {
  if (is.na(code)) {
    code <- ""
  } else {
    lines <- strsplit(code, split = "\n")[[1]]
    lines <- lapply(lines, justify_line, indent = indent)
    code <- paste(lines, collapse = "\n")
  }
  return(code)
}

#' @rdname format_code_helpers
justify_line <- function(line, indent) {
  spaces <- paste(rep(" ", indent), collapse = "")
  line <- gsub("^\\s+|\\s+$", "", line) # remove leading + trailing whitespace
  line <- paste0(spaces, line)
  return(line)
}

#' @rdname format_code_helpers
locate_closing_bracket <- function(code) {
  op <- stringr::str_locate_all(code, "[{]")[[1]][, 1]
  cl <- stringr::str_locate_all(code, "[}]")[[1]][, 1]
  N <- length(cl)
  for (i in 1:N) {
    idx <- cl[i]
    if (sum(op < idx) < i) {
      return(idx)
    }
  }
  stop("matching closing bracket not found")
}

#' Split code at given location or first occurrence of a character
#'
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
  pattern <- paste0("[", char, "]")
  if (is.null(idx)) {
    idx <- stringr::str_locate(code, pattern)[[1]][1]
  }
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
    msg <- "split_code didn't work correctly, please report a bug!\n"
    msg <- paste0(msg, "original = {", code, "}\n")
    msg <- paste0(msg, "reconstruction = {", reconst, "}\n")
    stop(msg)
  }
  return(out)
}
