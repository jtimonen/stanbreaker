#' Read a text file and return its contents as a string
#'
#' @param file file name
#' @param ... additional arguments to \code{readLines}
#' @return file contents as a string
#' @family IO functions
read_file <- function(file, ...) {
  checkmate::assert_string(file)
  lines <- readLines(con = file, ...)
  txt <- paste(lines, collapse = "\n")
  txt <- paste0(txt, "\n")
  return(txt)
}

#' Write text to a file, overwriting old content
#'
#' @param code code as a string
#' @inheritParams read_file
#' @return nothing
#' @family IO functions
write_file <- function(file, code) {
  checkmate::assert_string(code)
  checkmate::assert_string(file)
  cat(code, file = file)
}
