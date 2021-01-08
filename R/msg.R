
#' Print a message if verbose mode is on
#'
#' @param text The message to be printed. Will be appended with a line break.
#' @param verbose Should the message be printed?
#' @param ... additional arguments to \code{cat}
#' @return nothing
msg <- function(text, verbose, ...) {
  if (verbose) {
    text <- paste0(text, "\n")
    cat(text, ...)
  }
}
