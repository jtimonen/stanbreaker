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

#' Check if OS is Windows
#'
#' @return a Boolean value
using_windows <- function() {
  isTRUE(.Platform$OS.type == "windows")
}

#' Check if argument is one of the allowed options
#'
#' @param arg the argument to check
#' @param allowed allowed options
#' @return TRUE if check succeeds, throw error otherwise
assert_one_of <- function(arg, allowed) {
  if (!(arg %in% allowed)) {
    arg_name <- deparse(substitute(arg))
    str <- paste(allowed, collapse = ", ")
    msg <- paste0(arg_name, " was '", arg, "', but must be one of: {")
    msg <- paste0(msg, str, "}\n")
    stop(msg)
  }
  TRUE
}
