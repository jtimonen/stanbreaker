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
