#' Write stan model code to file
#'
#' @export
#' @param stan_code stan code as a string
#' @param file path to file
#'
#' @return nothing
write_model <- function(stan_code, file = NULL) {
  if (is.null(file)) {
    filename <- paste0(deparse(substitute(stan_code)), ".stan")
    cat("writing to", filename, "...")
  }
  writeLines(stan_code, filename)
}

#' Create a model
#'
#' @export
#' @param stan_file path to a \code{.stan} file that describes the model
#' @param ... additional arguments to \code{$compile} in \code{cmdstanr}
#'
#' @return a CmdStanModel object
create_model <- function(stan_file, ...) {
  m <- cmdstanr::cmdstan_model(stan_file, compile = TRUE, ...)
  return(m)
}
