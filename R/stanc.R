#' Call stanc3 using processx
#'
#' @export
#' @description Calls stanc3 using processx. Thanks to Rok Cesnovar.
#' @param code Stan code as a character vector
#' @param stanc_path Path to the parent directory of \code{stanc}.
#' If this is \code{NULL}, an attempt is made to find \code{stanc} from the
#' CmdStan path set by \code{cmdstanr}.
#' @param args Command line arguments to \code{stanc}. When
#' \code{args = c("--help")}, the help page with all possible options is
#' returned.
#' @param print_stdout Should the output of \code{stanc} be printed?
#' @inheritParams format_code
#' @return The output of \code{processx::run} for the \code{stanc} command.
#' The output value is returned invisibly.
stanc3 <- function(code = "", stanc_path = NULL,
                   args = c("--help"),
                   print_stdout = TRUE, verbose = FALSE) {

  # Create a temporary file
  stan_file <- cmdstanr::write_stan_file(code)

  # Get path to stanc
  if (is.null(stanc_path)) {
    stanc_path <- cmdstanr::cmdstan_path()
    stanc_path <- file.path(stanc_path, "bin")
  }
  cmd <- file.path(stanc_path, "stanc")
  if (using_windows()) {
    cmd <- paste0(cmd, ".exe")
  }
  msg(paste0("path to stanc binary: ", cmd), verbose)

  # Call stanc with given command line arguments
  args <- c(stan_file, args)
  out <- processx::run(cmd, args = args)

  # Print stdout
  if (print_stdout) {
    cat(out$stdout)
  }

  # Remove the temporary file
  if (file.exists(stan_file)) {
    file.remove(stan_file)
  }
  return(invisible(out))
}
