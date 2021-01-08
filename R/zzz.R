get_version <- function() {
  ver <- "."
  tryCatch(
    {
      lib <- dirname(system.file(package = "stanbreaker"))
      desc <- utils::packageDescription("stanbreaker", lib.loc = lib)
      ver <- paste0(" (version ", desc$Version, ").")
    },
    warning = function(w) {
    }
  )
  return(ver)
}

get_dev_version <- function() {
  " (DEVELOPMENT VERSION)."
}

.onAttach <- function(...) {
  ver <- get_dev_version()
  info1 <- paste0("This is stanbreaker", ver)
  info2 <- " - Much of the functionality requires CmdStan to be installed."
  info3 <- paste0(
    " - See ?cmdstanr::install_cmdstan and ",
    "cmdstanr::check_cmdstan_toolchain()"
  )
  info <- paste(info1, info2, info3, sep = " \n")
  packageStartupMessage(info)
}
