.onAttach <- function(...) {
  ver <- "."
  tryCatch(
    {
      lib <- dirname(system.file(package = "stanbreaker"))
      desc <- packageDescription("stanbreaker", lib.loc = lib)
      ver <- paste0(" (version ", desc$Version, ").")
    },
    warning = function(w) {
    }
  )
  info1 <- paste0("This is stanbreaker", ver)
  info2 <- " - Much of the functionality requires CmdStan to be installed."
  info3 <- " - See for example ?cmdstanr::install_cmdstan."
  info <- paste(info1, info2, info3, sep = " \n")
  packageStartupMessage(info)
}
