#' Get matrix of unconstrained draws from a cmdstanr fit
#' object run with save_latent_dynamics = TRUE
#'
#' @export
#' @param fit A cmdstan fit object run with save_latent_dynamics = TRUE
#' @return A posterior matrix of unconstrained draws
cmdstan_unconstrained_draws <- function(fit) {
  if (!setequal(class(fit), c("CmdStanMCMC", "CmdStanFit", "R6"))) {
    msg <- "fit should be an MCMC fit object from cmdstanr"
    stop(msg)
  }

  latent_dynamics_files <- fit$latent_dynamics_files()
  latent_dynamics <- cmdstanr::read_cmdstan_csv(latent_dynamics_files)
  draws_df <- latent_dynamics$post_warmup_draws %>%
    posterior::as_draws_df()

  # Dimension of unconstrained space
  N <- (ncol(draws_df) - 4) / 3

  # TODO: use quoted string literals
  out <- draws_df %>% dplyr::select(2:(N + 1), `.chain`, `.iteration`, `.draw`)
  return(out)
}
