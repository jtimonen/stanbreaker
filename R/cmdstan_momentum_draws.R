#' Get momentum draws from a cmdstanr fit
#' object run with save_latent_dynamics = TRUE
#'
#' @export
#' @param fit A cmdstan fit object run with \code{save_latent_dynamics = TRUE}
#' @return A posterior matrix of momentum draws
cmdstan_momentum_draws <- function(fit) {
  if (!setequal(class(fit), c("CmdStanMCMC", "CmdStanFit", "R6"))) {
    msg <- "fit argument should be an MCMC fit object from cmdstanr"
    stop(msg)
  }

  latent_dynamics_files <- fit$latent_dynamics_files()
  latent_dynamics <- cmdstanr::read_cmdstan_csv(latent_dynamics_files)
  draws_df <- latent_dynamics$post_warmup_draws %>%
    posterior::as_draws_df()

  # Dimension of unconstrained space
  N <- (ncol(draws_df) - 4) / 3
  inds <- (N + 2):(2 * N + 1)
  out <- draws_df %>% select(inds, `.chain`, `.iteration`, `.draw`)
  return(out)
}
