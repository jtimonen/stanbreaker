#' Get matrix of unconstrained draws from a cmdstanr fit
#' object run with save_latent_dynamics = TRUE
#'
#' @export
#' @param fit A cmdstan fit object run with save_latent_dynamics = TRUE
#' @return A posterior matrix of unconstrained draws
cmdstan_unconstrained_draws <- function(fit) {
  if (!setequal(class(fit), c("CmdStanMCMC", "CmdStanFit", "R6"))) {
    msg <- "fit argument should be an MCMC fit object from cmdstanr"
    stop(msg)
  }

  latent_dynamics_files = fit$latent_dynamics_files()

  latent_dynamics = cmdstanr::read_cmdstan_csv(latent_dynamics_files)

  draws_matrix = latent_dynamics$post_warmup_draws %>%
    posterior::as_draws_matrix()

  return(draws_matrix[, 2:((ncol(draws_matrix) - 1) / 3 + 1)])
}
