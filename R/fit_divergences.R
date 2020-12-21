#' Analyze a cmdstanr fit to figure out where the divergences are coming from
#'
#' @export
#' @param fit A cmdstanr fit to analyze
#' @return Another \code{cmdstanr} fit informing you about your divergences
fit_divergences <- function(fit) {
  if (!setequal(class(fit), c("CmdStanMCMC", "CmdStanFit", "R6"))) {
    msg <- "Argument to fit_divergences should be an MCMC fit object
    from cmdstanr"
    stop(msg)
  }

  # Get divergent transitions as vector of 0s and 1s
  divs <- fit$sampler_diagnostics() %>%
    posterior::as_draws_df() %>%
    dplyr::pull("divergent__")

  # Get matrix of parameters
  draws <- fit$draws() %>%
    posterior::as_draws_matrix() %>%
    posterior::subset_draws(fit$metadata()$stan_variables)

  # Ignore lp__ (which will be the first column)
  draws <- draws[, 2:ncol(draws)]

  # Whiten inputs
  means <- apply(draws, 2, mean)
  sds <- apply(draws, 2, sd)

  X <- lapply(seq_len(nrow(draws)), function(i) {
    (draws[i, ] - means) / sds
  }) %>% do.call(rbind, .)

  # Create model
  filename <- system.file("extdata", "lr.stan", package = "stanbreaker")
  mod_lr <- cmdstanr::cmdstan_model(filename)

  # Fit model
  fit_lr <- mod_lr$sample(
    data = list(
      N = nrow(X), M = ncol(X),
      X = X, y = divs,
      prior = 0.05
    ),
    # TODO do the parallel mc cores thing here (system defaults)
    parallel_chains = 4
  )

  return(fit_lr)
}
