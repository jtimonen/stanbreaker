#' Analyze a cmdstanr fit to figure out where the divergences are coming from
#'
#' @export
#' @param fit A stan fit to analyze
#' @return Another \code{cmdstanr} fit informing you about your divergences
fit_divergences <- function(fit) {
  if (!setequal(class(fit), c("CmdStanMCMC", "CmdStanFit", "R6"))) {
    msg <- "Argument to fit_divergences should be an MCMC fit object
    from cmdstanr"
    stop(msg)
  }

  # TODO: explain what is done here
  # TODO: where are pull and as_draws_df and etc. coming from?
  divs <- fit$sampler_diagnostics() %>%
    as_draws_df() %>%
    pull(divergent__)

  draws <- fit$draws() %>%
    as_draws_matrix() %>%
    subset_draws(fit$metadata()$stan_variables)

  draws <- draws[, 2:ncol(draws)]

  means <- apply(draws, 2, mean)
  sds <- apply(draws, 2, sd)

  X <- lapply(1:nrow(draws), function(i) {
    (draws[i, ] - means) / sds
  }) %>% do.call(rbind, .)

  y <- divs

  # Create model
  # TODO: get rid of metastan here
  filename <- system.file("extdata", "lr.stan", package = "metastan")
  mod_lr <- cmdstanr::cmdstan_model(filename)

  # Fit model
  fit_lr <- mod_lr$sample(
    data = list(
      N = nrow(X), M = ncol(X),
      X = X, y = y,
      prior = 0.05
    ),
    parallel_chains = 4
  )

  return(fit_lr)
}
