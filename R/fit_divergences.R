#' Analyze a cmdstanr fit to figure out where the divergences are coming from
#'
#' @export
#' @param fit A stan fit to analyze
#'
#' @return Another cmdstanr fit informing you about your divergences
fit_divergences <- function(fit) {
  if(!setequal(class(fit), c("CmdStanMCMC", "CmdStanFit", "R6"))) {
    stop("Argument to fitfit should be an MCMC fit object from cmdstanr")
  }

  divs = fit$sampler_diagnostics() %>%
    as_draws_df %>%
    pull(divergent__)

  draws = fit$draws() %>%
    as_draws_matrix %>%
    subset_draws(fit$metadata()$stan_variables)

  draws = draws[, 2:ncol(draws)]

  means = apply(draws, 2, mean)
  sds = apply(draws, 2, sd)

  X = lapply(1:nrow(draws), function(i) {
    (draws[i,] - means) / sds
  }) %>% do.call(rbind, .)

  y = divs

  mod_lr = cmdstan_model(system.file("extdata", "lr.stan", package = "metastan"))

  fit_lr = mod_lr$sample(data = list(N = nrow(X), M = ncol(X),
                                     X = X, y = y,
                                     prior = 0.05),
                         parallel_chains = 4)

  return(fit_lr)
}
