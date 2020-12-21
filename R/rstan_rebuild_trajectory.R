#' Recreate an HMC trajectory
#'
#' @description Recreate an HMC trajectory given a parameter and momentum draw,
#' a maximum number of leapfrog steps to take, a stepsize, and an inverse
#' metric.
#' @export
#' @param fit An rstan stanfit object
#' @param udraws A vector of unconstrained parameters to start the trajectory
#' from
#' @param mdraws A vector of momentums to start the trajectory from
#' @param leapfrogs Number of leapfrog steps to integrate in each direction
#' @param stepsize Stepsize for leapfrog integrator
#' @param inv_metric Positive definite inverse metric
#' @return A dataframe with the reconstructed trajectory
rstan_rebuild_trajectory <- function(fit, udraws, mdraws, leapfrogs, stepsize,
                                     inv_metric) {
  if (!setequal(class(fit), c("stanfit"))) {
    msg <- "fit should be a stanfit object (from rstan)"
    stop(msg)
  }

  as_vector <- function(name, x) {
    if (posterior::is_draws_array(x) ||
      posterior::is_draws_matrix(x) ||
      posterior::is_draws_list(x) ||
      posterior::is_draws_df(x)) {
      x <- posterior::as_draws_matrix(x)

      if (nrow(x) > 1) {
        msg <- paste(
          "if argument", name,
          "is a posterior type, it can only have draw",
          "(and 1 chain)"
        )
        stop(msg)
      } else {
        return(as.vector(x))
      }
    }

    if (is.list(x)) {
      msg <- paste("argment", name, "must be a vector not a list")
      stop(msg)
    }
    if (is.vector(x)) {
      return(x)
    }

    msg <- paste("argument", name, "must be a vector")
    stop(msg)
  }

  column_names <- colnames(udraws)

  udraws <- as_vector("udraws", udraws)
  mdraws <- as_vector("mdraws", mdraws)

  N <- length(udraws)

  if (is.null(column_names)) {
    column_names <- paste0("V", 1:N)
  } else {
    column_names <- column_names[1:N]
  }

  if (N != length(mdraws)) {
    msg <- "udraws and mdraws must be vectors with the same length"
    stop(msg)
  }

  if (any(leapfrogs != as.integer(leapfrogs))) {
    msg <- "leapfrogs must be an integer"
    stop(msg)
  }

  if (N != rstan::get_num_upars(fit)) {
    msg <- paste(
      "Supplied upars has", N,
      "parameters but model has", rstan::get_num_upars(fit),
      "parameters"
    )
    stop(msg)
  }

  if (stepsize <= 0.0) {
    msg <- "stepsize must be greater than 0"
    stop(msg)
  }

  upath <- matrix(0, nrow = 2 * leapfrogs + 1, ncol = N)
  colnames(upath) <- column_names
  hamiltonian <- rep(0, 2 * leapfrogs + 1)
  upath[leapfrogs + 1, ] <- udraws
  q <- udraws
  p <- -mdraws
  grad_lp <- rstan::grad_log_prob(fit, q)
  ham_value <- -attr(grad_lp, "log_prob") + p %*% inv_metric %*% p / 2.0
  hamiltonian[leapfrogs + 1] <- ham_value
  p <- p - stepsize * -grad_lp / 2
  give_up <- FALSE
  for (i in (leapfrogs + 2):nrow(upath)) {
    q <- q + stepsize * inv_metric %*% p
    if (any(is.nan(q))) {
      give_up <- TRUE
    }

    if (give_up) {
      upath[i, ] <- NaN
      hamiltonian[i] <- NaN
    } else {
      upath[i, ] <- q
      grad_lp <- rstan::grad_log_prob(fit, q)
      ph <- p - stepsize * -grad_lp / 2.0
      ham_value <- -attr(grad_lp, "log_prob") + ph %*% inv_metric %*% ph / 2.0
      hamiltonian[i] <- ham_value
      p <- p - stepsize * -grad_lp
    }
  }

  q <- udraws
  p <- -mdraws
  grad_lp <- rstan::grad_log_prob(fit, q)
  ham_value <- -attr(grad_lp, "log_prob") + p %*% inv_metric %*% p / 2.0
  hamiltonian[leapfrogs + 1] <- ham_value
  p <- p + stepsize * -grad_lp / 2
  give_up <- FALSE
  for (i in leapfrogs:1) {
    q <- q - stepsize * inv_metric %*% p
    if (any(is.nan(q))) {
      give_up <- TRUE
    }

    if (give_up) {
      upath[i, ] <- NaN
      hamiltonian[i] <- NaN
    } else {
      upath[i, ] <- q
      grad_lp <- rstan::grad_log_prob(fit, q)
      ph <- p + stepsize * -grad_lp / 2.0
      ham_value <- -attr(grad_lp, "log_prob") + ph %*% inv_metric %*% ph / 2.0
      hamiltonian[i] <- ham_value
      p <- p + stepsize * -grad_lp
    }
  }

  dplyr::as_tibble(upath) %>%
    dplyr::mutate(
      hamiltonian = hamiltonian,
      step = -leapfrogs:leapfrogs
    ) %>%
    dplyr::select(hamiltonian, step, dplyr::everything())
}
