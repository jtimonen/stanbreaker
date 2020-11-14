#' Compute hessian in unconstrained space
#'
#' If udraws is a vector, the output is the Hessian computed
#' at the point in parameter space corresponding to udraws
#'
#' If udraws is a matrix, each row is interpreted as a place
#' to compute a Hessian and the output is a list of Hessians
#'
#' @export
#' @param fit An rstan stanfit
#' @param udraws Place(s) to compute Hessian(s) at
#' @return Hessian(s)
rstan_hessian <- function(fit, udraws) {
  if (!setequal(class(fit), c("stanfit"))) {
    msg <- "rfit argument should be a stanfit object (from rstan)"
    stop(msg)
  }

  if (is.null(nrow(udraws))) {
    udraws <- matrix(udraws, nrow = 1)
  } else {
    udraws <- matrix(udraws, nrow = nrow(udraws))
  }

  cbrt_epsilon <- .Machine$double.eps^(1 / 3)
  M <- nrow(udraws)
  N <- ncol(udraws)

  if (N != rstan::get_num_upars(fit)) {
    msg <- paste(
      "Supplied upars has", N,
      "parameters but model has", rstan::get_num_upars(fit),
      "parameters"
    )
    stop(msg)
  }

  out <- list()
  for (m in 1:M) {
    H <- matrix(0, nrow = N, ncol = N)
    for (n in 1:N) {
      dx <- cbrt_epsilon * max(1, abs(udraws[m, n]))
      xp <- udraws[m, ]
      xp[n] <- xp[n] + dx
      xm <- udraws[m, ]
      xm[n] <- xm[n] - dx

      H[, n] <- (rstan::grad_log_prob(fit, as.vector(xp)) -
        rstan::grad_log_prob(fit, as.vector(xm))) / (2.0 * dx)
    }

    out[[m]] <- 0.5 * (H + t(H))
  }

  if (M == 1) {
    return(out[[1]])
  } else {
    return(out)
  }
}
