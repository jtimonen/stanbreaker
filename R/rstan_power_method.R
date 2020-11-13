#' Compute largest magnitude eigenvalue of Hessian and
#' corresponding eigenvector
#'
#' If udraws is a vector, the output is the eigenvalue/
#' eigenvector pair computed at the point in parameter
#' space corresponding to udraws
#'
#' If udraws is a matrix, each row is interpreted as a place
#' to compute an eigenvalue/eigenvector pair and the output
#' is a list of eigenvalue/eigenvector pairs
#'
#' L is an optional preconditioner matrix. If this is supplied
#' compute the eigenvalue/eigenvector pairs of L^T * H * L
#'
#' max_iterations is the maximum number of power method iterations
#' to use on any output
#'
#' tol is the relative tolerance check done on the eigenvalue
#'
#' @export
#' @param fit An rstan stanfit
#' @param udraws Place(s) to compute Hessian(s) at
#' @return eigenvalue/eigenvector pairs
rstan_power_method <- function(fit, udraws, L = NULL, max_iterations = 200, tol = 1e-5) {
  if (!setequal(class(fit), c("stanfit"))) {
    msg <- "rfit argument should be a stanfit object (from rstan)"
    stop(msg)
  }

  if(is.null(nrow(udraws))) {
    udraws = matrix(udraws, nrow = 1)
  } else {
    udraws = matrix(udraws, nrow = nrow(udraws))
  }

  cbrt_epsilon = .Machine$double.eps^(1/3)
  M = nrow(udraws)
  N = ncol(udraws)

  if(N != rstan::get_num_upars(fit)) {
    msg <- paste("Supplied upars has", N,
                 "parameters but model has", rstan::get_num_upars(fit),
                 "parameters")
    stop(msg)
  }

  if(is.null(L)) {
    L = diag(N)
  } else {
    if(N != nrow(L) || N != ncol(L)) {
      msg <- paste("L (if supplied) must be a square matrix of size", N, "by", N)
      stop(msg)
    }
  }

  if(max_iterations < 1) {
    msg <- "max_iterations must be greater than 0"
    stop(msg)
  }

  if(tol <= 0.0) {
    msg <- "tol must be greater than 0"
    stop(msg)
  }

  out = list()
  for(m in 1:M) {
    v = runif(N)
    v = v / sqrt(sum(v^2))
    eval = 0.0

    u = udraws[m, ]
    dx = cbrt_epsilon * pmax(1, max(abs(u)))

    f = function(v) {
      v = v / sqrt(sum(v^2))
      (grad_log_prob(fit, u + dx * L %*% v) -
        grad_log_prob(fit, u - dx * L %*% v)) / (2.0 * dx)
    }

    Av = f(v)

    for (i in 1:max_iterations) {
      v_norm = sqrt(sum(v^2))
      new_eval = v %*% Av / (v_norm * v_norm)
      if (i == max_iterations
          || abs(new_eval - eval) <= max(tol, tol * abs(eval))) {

        eval = new_eval
        break
      }

      eval = new_eval
      v = Av / sqrt(sum(Av^2))

      Av = f(v)
    }

    out[[m]] = list(e = eval, v = as.vector(v))
  }

  if(M == 1) {
    return(out[[1]])
  } else {
    return(out);
  }
}
