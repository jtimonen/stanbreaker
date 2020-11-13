#' Build an rstan fit object from a modelfile and a data list
#'
#' @export
#' @param modelfile A stan model file
#' @param data Data for stan model
#' @return An rstan fit object
rstan_make_fit <- function(modelfile, data) {
  rstan::stan(modelfile, data = data, cores = 1, iter = 1)
}
