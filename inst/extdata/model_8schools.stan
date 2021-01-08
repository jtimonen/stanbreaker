data {
  int<lower=0> n; //number of schools
  real y[n]; // effect of coaching
  real<lower=0> sigma[n]; // standard errors of effects
}
parameters {
  real mu;  // the overall mean effect
  real<lower=0> tau; // the inverse variance of the effect
  vector[n] eta; // standardized school-level effects
}
transformed parameters {
  vector[n] theta;
  theta = mu + tau * eta;
}
model {
  target += normal_lpdf(eta | 0, 1);
  target += normal_lpdf(y | theta, sigma);
}
