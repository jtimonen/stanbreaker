data {
  int N;
  int M;
  matrix[N, M] X;
  int y[N];
  real prior;
}

parameters {
  real beta0;
  vector[M] beta;
}

model {
  beta0 ~ normal(0, prior);
  beta ~ normal(0, prior);
  y ~ bernoulli_logit(beta0 + X * beta);
}
