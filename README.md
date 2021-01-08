
# stanbreaker

<!-- badges: start -->
<!-- badges: end -->

**NOTE:** This is work in progress! Interface not stable or anything.

## Installation

``` r
devtools::install_github("jtimonen/stanbreaker", ref = "main")
```

## Code formatting

Assume you have `model.stan` with the content
``` stan
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
#include loop.stan
  target += normal_lpdf(y | theta, sigma);
}


```

where `loop.stan` contains
``` stan
  // Loop over each eta
for(j in 1:n) {
target += normal_lpdf(eta[j] | 0, 1);
  }

```

Then you can call `stanbreaker::format_code(file = "model.stan", use_stanc = FALSE, place_includes = TRUE)` which returns

``` stan
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
  // Loop over each eta
  for(j in 1:n) {
    target += normal_lpdf(eta[j] | 0, 1);
  }

  target += normal_lpdf(y | theta, sigma);
}
```

See `?stanbreaker::format_code` for all options.

