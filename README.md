
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

Then you can call `stanbreaker::format_code(file = "model.stan", place_includes = TRUE)` which returns

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

If you set `place_includes = FALSE`, the `#include` directive wont be replaced with the content of `loop.stan`. You can control the amount of indenting by setting for example `spaces = 4`. By default, code is formatted by handling strings in R. More sophisticated formatting backend (`stanc3` with the `--auto-format` option) is called if you set `use_stanc = TRUE`, but this currently removes all comments from the code. See `?stanbreaker::format_code` for all options.

## Code analysis

You can list the output variables of your Stan model using the functions `parameters()`, `transformed_parameters()` and `generated_quantities()`. For example, for the
above `model.stan` we can call `stanbreaker::parameters(file = "model.stan")`, which returns the data frame

``` stan
   Name      Type
1    mu      real
2   tau      real
3   eta vector[n]
4 theta vector[n]
```
