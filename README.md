
# stanbreaker

<!-- badges: start -->
<!-- badges: end -->

The goal of stanbreaker is to be a tool that can help in developing stan and
trying out different types of adaptation.

**NOTE:** This is work in progress! Interface not stable or anything.

## Installation

``` r
devtools::install_github("jtimonen/stanbreaker")
```

## Example

``` r
library(stanbreaker)
## basic example code
```


## Development

Before committing
* run `devtools::document()`
* run `source("dev/style.R")`, which automatically styles the code
* run `source("dev/lint.R")`, and manually correct problems shown by it
* run `devtools::check()` and correct possible problems
* run `devtools::test()` and see if tests pass
