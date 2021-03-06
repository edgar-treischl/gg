
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gg

<!-- badges: start -->
<!-- badges: end -->

The goal of gg is to …

## Installation

You can install the development version of gg from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("edgar-treischl/gg")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(gg)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
ggpie(palmerpenguins::penguins, slices = island)
```

<img src="man/figures/README-ggpie-1.png" width="100%" />

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/v1/examples>.

You can also embed plots, for example:

``` r
df <- tibble::tribble(
  ~times, ~country,  ~gdp, ~inc,
  "1990",   "A",  22.3, T, 
  "2000",   "A",  44.6, T, 
  "1990",   "B",  12.3, F, 
  "2000",   "B",  4.6, F
)

ggslope(df,
        times = times,
        outcome = gdp,
        group = country)
```

<img src="man/figures/README-ggslope-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
