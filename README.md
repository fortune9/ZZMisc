
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ZZMisc

<!-- badges: start -->
<!-- badges: end -->

This package hosts any functions or utilities that have not found their
specific packages.

## Installation

You can install this package with the following command from github

``` r
library(devtools)
install_github("fortune9/ZZMisc")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(ZZMisc)
# load a package, and install it if not installed yet
load_package("ggplot2")
#> Loading required package: ggplot2
## basic example code
```

## Package website

The associated package website built with
[pkgdown](https://pkgdown.r-lib.org/) is available at
[here](https://fortune9.github.io/ZZMisc/)

<!-- comments

What is special about using `README.Rmd` instead of just `README.md`? You can include R chunks like so:


```r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You'll still need to render `README.Rmd` regularly, to keep `README.md` up-to-date. `devtools::build_readme()` is handy for this. You could also use GitHub Actions to re-render `README.Rmd` every time you push. An example workflow can be found here: <https://github.com/r-lib/actions/tree/master/examples>.

In that case, don't forget to commit and push the resulting figure files, so they display on GitHub and CRAN.
-->

## Contributors

Zhenguo Zhang *Author*
