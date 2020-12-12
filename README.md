
<!-- README.md is generated from README.Rmd. Please edit that file -->

# forester package

<!-- badges: start -->

<!-- badges: end -->

The `forester` package constitutes a set of functions with mathematical
and statistical methods traditionally used by forestry engineers to
analyze forest inventory data.

## Installation

You can install the released version of `forester` from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("forester")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("DeivisonSouza/forester")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(forester)
data("pinus")
SRS(x = pinus$Volume, A = 400000, a = 600, FP = F, DT = F)
#> New names:
#> * `` -> ...1
#> # A tibble: 16 x 2
#>    Parameters                             Estimates
#>    <chr>                                  <chr>    
#>  1 Sum                                    381.22   
#>  2 Sample mean                            23.83    
#>  3 Number of potential sample units       667.00   
#>  4 Sample fraction                        0.02     
#>  5 Maximum acceptable error               2.38     
#>  6 t-student value                        2.13     
#>  7 Sample sufficiency                     14.00    
#>  8 Variance of mean                       1.09     
#>  9 Standard error of mean                 1.04     
#> 10 Absolute sampling error                2.22     
#> 11 Relative sampling error                9.33     
#> 12 Lower confidence interval the mean     21.60    
#> 13 Upper confidence interval the mean     26.05    
#> 14 Total population                       15,892.11
#> 15 Lower confidence interval (Population) 14,409.79
#> 16 Upper confidence interval (Population) 17,374.42
```
