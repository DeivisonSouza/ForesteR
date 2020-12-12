
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Hello, welcome\! :smiley: :grin:

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
#>  1 Sum                                    381.220   
#>  2 Sample mean                            23.826    
#>  3 Number of potential sample units       667.000   
#>  4 Sample fraction                        0.024     
#>  5 Maximum acceptable error               2.383     
#>  6 t-student value                        2.131     
#>  7 Sample sufficiency                     14.000    
#>  8 Variance of mean                       1.087     
#>  9 Standard error of mean                 1.043     
#> 10 Absolute sampling error                2.222     
#> 11 Relative sampling error                9.327     
#> 12 Lower confidence interval the mean     21.604    
#> 13 Upper confidence interval the mean     26.049    
#> 14 Total population                       15,884.167
#> 15 Lower confidence interval (Population) 14,402.601
#> 16 Upper confidence interval (Population) 17,365.732
```
