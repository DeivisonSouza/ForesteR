
<!-- README.md is generated from README.Rmd. Please edit that file -->

# forester

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
SRS(x = pinus$Volume, A = 400000, a = 600, DT=F)
#> New names:
#> * `` -> ...1
#> # A tibble: 16 x 2
#>    Parameters                             Estimates     
#>    <chr>                                  <chr>         
#>  1 Sum                                    6,353.67      
#>  2 Sample mean                            397.10        
#>  3 Number of possible samples             667.00        
#>  4 Sampling fraction                      0.02          
#>  5 Erro maximo admissivel                 39.71         
#>  6 t-student                              2.13          
#>  7 Sample intensity                       14.00         
#>  8 Variancia da media                     301.98        
#>  9 Mean standard error                    17.38         
#> 10 Absolute sampling error                37.04         
#> 11 Relative sampling error                9.33          
#> 12 Lower confidence interval (Mean)       360.06        
#> 13 Upper confidence interval (Mean)       434.14        
#> 14 Total population                       264,868.48    
#> 15 Lower confidence interval (Population) 144,025,919.02
#> 16 Upper confidence interval (Population) 173,657,414.31
```
