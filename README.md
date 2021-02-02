
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

## Simple Random Sampling

This is a basic example which shows you how to solve a common problem
(Sanquetta et al., 2014, page: 110-113):

``` r
library(forester)
data("pinus")
RS(x = pinus$Volume, A = 40, a = 0.06, FP = F, DT = F)
#> $Descriptive
#> # A tibble: 13 x 2
#>    Parameters Value
#>    <chr>      <dbl>
#>  1 count      16   
#>  2 mean       23.8 
#>  3 sd          4.22
#>  4 var        17.8 
#>  5 cv         17.7 
#>  6 min        17.8 
#>  7 Q1         22.0 
#>  8 median     23.7 
#>  9 Q3         24.8 
#> 10 IQR         2.86
#> 11 max        36.2 
#> 12 kurt        5.77
#> 13 skew        1.47
#> 
#> $Estimated
#> # A tibble: 13 x 2
#>    Parameters                                Value
#>    <chr>                                     <dbl>
#>  1 Number of potential sample units         667.  
#>  2 Count                                     16   
#>  3 Sample mean                               23.8 
#>  4 Sample sufficiency                        14   
#>  5 Variance of mean                           1.09
#>  6 Standard error of mean                     1.04
#>  7 Absolute sampling error                    2.22
#>  8 Relative sampling error                    9.33
#>  9 Lower confidence interval the mean        21.6 
#> 10 Upper confidence interval the mean        26.0 
#> 11 Total population                       15884.  
#> 12 Lower confidence interval (Population) 14403.  
#> 13 Upper confidence interval (Population) 17366.  
#> 
#> $`Base information`
#> # A tibble: 1 x 7
#>       E     t     A     a     N      f    fc
#>   <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl> <dbl>
#> 1  2.38  2.13    40  0.06  667. 0.0240 0.976
#> 
#> attr(,"class")
#> [1] "forester" "RS"
```

An example using factor:

``` r
data("species2")
RS(x = species2$Volume, by = species2$Specie, A = c(40, 50, 60, 70), a = c(0.06, 0.07, 0.05, 0.08), FP = F, DT = F)
#> $Descriptive
#> # A tibble: 13 x 5
#>    Parameters Pinus Eucaliptus  Teca Acacia
#>    <chr>      <dbl>      <dbl> <dbl>  <dbl>
#>  1 count      16         14    11     13   
#>  2 mean       23.8       23.8  24.2   24.1 
#>  3 sd          4.22       4.53  4.92   4.53
#>  4 var        17.8       20.5  24.2   20.5 
#>  5 cv         17.7       19.1  20.3   18.8 
#>  6 min        17.8       17.8  17.8   17.8 
#>  7 Q1         22.0       21.2  21.7   22.4 
#>  8 median     23.7       23.1  23.7   23.7 
#>  9 Q3         24.8       25.0  24.6   24.8 
#> 10 IQR         2.86       3.75  2.85   2.41
#> 11 max        36.2       36.2  36.2   36.2 
#> 12 kurt        5.77       5.14  4.33   5.04
#> 13 skew        1.47       1.42  1.23   1.34
#> 
#> $Estimated
#> # A tibble: 13 x 5
#>    Parameters                                Pinus Eucaliptus     Teca   Acacia
#>    <chr>                                     <dbl>      <dbl>    <dbl>    <dbl>
#>  1 Number of potential sample units         667.       714.    1200      875   
#>  2 Count                                     16         14       11       13   
#>  3 Sample mean                               23.8       23.8     24.2     24.1 
#>  4 Sample sufficiency                        14         17       21       17   
#>  5 Variance of mean                           1.09       1.47     2.20     1.58
#>  6 Standard error of mean                     1.04       1.21     1.48     1.26
#>  7 Absolute sampling error                    2.22       2.62     3.31     2.74
#>  8 Relative sampling error                    9.33      11.0     13.7     11.3 
#>  9 Lower confidence interval the mean        21.6       21.2     20.9     21.4 
#> 10 Upper confidence interval the mean        26.0       26.4     27.5     26.9 
#> 11 Total population                       15884.     16977.   29060.   21127.  
#> 12 Lower confidence interval (Population) 14403.     15108.   25092.   18731.  
#> 13 Upper confidence interval (Population) 17366.     18846.   33027.   23523.  
#> 
#> $`Base information`
#> # A tibble: 4 x 8
#>   by             E     t     A     a     N       f    fc
#>   <fct>      <dbl> <dbl> <dbl> <dbl> <dbl>   <dbl> <dbl>
#> 1 Pinus       2.38  2.13    40  0.06  667. 0.0240  0.976
#> 2 Eucaliptus  2.38  2.16    50  0.07  714. 0.0196  0.980
#> 3 Teca        2.42  2.23    60  0.05 1200  0.00917 0.991
#> 4 Acacia      2.41  2.18    70  0.08  875  0.0149  0.985
#> 
#> attr(,"class")
#> [1] "forester" "RS"
```

## Stratified Random Sampling

This is a basic example which shows you how to solve a common problem
(Sanquetta et al., 2014, page: 122-129):

``` r
data("native")
SRS(x=native$Volume1, strata=native$Strata, A = c(650, 350), a = 1, LE = 0.1, SA = "PA", FP = FALSE, DT = FALSE, digits = 3)
#> $Descriptive
#> # A tibble: 13 x 3
#>    Parameters      S1      S2
#>    <chr>        <dbl>   <dbl>
#>  1 count       12      12    
#>  2 Mean        89.1   125.   
#>  3 sd           8.46   16.2  
#>  4 var         71.5   261.   
#>  5 cv           9.49   12.9  
#>  6 min         74      99    
#>  7 Q1          82.8   116.   
#>  8 median      90.5   124.   
#>  9 Q3          95.2   133.   
#> 10 IQR         12.5    16.8  
#> 11 max        101     153    
#> 12 kurt         2.02    2.22 
#> 13 skew        -0.233   0.201
#> 
#> $Anova
#>             Df Sum Sq Mean Sq F value   Pr(>F)    
#> strata       1   7921    7921   47.61 6.27e-07 ***
#> Residuals   22   3660     166                     
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> $Estimated
#> $Estimated$parameters
#> # A tibble: 15 x 2
#>    Parameters                                                             Value
#>    <chr>                                                                  <dbl>
#>  1 Number of potential sample units                                     1000   
#>  2 Count                                                                  24   
#>  3 Sample sufficiency (df =23) (Proportional allocation)                   5.66
#>  4 Sample sufficiency recalculation (df =5) (Proportional allocation)      8.72
#>  5 Stratified sample mean                                                102.  
#>  6 Stratified sample variance                                            138.  
#>  7 Variance of the mean stratified                                         5.05
#>  8 Standard error of the mean stratified                                   2.25
#>  9 Absolute sampling error                                                 4.66
#> 10 Relative sampling error                                                 4.58
#> 11 Lower confidence interval for the mean                                 97.1 
#> 12 Upper confidence interval for the mean                                106.  
#> 13 Total population                                                   101800   
#> 14 Lower confidence interval for the population                        97141.  
#> 15 Upper confidence interval for the population                       106459.  
#> 
#> $Estimated$nst
#>  strata nh (Proportional allocation)
#>      S1                            6
#>      S2                            3
#>   Total                            9
#> 
#> 
#> $BaseInfo
#> $BaseInfo$`1`
#>  strata   Ah   Nh   Wh
#>      S1  650  650 0.65
#>      S2  350  350 0.35
#>   Total 1000 1000 1.00
#> 
#> $BaseInfo$`2`
#> # A tibble: 5 x 2
#>   Parameters  Value
#>   <chr>       <dbl>
#> 1 E          10.2  
#> 2 t           2.07 
#> 3 f           0.024
#> 4 fc          0.976
#> 5 ne         22.0  
#> 
#> 
#> attr(,"class")
#> [1] "forester" "SRS"
```

## Reference

SANQUETTA, C. R.; CORTE, A. P. D.; RODRIGUES, A. L.; WATZLAWICK, L. F.
Inventário Florestal: Planejamento e execução. 3. ed. Curitiba:
Multi-Graphic Gráfica e Editora., 2014. 406p.
