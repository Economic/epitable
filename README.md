
<!-- README.md is generated from README.Rmd. Please edit that file -->
epitable
========

epitable is an R package that creates EPI-formatted HTML tables.

Installation
------------

``` r
#install the package
devtools::install_github("Economic/epitable")

## install package and vignettes
devtools::install_github("Economic/epitable", build_vignettes = TRUE)
browseVignettes("epitable")
```

Example
-------

Basic use:

``` r
library(epitable)
epitable(tradebalance, rownamesvar = industry)
```

See the epitable vignette for more examples:

``` r
browseVignettes("epitable")
```
