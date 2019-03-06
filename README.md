
<!-- README.md is generated from README.Rmd. Please edit that file -->
epitable
========

epitable is an R package that creates HTML tables formatted for EPI's website.

Documentation
-------------

<https://economic.github.io/epitable/>

Installation
------------

``` r
## install package and vignettes
devtools::install_github("Economic/epitable", build_opts = c("--no-resave-data", "--no-manual"))
browseVignettes("epitable")
```

Basic use
---------

``` r
library(epitable)
epitable(tradebalance, rownamesvar = industry)
```
