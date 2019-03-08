
<!-- README.md is generated from README.Rmd. Please edit that file -->
epitable
========

epitable is an R package that prints HTML tables formatted for [EPI's website](https://www.epi.org/).

This is primarily of use for folks who work at EPI. If you are looking for a great all-purpose HTML table-making package, check out [htmlTable](https://cran.r-project.org/web/packages/htmlTable/). epitable is based on htmlTable but is adapted to create tables in line with EPI's website specifications.

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
