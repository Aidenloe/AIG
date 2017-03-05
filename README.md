<!-- README.md is generated from README.Rmd. Please edit that file -->
AIG
===

AIG is a collection of Automatic Item Generators. Currently, the generator can create linear syllogistic, arithmetic and 2D spatial reasoning items. More is to follow soon!

-   AIG
    -   `lisy`
    -   `arith`
    -   `spatial2d`
    -   `spatial3d`
    -   `spatial3d_mirror`

Installation
------------

`AIG` is in [CRAN](https://cran.r-project.org/), but you can use [devtools](https://cran.r-project.org/package=devtools) to install the latest and greatest version. To do so:

``` r
if(!require("devtools")) install.packages("devtools")
devtools::install_github("Aidenloe/AIG")
```

To do list
==========

-   Making a loop so that the function can generate more than 1 item at a time.
