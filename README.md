<!-- README.md is generated from README.Rmd. Please edit that file -->
AIG
===

AIG is a collection of Automatic Item Generators. Currently, only the linear syllogism and arithmetic generators are in the package. More is to follow soon!

-   AIG
    -   `lisy`
    -   `arith`

Installation
------------

`AIG` is in [CRAN](https://cran.r-project.org/), but you can use [devtools](https://cran.r-project.org/package=devtools) to install the latest and greatest version. To do so:

``` r
if(!require("devtools")) install.packages("devtools")
devtools::install_github("Aidenloe/AIG")
```

To do list
==========

Giving two answers rather than one. i.e. A is greater than B, and B is smaller than A. Currently only one answer is provided. Making a linear premise for up to 3 inferences.
