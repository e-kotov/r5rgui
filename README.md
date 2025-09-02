

<!-- README.md is generated from README.Rmd. Please edit that file -->

# r5rgui

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/r5rgui)](https://CRAN.R-project.org/package=r5rgui)
[![R-CMD-check](https://github.com/e-kotov/r5rgui/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/e-kotov/r5rgui/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/e-kotov/r5rgui/graph/badge.svg)](https://app.codecov.io/gh/e-kotov/r5rgui)
<!-- badges: end -->

The goal of r5rgui is to allow the user to interactively explore routes
calcualted with `{r5r}` (https://github.com/ipeaGIT/r5r/) package in a
`Shiny` app, e.g. for troubleshooting routing problems.

## Installation

You can install the development version of r5rgui from
[GitHub](https://github.com/e-kotov/r5rgui) with:

``` r
# install.packages("pak")
pak::pak("e-kotov/r5rgui")


# setup java as you would for r5r package
# install.packages('rJavaEnv')

# check version of Java currently installed (if any) 
rJavaEnv::java_check_version_rjava()

# install Java 21
rJavaEnv::java_quick_install(version = 21)
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(r5rgui)
r5r_gui_demo()
```

![r5gui demo in
action](https://github.com/user-attachments/assets/58e8828e-e307-4cc7-9c93-73a4c89e1abf)

What the demo runs internally is this simple example code:

``` r
data_path <- system.file("extdata/poa", package = "r5r")
r5r_network <- build_network(data_path = data_path, verbose = FALSE)
r5r_gui(r5r_network, center = c(-51.22, -30.05), zoom = 11)
```

Therefore you can replace `data_path` with your own data path and
explore your own routing network.
