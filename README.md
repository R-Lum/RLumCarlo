




<!-- README.md was auto-generated by README.Rmd. Please DO NOT edit by hand!-->

# RLumCarlo <img width=120px src="man/figures/Logo_RLumCarlo.png" align="right" />

The **R** package ‘RLumCarlo’ provides a collection of R functions
modelling luminescence production in isolators and semiconductors for
various stimulation modes using Monte Carlo methods.

[![Project Status:
Concept](http://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
[![CRAN](http://www.r-pkg.org/badges/version/RLumCarlo)](https://cran.r-project.org/package=RLumCarlo)
[![Downloads](http://cranlogs.r-pkg.org/badges/grand-total/RLumCarlo)](http://www.r-pkg.org/pkg/RLumCarlo)
[![Build
Status](https://travis-ci.com/R-Lum/RLumCarlo.svg?token=ymRxA7NeJn6EE7PixZr8&branch=master)](https://travis-ci.com/R-Lum/RLumCarlo)
[![codecov](https://codecov.io/gh/R-Lum/RLumCarlo/branch/master/graph/badge.svg?token=W0q5JUInf7)](https://codecov.io/gh/R-Lum/RLumCarlo)

## Installation

#### i. Requirements

Depending on your OS please download and install one of the following:

**Windows (32/64bit)** - ‘Rtools’ (provided by CRAN)

<https://cran.r-project.org/bin/windows/Rtools/>

**Mac OS X** - ‘Xcode’ (provided by Apple)

<https://developer.apple.com/xcode/downloads/>

For **Linux** users *gcc* often comes pre-installed in most
distributions. Should *gcc* be not available, however, we kindly refer
to the exhaustive collection of installation guides depending on the
linux distribution.

#### ii. Install the package from GitHub

``` r
if(!require("devtools"))
  install.packages("devtools")
devtools::install_github("R-Lum/RLumCarlo@master")
```

## Examples

``` r
run_MC_CW_IRSL_TUN(A = 0.12, rho = 0.003, times = 0:1000) %>%
     plot_RLumCarlo(norm = TRUE, legend = TRUE)
```

<img src="man/figures/README-unnamed-chunk-2-1.png" style="display: block; margin: auto;" />

## License

This program is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or any later
version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the [GNU
General Public
License](https://github.com/R-Lum/RLumCarlo/blob/master/LICENSE) for
more details.

## <span class="glyphicon glyphicon-euro"></span> Funding

The development of RLumCarlo benefitted from the support by various
funding bodies:

  - The initial work by Johannes Friedrich, Sebastian Kreutzer and
    Christoph Schmidt was supported by the [DFG (2015–2018,
    SCHM 3051/4-1, “Modelling quartz luminescence signal dynamics
    relevant for dating and dosimetry”, SCHM
    3051/4-1)](https://gepris.dfg.de/gepris/projekt/279969851).

  - Later financial support was secured through the project *ULTIMO:
    Unifying Luminescence Models of quartz and feldspar (German Academic
    Exchange Service) DAAD: Deutscher Akademischer Austauschdienst DAAD
    PPP USA 2018, ID: 57387041)*.

  - The work of Sebastian Kreutzer as maintainer of the package was
    supported by [LabEx LaScArBx (ANR -
    n. ANR-10-LABX-52)](https://lascarbx.labex.u-bordeaux.fr) between
    2017 and 2019.

## Related projects

  - [Luminescence](https://github.com/R-Lum/Luminescence)
  - [RLumModel](https://github.com/R-Lum/RLumModel)
