




<!-- README.md was auto-generated by README.Rmd. Please DO NOT edit by hand!-->

# RLumCarlo <img width=120px src="man/figures/Logo_RLumCarlo.png" align="right" />

The **R** package ‘RLumCarlo’ by Johannes Friedrich (University of
Bayreuth, Germany) & Sebastian Kreutzer (IRAMAT-CRP2A, UMR 5060, CNRS -
Université Bordeaux Montaigne, France) provides a collection of various
R functions modelling luminescence signals using Monte Carlo methods

[![Project Status:
Concept](http://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
[![CRAN](http://www.r-pkg.org/badges/version/RLumCarlo)](https://cran.r-project.org/package=RLumCarlo)
[![Downloads](http://cranlogs.r-pkg.org/badges/grand-total/RLumCarlo)](http://www.r-pkg.org/pkg/RLumCarlo)

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

#### ii. Install the package

To install the latest development builds directly from GitHub, run

``` r
if(!require("devtools"))
  install.packages("devtools")
devtools::install_github("R-Lum/RLumCarlo@master")
```

To install a developer build other than ‘master’, replace the term
‘master’ in the codeline by the name of the wanted developer build
(not available yet).

## Examples

### Simulating CW-IRSL measurements

``` r
run_MC_CW_IRSL(A = 0.12, rho = 0.003, times = 0:1000) %>%
     plot_RLumCarlo(norm = T, legend = T)
```

![](man/figures/README-unnamed-chunk-2-1.png)<!-- -->

## Note

**The package comes without any guarantee\!**

Please further note that this version is a development version and may
change day by day. For stable branches please visit the package on CRAN.

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

## Related projects

  - [Luminescence](https://github.com/R-Lum/Luminescence)
  - [RLumModel](https://github.com/R-Lum/RLumModel)
