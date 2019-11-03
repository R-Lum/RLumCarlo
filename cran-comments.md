Dear CRAN-Team, 

This contribution is a new R package submission. Our package deals with the simulation of 
luminescence production in isolators and semiconductors (e.g., natural minerals such as quartz 
or feldspar) using different stimulation modes. In contrast to other work, which uses coupled 
differential equations, the simulation base on Monte Carlo runs allowing us to estimate 
(for the first time) the stochastic uncertainties of luminescence production. 

Each function comes with a minimal running example, and a vignette provides further elaborated examples. 

The package has no reverse dependencies and was developed and carefully tested in compliance 
with the CRAN policy. We summarise further information on the test environment below. 
The package is licenced under GPL-3. 

Thank you for considering our package for publication on CRAN. 

On behalf of the package developer team, 

Sebastian Kreutzer

## R CMD check --as-cran results

0 errors | 0 warnings | 0 note

## Other notes or warnings

* *winbuilder* 

## Test environments
* local macOS High Sierra 10.14.6, Xcode 10.3, R-devel
* on AppVeyor CI
    * i386-w64-mingw32/i386 (32-bit), R-devel
    * x86_64_w64-mingw32/64 (64-bit), R-devel
    * x86_64_w64-mingw32/64 (64-bit), R 3.6.1 (2019-07-20)
    * i386-w64-mingw32/i386 (32-bit), R 3.6.1 (2019-07-20)
* on Travis CI
    * Ubuntu 16.04.6 LTS, oldrel
    * Ubuntu 16.04.6 LTS, release
    * Ubuntu 16.04.6 LTS, devel
    * macOS Sierra 10.13.3, Xcode 9.4.1, release
