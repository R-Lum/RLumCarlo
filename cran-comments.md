Dear CRAN-Team, 

This contribution is a new R package submission. Our package deals with the simulation of 
luminescence production in isolators and semiconductors (e.g., natural minerals such as quartz 
or feldspar) using different stimulation modes. In contrast to other work, which uses coupled 
differential equations, the simulation bases on Monte-Carlo runs allowing us to estimate 
(for the first time) the stochastic uncertainties of luminescence production. 

Each function comes with a short example, and a vignette provides further elaborated examples. 

Our package has no reverse dependencies and was developed and carefully tested in compliance 
with the CRAN policy. We summarise further information on the test environment below. 
The package is licenced under GPL-3. 

Thank you for considering our package for publication on CRAN. 

On behalf of the package developer team, 

Sebastian Kreutzer

## R CMD check --as-cran results

0 errors | 0 warnings | 1 note

The check complains about two (possibly) invalid URLSs 
( Status: 403, Message: Forbidden)

https://doi.org/10.1088/0953-8984/18/4/020
https://doi.org/10.1088/0953-8984/24/38/385402

Both URLs work find, I cannot see the problem. 

## Other notes or warnings

### *winbuilder* 

* R-devel complains about mis-spelled words in the DESCRIPTION: There is nothing 
wrong with the spelling in the DESCRIPTION. It also claims that 
the url https://doi.org/10.1088/0953-8984/24/38/385402 is wrong. This url 
works fine. 

* R-3.6.3 shows the same notes and R-4.0.0 did not send a report. 

## Test environments
* local macOS Catalina 10.15.5, Xcode 11.5, R-devel
* on AppVeyor CI
    * i386-w64-mingw32/i386 (32-bit), R-devel
    * x86_64_w64-mingw32/64 (64-bit), R-devel
    * x86_64_w64-mingw32/64 (64-bit), R 4.0.0 (2020-04-24)
    * i386-w64-mingw32/i386 (32-bit), R 4.0.0 (2020-04-24)
* on Travis CI
    * Ubuntu 16.04.6 LTS, oldrel
    * Ubuntu 16.04.6 LTS, release
    * Ubuntu 16.04.6 LTS, devel
    * macOS Sierra 10.13.6, Xcode 9.4.1, release
