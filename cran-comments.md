Dear CRAN-Team, 

This fixes the error message for the CRAN Solaris check ressource flagged
thanks to Prof Ripley. 

Thank you for your support and on behalf of the RLumCarlo package developer team, 
best wishes, 

Sebastian Kreutzer

## R CMD check --as-cran results

0 errors | 0 warnings | 1 note

> Found the following (possibly) invalid DOIs:
>  DOI: 10.1088/0953-8984/24/38/385402
>      From: DESCRIPTION
>      Status: Bad Request
>      Message: 400

This URL works.

## Other notes or warnings

### *winbuilder* 

* R-devel complains about misspelt words in the DESCRIPTION: There is,
not to our knowledge wrong with the spelling in the DESCRIPTION.

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
