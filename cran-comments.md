Dear CRAN-Team, 

This replaces RLumCarlo v0.1.3 currently under inspection by the CRAN Team. 

After my submission yesterday, I realised a couple of other problems with links and possibly misspelt words. This will likely lead to another rejection of my submission. 
The following measures are now implemented, to avoid wasting more time of the CRAN Team: 

* The package now uses a WORDLIST and unit test with the package 'spelling'
* All flagged misspelt words were corrected
* I removed all 'bad' DOIs from the manual, except for the DESCRIPTION file (where it was requested by the CRAN Team). 
* The corresponding references are still correctly mentioned in the manual, but not anymore linked through a DOI. However, I notified the publisher, and I will add these DOIs again as soon as the underlying problem got solved.

Thank you very much for your patience and support!

On behalf of the RLumCarlo package developer team, best wishes, 

Sebastian Kreutzer

## Responses to 3rd rejection

>  Possibly mis-spelled words in DESCRIPTION:
>  linerarly (32:5)

Done and I am very sorry for the extra round and the trouble.

## Responses to 2nd rejection

> Please always use the canonical link for R packages.
> Please fix and resubmit, and document what was changed in the submission comments.

The flagged link was: `https://www.r-pkg.org/pkg/RLumCarlo`. 

This is **not** a link, this is the URL to retrive the badge: 

`[![CRAN](https://www.r-pkg.org/badges/version/RLumCarlo)](https://cran.r-project.org/package=RLumCarlo)`

If you click on the badge the site link is: https://cran.r-project.org/package=RLumCarlo
To be sure, I changed it now to `https://CRAN.R-project.org/package=RLumCarlo`. 
Obviosuly it does not change the URL, but maybe this is what was meant. 


## Responses to 1st rejection

> If there are references describing the (theoretical background of) methods in your package, please add these in the Description field of your DESCRIPTION file in the form
> authors (year) <doi:...>
> authors (year) <arXiv:...>
> authors (year, ISBN:...)
> with no space after 'doi:', 'arXiv:' and angle brackets for auto-linking.

Done.

> Please replace \dontrun{} by \donttest{} or unwap the examples if they can be 
executed in less than 5 sec per Rd-file.

Nothing done. We have at least one running example per function (not wrapped
in `\dontrun{}`) If we wrapped something in `\dontrun{}` it was because
it exceeded this 5 s but we wanted to provide some longer examples 
(obviously it depends a little bit on the computer, 
but we did not want to push it too far and overcharge CRAN resources). 
More examples can be found in the vignette. 

>You are changing the user's par() settings in your functions. Please ensure with an immediate call of on.exit() that the settings are reset. E.g.
>   opar <- par(no.readonly =TRUE)       # code line i
>   on.exit(par(opar))                   # code line i+1

Added as requested in code-lines 120-121 in `plot_RLumCarlo()` (the only 
function that produces a plot output).

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
