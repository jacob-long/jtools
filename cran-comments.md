## Test environments
* Local Windows 10 install, R 3.5.2/R 3.6.0
* Ubuntu (Trusty) (on Travis-CI), R-release, R-devel
* Windows 2012 Server (on Appveyor) R-release, R-devel

## R CMD check results
There were two NOTEs. One is a notice that the package "interactions" 
listed in Enhances is not available in a mainstream repository. "interactions"
will be submitted to CRAN as soon as this version of this package is 
accepted by CRAN, as it is needed for "interactions" to function. 

The other note is what appears to be a spurious 403 for the following URL:
https://doi.org/10.2307/2533558 which works on my machine.

