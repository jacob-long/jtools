## Test environments
* Local Windows 10 install, R 3.5.2/R 3.6.0
* Ubuntu (Trusty) (on Travis-CI), R-release, R-devel
* Windows 2012 Server (on Appveyor) R-release, R-devel

## R CMD check results
There is one NOTE. It is what appears to be a spurious 403 for the following 
URL: https://doi.org/10.2307/2533558 which works on my machine. On one other
instance WinBuilder gave a similar NOTE regarding another doi.org URL which also 
does not replicate on my machine.

