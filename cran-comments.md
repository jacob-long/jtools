## Test environments
* Local macOS Sierra install, R 3.4.3
* Local Kubuntu (17.10) install, R 3.4.3
* Ubuntu (Trusty) (on Travis-CI), R-release, R-devel
* Windows 2012 Server (on Appveyor) R-release, R-devel

## R CMD check results
There is a NOTE about the suggested package `huxtable` not being in a
"mainstream" repository. That package was recently pulled from CRAN for failing
its checks. Its developer is actively working to resubmit to CRAN, but in
the meanwhile this NOTE is generated. The only changes in this release of this
package are avoiding test failures that require `huxtable`. If it turns out
that `huxtable` does not return to CRAN soon, I will remove the dependency
entirely; but for now I prefer to not disable the function that relies on 
`huxtable` when it is likely it will return in the next CRAN release of this
package.

