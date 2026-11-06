## Test environments
* Local macOS (aarch64), R 4.5.1
* Windows (R-hub), R-devel r90840

## R CMD check results
Windows R-devel: 0 ERRORs | 0 WARNINGs | 0 NOTEs.

Local checks had no errors or warnings and two NOTEs: HTML Tidy was too old
to validate the HTML manual, and the Stack Overflow link in `svycor` returned
HTTP 403. The Stack Exchange API confirms the cited answer (41031088).

## Reverse dependencies
No new errors in the packages checked. With both versions of `jtools`,
`autostats` had the same `xgboost` errors and `panelr` 1.0.1 failed with
`lme4` 2.1-0. The `panelr` 1.1.0 candidate passes.
