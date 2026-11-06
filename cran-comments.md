## Release scope

Version 2.3.2 is a narrow maintenance update for model refitting with
weights, offsets, and subsets, quasi-family likelihood summaries, and
compatibility with recent lme4/reformulas. It also removes a stray debugging
flag that made formula helpers fail. It excludes unrelated glmmTMB
development work from the working repository.

## Test environments

* Local macOS (aarch64), R 4.5.1, ggplot2 4.0.3, lme4 2.1-0.
* Targeted tests also pass with ggplot2 3.4.4, reformulas 0.4.2,
  and lme4 1.1-38 (80 assertions, no warnings or skips).
* R-hub Linux R-release (R 4.6.2), Linux clang R-devel, and Windows R-devel
  (r90836) all pass.

## R CMD check results

Final local `--as-cran`, including incoming checks, vignettes and PDF manual:

0 ERRORs | 0 WARNINGs | 2 NOTEs

HTML Tidy is too old to validate the HTML manual. The second NOTE is an HTTP
403 response from a Stack Overflow reference. The official Stack Exchange API
confirms that answer 41031088 still belongs to question 34418822, so the
reference is retained. This is a draft release report, not a submission.
The packaged suite passes 329 assertions; its existing ggplot2 `fatten`
deprecation warning remains. The earlier full visual suite passes 402
assertions. All 74 legacy SVGs are unchanged in a paired baseline/candidate
comparison using identical dependencies and seeds.

## Downstream checks

Baseline/candidate comparisons found no new failures among autostats,
bruceR, dpm, webSDM, released panelr, and released interactions. bruceR, dpm,
and webSDM pass without errors, warnings, or notes. autostats has the same
xgboost-related failures with both dependency sets. Released panelr 1.0.1
fails with lme4 2.1-0 in both sets; the coordinated panelr 1.1.0 candidate
fixes that incompatibility. Released interactions 1.2.0 passes with only an
existing unused-import NOTE.

These local reverse checks run examples, tests, and vignette R code, but
skip PDF manuals and vignette rebuilding.

## Hosted checks

All three R-hub platform checks finish with Status: OK (no errors, warnings,
or notes). The suites pass 311 assertions, with the existing ggplot2 `fatten`
test warning and 34 visual/optional-package skips. R-devel also reports an
informational missing usage entry for the legacy scale_lm alias; that usage
entry has now been added without changing that function's implementation.
The formula-helper fix, its tests, and the usage documentation postdate the
hosted run. They pass the final local check and older-dependency tests; a fresh
hosted check of the final archive remains necessary before submission.
Windows Builder R-release/R-devel uploads are accepted; reports are pending.
