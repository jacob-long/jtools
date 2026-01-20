# Contributing

Contributions to `jtools` whether in the form of bug fixes, issue
reports, new code or documentation improvement are welcome. Please use
the Github issue tracker if remotely possible. For any pull request
please link to or open a corresponding issue in the issue tracker.

## Tests

`jtools` uses `testthat` for testing. Please try to provide complete
test coverage for any submitted code and always check that existing
tests continue to pass. If you aren’t familiar with `testthat` or
writing tests in general, please indicate that in your pull request and
I will likely be able to add those tests.

## Code style

Generally speaking, the [tidyverse style
guide](http://style.tidyverse.org/) won’t steer you wrong. A few
specific things to note, however:

- `<-` must be used for assignment. I may reject pull requests using
  `=`.
- Avoid using periods for function names and separate words with `_` in
  function names.
- In function *arguments*, use periods to separate words (not `_`). This
  is a convention designed to be consistent with this package and to
  clearly differentiate function arguments from other objects.
- I prefer commented code, especially if there’s any ambiguity about
  what sections of code are doing on first read.

## Code of Conduct

When contributing to `jtools` you must follow the code of conduct
defined in [CONDUCT.md](CONDUCT.md)
