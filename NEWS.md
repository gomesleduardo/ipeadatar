# ipeadatar 0.2.0

## Breaking changes

- Package now requires R >= 4.1.0.
- Updated internal implementation to use the native R pipe (`|>`).

## New features

- Added `label` argument to main functions.
- Added support for optional variable labels using `sjlabelled`.
- Added improved argument validation with `rlang::abort()`.

## Improvements

- Modernized package infrastructure and documentation.
- Replaced legacy messaging with `cli`.
- Improved API error handling.
- Updated vignette and examples.
- Added unit tests using `testthat`.

## Internal changes

- Updated roxygen2 documentation style.
- Improved compatibility with `R CMD check`.
- Replaced deprecated tidyverse patterns.
