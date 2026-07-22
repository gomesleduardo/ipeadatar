# ipeadatar 0.2.1

## Bug fixes

Fixed SSL-related connection issues in:

- `ipeadata()`
- `metadata()`
- `available_series()`
- `available_subjects()`
- `available_territories()`

Data retrieval now uses explicit HTTP requests via `curl::curl_fetch_memory()` before JSON parsing, improving compatibility on systems where direct URL access could fail with SSL errors.

## Improvements

- Added automatic fallback between HTTPS and HTTP API endpoints, increasing robustness when the primary endpoint is temporarily unavailable.
- Improved handling of API responses.
- Minor maintenance and code quality improvements.

## Documentation

- Updated package vignettes.
- Corrected vignette paths and examples.
- Improved reproducibility of vignette code.

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
