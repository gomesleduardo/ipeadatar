# ipeadatar: An R Interface to the Ipeadata API

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/ipeadatar)](https://CRAN.R-project.org/package=ipeadatar)
[![CRAN_Downloads](https://cranlogs.r-pkg.org/badges/grand-total/ipeadatar)](https://CRAN.R-project.org/package=ipeadatar)

## Introduction

The **ipeadatar** package provides a convenient interface to the
Ipeadata API, maintained by the Brazilian Institute for Applied Economic
Research (Ipea). It allows direct access to macroeconomic, financial,
and regional time series without requiring manual downloads from the
[Ipeadata website](https://www.ipeadata.gov.br/).

The package is designed to simplify the discovery, retrieval, and use of
Ipeadata series within R, offering a practical and reproducible workflow
for applied research and data analysis.

## Installation

### From CRAN

```{r, eval = FALSE}
install.packages("ipeadatar")
```

### Development version from GitHub

```{r, eval = FALSE}
library(devtools)
install_github("gomesleduardo/ipeadatar")
```

## Available functions

| Function            | Description                           |
|---------------------|---------------------------------------|
| available_countries | List available countries              |
| available_series    | List available series                 |
| available_subjects  | List available subjects               |
| ipeadata            | Retrieve data for selected series     |
| metadata            | Retrieve metadata for selected series |
| search_series       | Search for series by keywords         |

## Documentation

For full details and examples, see the package reference manual: [R
Documentation](https://cran.r-project.org/web/packages/ipeadatar/ipeadatar.pdf).
