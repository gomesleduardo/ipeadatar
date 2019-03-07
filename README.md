# R package for Ipeadata API database 

[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/ipeadatar)](https://CRAN.R-project.org/package=ipeadatar) [![CRAC\_Downloads](https://cranlogs.r-pkg.org/badges/grand-total/ipeadatar)](https://CRAN.R-project.org/package=ipeadatar)

## Introduction

This R package is an easier alternative to use the dataset of Brazilian Institute for Applied Economic Research (Ipea). It allows directly access to the macroeconomic, financial and regional databases. This package helps you to find the series you're looking for without the need to go on the [Ipeadata website](http://ipeadata.gov.br/) to find it and downloading it. This is a pratical way to use the Ipea databases on R.

## Instalation

### Via CRAN
```{r, eval = FALSE}
install.packages("ipeadatar")
```

### Via GitHub
```{r, eval = FALSE}
library(devtools)
install_github("gomesleduardo/ipeadatar")
```

## Functions list

  |       Functions       |                     Outputs                    |
  |-----------------------|------------------------------------------------|
  | available_countries   |  List with available countries                 |
  | available_series      |  List with available series                    |
  | available_subjects    |  List with available subjects                  |
  | available_territories |  List with available territorial divisions     |
  | ipeadata              |  Returns a database about the requested series |
  | metadata              |  Returns a metadata about the requested series |
  | search_series         |  List with searched series                     |

## Help

For more details and examples, see [R Documentation](https://cran.r-project.org/web/packages/ipeadatar/ipeadatar.pdf).

For a brief tutorial, check this [page](https://gomesleduardo.000webhostapp.com/ipeadatar_tutorial.html).
