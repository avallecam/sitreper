
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sitreper

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/sitreper)](https://cran.r-project.org/package=sitreper)
<!-- badges: end -->

The goal of `sitreper` is to provide a multiple set of functions for the
generation of situational reports:

  - space-time tibble delimiters based on epidemiological weeks
  - geocode places within a tibble
  - surveillance specific cleaning tools

## Installation

<!--

You can install the released version of `sitreper` from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("sitreper")
```

-->

You can install the development version of `sitreper` using:

``` r
if(!require("devtools")) install.packages("devtools")
devtools::install_github("______/sitreper")
```

## Example

This is a basic example which shows you how to solve a common problem:

### geocode

``` r
library(opencage)
library(tidyverse)
library(sitreper)

opencagekey <- "paste_free_geocode_key_here"

# examples ----------------------------------------------------------------

place_01 <- "Calle Daniel Olaechea 199, LIMA PERU"
place_02 <- "Ministerio de Salud Lima Peru"
place_03 <- "Av. Salaverry 801, Jesus Maria 15072"
place_10 <- "Sarzeau"

data_query <- tibble(place=c(place_01,place_02,place_03,place_10))

data_query %>%
  sitrep_opencage_tidy_map(place = place,opencagekey = opencagekey)
#> # A tibble: 3 x 8
#>   place geocode geometry.lat geometry.lng confidence components.post~
#>   <chr> <list>         <dbl>        <dbl> <fct>      <fct>
#> 1 Call~ <tibbl~        -12.1        -77.0 9          15072
#> 2 Mini~ <tibbl~        -12.1        -77.0 10         LIMA 15046
#> 3 Av. ~ <tibbl~        -12.1        -77.0 10         LIMA 15046
#> # ... with 2 more variables: components._category <fct>,
#> #   annotations.OSM.url <fct>
```

### Surveillance data management (`cdcper_sitrep`)

  - `clean_eda_sp`: limpia base consolidada de EDA (vigilancia semanal)
  - `clean_ira_sp`: limpia base consolidada de IRA (vigilancia semanal)
  - `cdc_casos_tiempo`: resume casos por semana de EDA o IRA
  - `cdc_casos_nacional`: resume casos de EDA o IRA en el presente año

## To-Do

  - ( ) solve **“R/cdc\_eda\_ira.R”**
