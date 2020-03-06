#' @title apply a tidy geocoding with fixed parameters
#'
#' @description geocode a vector inside a tibble using opencage
#'
#' @describeIn sitrep_opencage_tidy tidy opencage output for only one string
#'
#' @param place string with place name
#' @param opencagekey opencage key
#'
#' @import dplyr
#' @import opencage
#'
#' @return columns with geolocation coordinates
#'
#' @export sitrep_opencage_tidy
#' @export sitrep_opencage_tidy_map
#'
#' @examples
#'
#' #
#' #library(opencage)
#' #library(tidyverse)
#' #library(sitreper)
#' #
#' #opencagekey <- "paste_free_geocode_key_here"
#' #
#' ## examples ----------------------------------------------------------------
#' #
#' #place_01 <- "Calle Daniel Olaechea 199, LIMA PERU"
#' #place_02 <- "Ministerio de Salud Lima Peru"
#' #place_03 <- "Av. Salaverry 801, Jesus Maria 15072"
#' #place_10 <- "Sarzeau"
#' #
#' ## -------------------------------------------------------------------------
#' #
#' #output_clean <- sitrep_opencage_tidy(place = place_02,opencagekey = opencagekey)
#' #
#' #output_clean %>% glimpse()
#' ##> Observations: 1
#' ##> Variables: 22
#' ##> $ `components.ISO_3166-1_alpha-2` <fct> PE
#' ##> $ `components.ISO_3166-1_alpha-3` <fct> PER
#' ##> $ components._category            <fct> building
#' ##> $ components._type                <fct> building
#' ##> $ components.city                 <fct> Jesús María
#' ##> $ components.continent            <fct> South America
#' ##> $ components.country              <fct> Perú
#' ##> $ components.country_code         <fct> pe
#' ##> $ components.house_number         <fct> 801
#' ##> $ components.postcode             <fct> LIMA 15046
#' ##> $ components.region               <fct> Lima
#' ##> $ components.road                 <fct> Avenida Salaverry
#' ##> $ components.state                <fct> Lima
#' ##> $ components.state_code           <fct> LIM
#' ##> $ components.suburb               <fct> Jesús María
#' ##> $ components.unknown              <fct> Ministerio de Salud
#' ##> $ geometry.lat                    <dbl> -12.07298
#' ##> $ geometry.lng                    <dbl> -77.04095
#' ##> $ confidence                      <fct> 10
#' ##> $ formatted                       <fct> "Ministerio de Salud, Avenida ...
#' ##> $ query                           <chr> "Ministerio de Salud Lima Peru"
#' ##> $ annotations.OSM.url             <fct> https://www.openstreetmap.org/...
#' #
#' ## -------------------------------------------------------------------------
#' #
#' #data_query <- tibble(place=c(place_01,place_02,place_03,place_10))
#' #
#' #data_query %>%
#' #  sitrep_opencage_tidy_map(place = place,opencagekey = opencagekey)
#' ##> # A tibble: 3 x 8
#' ##>   place geocode geometry.lat geometry.lng confidence components.post~
#' ##>   <chr> <list>         <dbl>        <dbl> <fct>      <fct>
#' ##> 1 Call~ <tibbl~        -12.1        -77.0 9          15072
#' ##> 2 Mini~ <tibbl~        -12.1        -77.0 10         LIMA 15046
#' ##> 3 Av. ~ <tibbl~        -12.1        -77.0 10         LIMA 15046
#' ##> # ... with 2 more variables: components._category <fct>,
#' ##> #   annotations.OSM.url <fct>
#'

sitrep_opencage_tidy <- function(place,opencagekey) {
  output <- opencage_forward(placename = place,
                             key = opencagekey,
                             countrycode = "PE",
                             language = "es",
                             min_confidence = 7,
                             limit = 1)
  if (is.null(output$results)) {
    output_clean <- NA
  } else {
    output_clean <- output$results %>%
      select(starts_with("components"),
             starts_with("geometry"),
             confidence,formatted,query,
             annotations.OSM.url)
  }
  output_clean
}

#' @describeIn sitrep_opencage_tidy tidy opencage output for only a tibble
#' @inheritParams sitrep_opencage_tidy
#' @param data tibble with column of places

sitrep_opencage_tidy_map <- function(data,place,opencagekey) {
  data %>%
    #filter out if tibble have no place
    filter(!is.na({{place}})) %>%
    mutate(geocode=map(.x = {{place}},
                       .f = sitrep_opencage_tidy,
                       opencagekey = opencagekey)) %>%
    #filter out if no result were optained
    filter(!is.na(geocode)) %>%
    #select general columns
    mutate(geocode_select=map(.x = geocode,
                              .f = ~select(.x,
                                           starts_with("geometry"),
                                           confidence,
                                           #components.postcode,
                                           #components.state_code,components.suburb,
                                           components._category,
                                           annotations.OSM.url))) %>%
    unnest(cols = c(geocode_select))
}
