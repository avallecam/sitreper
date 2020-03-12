#' @title who sitrep data management
#'
#' @description clean and plot who sitrep
#'
#' @describeIn who_sitrep_import import data from github
#'
#' @param data_input string of github repo
#'
#' @import readr
#' @import dplyr
#' @import tidyr
#' @import forcats
#' @import ggplot2
#' @import patchwork
#'
#' @return report per country
#'
#' @export who_sitrep_import
#' @export who_sitrep_cleandb
#' @export who_sitrep_ggline
#' @export who_sitrep_ggbar
#' @export who_sitrep_country_report
#'
#' @examples
#'
#' who_sitrep_country_report(data_input = "https://raw.github.com/fkrauer/COVID-19/master/data/WHO_COVID19_ALL_ADM0_2020-03-10.csv",
#'                           country = "Brazil")
#'

who_sitrep_import <- function(data_input) {
  path_file <- {{data_input}}
  who_sitrep <- read_csv(file = path_file)
}

#' @describeIn who_sitrep_import clean dataset
#' @inheritParams who_sitrep_import
#' @param data input of raw dataset

who_sitrep_cleandb <- function(data) {
  data %>%
    mutate(class=fct_explicit_na(class)) %>%
    mutate_at(.vars = vars(n_cum_deaths,n_inc_conf,n_inc_deaths),.funs = ~replace_na(.x,0))
}

#' @describeIn who_sitrep_import plot a ggplot geom_line
#' @inheritParams who_sitrep_import
#' @param country string of country name
#' @param y_cum_value cumulative variable
#' @param color colo of attribute
#' @param n_breaks y axis breaks

who_sitrep_ggline <- function(data,country="Peru",y_cum_value,color,n_breaks=5) {
  data %>%
    # mutate(class=fct_explicit_na(class)) %>%
    # mutate_at(.vars = vars(n_cum_deaths,n_inc_conf,n_inc_deaths),.funs = ~replace_na(.x,0)) %>%
    filter(country == {{country}}) %>%
    ggplot(aes(x = date,y = {{y_cum_value}},color={{color}})) +
    geom_line() +
    geom_point() +
    scale_y_continuous(breaks= scales::pretty_breaks(n = {{n_breaks}})) +
    scale_color_viridis_d() +
    labs(title = {{country}},caption = "Data: WHO situation report") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}

#' @describeIn who_sitrep_import plot a ggplot geom_col
#' @inheritParams who_sitrep_import
#' @param y_inc_value incidence variable
#' @param fill fill of attribute

who_sitrep_ggbar <- function(data,country="Peru",y_inc_value,fill,n_breaks=5) {
  data %>%
    # mutate(class=fct_explicit_na(class)) %>%
    # mutate_at(.vars = vars(n_cum_deaths,n_inc_conf,n_inc_deaths),.funs = ~replace_na(.x,0)) %>%
    filter(country == {{country}}) %>%
    ggplot(aes(x = date,y = {{y_inc_value}},fill={{fill}})) +
    geom_col() +
    scale_y_continuous(breaks= scales::pretty_breaks(n = {{n_breaks}})) +
    scale_fill_viridis_d() +
    labs(title = {{country}},caption = "Data: WHO situation report") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),)
}

#' @describeIn who_sitrep_import remove replicates
#' @inheritParams who_sitrep_import

who_sitrep_country_report <- function(data_input,country) {

  who_sitrep <- who_sitrep_import(data_input = {{data_input}})

  country_name <- {{country}}

  f0 <- who_sitrep %>%
    who_sitrep_cleandb() %>%
    filter(country==country_name) %>%
    tail(4) %>%
    gridExtra::tableGrob()

  f1 <- who_sitrep %>%
    who_sitrep_cleandb() %>%
    who_sitrep_ggline(country = country_name,
                      y_cum_value = n_cum_conf,
                      color = class, n_breaks = 10)
  f3 <- who_sitrep %>%
    who_sitrep_cleandb() %>%
    who_sitrep_ggline(country = country_name,
                      y_cum_value = n_cum_deaths,
                      color = class, n_breaks = 10)
  f2 <- who_sitrep %>%
    who_sitrep_cleandb() %>%
    who_sitrep_ggbar(country = country_name,
                     y_inc_value = n_inc_conf,
                     fill = class)
  f4 <- who_sitrep %>%
    who_sitrep_cleandb() %>%
    who_sitrep_ggbar(country = country_name,
                     y_inc_value = n_inc_deaths,
                     fill = class)

  #library(patchwork)
  # design <- c(
  #   area(1,2),
  #   area(3,4),
  #   area(5,5),
  #   area(5,5),
  # )
  (f1 | f2) / (f3 | f4) / (f0) #+
  #plot_layout(design = design)

}
