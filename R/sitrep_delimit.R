#' @title Delimit in space and time
#'
#' @description Create
#'
#' @describeIn sitrep_delimit_metadata
#'
#' @param data_raw_name_string filename as string "data-raw/ficha_GB_26Feb20_09_52.xls"
#' @param week_ini week ini
#' @param week_fin week fin
#' @param space_var define the administrative unit variable
#' @param space_num number of administrative units
#'
#' @import dplyr
#' @import aweek
#' @import lubridate
#' @import magrittr
#' @import rlang
#'
#' @return retorno
#'
#' @export sitrep_delimit_metadata
#' @export sitrep_delimit_spacetime_eval
#' @export sitrep_delimit_spacetime_make
#'
#' @examples
#'
#'

sitrep_delimit_metadata <- function(data_raw_name_string,
                                    week_ini="2020-W01",week_fin="today",space_num=4) {

  nombre <- data_raw_name_string

  if (week_fin!="today") {
    week_fin <- week_fin
  } else{
    week_fin <- today() %>% date2week(floor_day = TRUE)
  }

  nombre_cierre <- str_replace(nombre,"(.+)\\/ficha_GB_(.+)_(..)_(..)\\.xls","\\2(\\3\\4h)")
  # nombre departamento de residencia
  # {{space_var}} o dpto_res

  title_week_range <- paste0("de ",week_ini," a ",week_fin)
  subtitle_cierre <- str_c("cierre ",nombre_cierre)

  export_name <- str_c("sgbdata_raw_cleaned_timespace-",
                       str_replace_all(title_week_range," ","_") %>% str_replace_all(.,"-",""),
                       "-",
                       str_replace_all(subtitle_cierre," ","_"),
                       "_spacenum_",space_num) %>%
    str_to_lower()

  #export_name

  list(title_week_range=title_week_range,subtitle_cierre=subtitle_cierre,export_name=export_name)

}

#' @describeIn sitrep_delimit_metadata create 2
#' @inheritParams sitrep_delimit_metadata
#' @param data outcome

sitrep_delimit_spacetime_eval <- function(data,week_ini="2020-W01",week_fin="today",space_var,space_num=4) {

  sgbdata_raw_cleaned <- data

  if (week_fin!="today") {
    week_fin <- week_fin
  } else{
    week_fin <- today() %>% date2week(floor_day = TRUE)
  }

  #tiempo: desde una determinada semana a la actualidad
  #week_ini <- "2019-W41"
  #week_ini <- "2020-W01"
  #week_ini <- "2019-W01"
  #week_ini <- "2019-W21"
  # week_fin <- if_else(condition = week_fin=="today",
  #                     true = (today() %>% date2week(floor_day = TRUE)),
  #                     false = week_fin)
  #week_fin <- today() %>% date2week(floor_day = TRUE)
  #week_fin <- "2019-W52"
  #week_fin <- "2019-W30"
  #time_range <- 41:52 #eliminar

  #espacio
  #space_num <- 4 #how many departments would you like to see?

  #generar rango de semanas
  week_range <- tibble(date_range=seq(from = week2date(week_ini),
                                      to = week2date(week_fin),
                                      by = '1 day')) %>%
    mutate(epiweek_w=date2week(date_range,
                               week_start = "Sunday",
                               floor_day = TRUE)) %>%
    count(epiweek_w) %>%
    pull(epiweek_w)

  #espacio
  #lima,cajamarca,lambayeque,lalibertad,piura,junin+amazonas (sin huancavelica)
  dptos <- sgbdata_raw_cleaned %>%
    #tiempo para priorizar espacio
    filter(is_in(epiweek_w,week_range)) %>%
    #filter((is_in(epiweek,time_range) & ano_adm_hos == 2019) | ano_adm_hos == 2020) %>%
    count({{space_var}},sort = T) %>%
    top_n(space_num,n)
  dptos

  #completo
  # sgbdata_raw_cleaned_timespace <- sgbdata_raw_cleaned %>%
  #   #tiempo para priorizar espacio
  #   filter(is_in(epiweek_w,week_range)) %>%
  #   #filter((is_in(epiweek,time_range) & ano_adm_hos == 2019) | ano_adm_hos == 2020) %>%
  #   #espacio
  #   filter(is_in({{space_var}},c(dptos %>% pull({{space_var}}))))
  #
  # sgbdata_raw_cleaned_timespace
}

#' @describeIn sitrep_delimit_metadata create 3
#' @inheritParams sitrep_delimit_metadata
#' @param data outcome

sitrep_delimit_spacetime_make <- function(data,week_ini="2020-W01",week_fin="today",space_var,space_num=4) {

  sgbdata_raw_cleaned <- data

  if (week_fin!="today") {
    week_fin <- week_fin
  } else{
    week_fin <- today() %>% date2week(floor_day = TRUE)
  }

  #tiempo: desde una determinada semana a la actualidad
  #week_ini <- "2019-W41"
  #week_ini <- "2020-W01"
  #week_ini <- "2019-W01"
  #week_ini <- "2019-W21"
  # week_fin <- if_else(condition = week_fin=="today",
  #                     true = (today() %>% date2week(floor_day = TRUE)),
  #                     false = week_fin)
  #week_fin <- today() %>% date2week(floor_day = TRUE)
  #week_fin <- "2019-W52"
  #week_fin <- "2019-W30"
  #time_range <- 41:52 #eliminar

  #espacio
  #space_num <- 4 #how many departments would you like to see?

  #generar rango de semanas
  week_range <- tibble(date_range=seq(from = week2date(week_ini),
                                      to = week2date(week_fin),
                                      by = '1 day')) %>%
    mutate(epiweek_w=date2week(date_range,
                               week_start = "Sunday",
                               floor_day = TRUE)) %>%
    count(epiweek_w) %>%
    pull(epiweek_w)

  #espacio
  #lima,cajamarca,lambayeque,lalibertad,piura,junin+amazonas (sin huancavelica)
  dptos <- sgbdata_raw_cleaned %>%
    #tiempo para priorizar espacio
    filter(is_in(epiweek_w,week_range)) %>%
    #filter((is_in(epiweek,time_range) & ano_adm_hos == 2019) | ano_adm_hos == 2020) %>%
    count({{space_var}},sort = T) %>%
    top_n(space_num,n)
  #dptos

  #completo
  sgbdata_raw_cleaned_timespace <- sgbdata_raw_cleaned %>%
    #tiempo para priorizar espacio
    filter(is_in(epiweek_w,week_range)) %>%
    #filter((is_in(epiweek,time_range) & ano_adm_hos == 2019) | ano_adm_hos == 2020) %>%
    #espacio
    filter(is_in({{space_var}},c(dptos %>% pull({{space_var}}))))

  sgbdata_raw_cleaned_timespace
}
