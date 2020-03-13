#' Limpiar bases de EDA e IRA
#'
#' Funciones para limpiar bases de EDA e IRA.
#'
#' @describeIn basal_ira_sp limpiar base IRA
#'
#' @param rute_ira ruta a la carpeta con todas las bases de datos crudas de IRA
#' @param year_range rango de años a usar como basal
#' @param diagnosis elegir el diagnostico a evaluar. para IRA: ira, neu, ngr, hos, dih, deh, sob. para EDA: eda, col, dis, cop.
#'
#' @import dplyr
#' @import foreign
#' @import tidyr
#' @import janitor
#'
#' @return Bases limpiar de EDA e IRA.
#'
#' @export basal_ira_sp
#' @export basal_eda_sp
#'
#' @examples
#'
#' # to run this examples
#' # change rute_eda or rute_ira accordingly
#' #
#' # library(sitreper)
#' # library(tidyverse)
#' #
#' # #ira
#' # diagnosis <- "neu" #ira, neu, ngr, hos, dih, deh, sob
#' # ira_ira <- basal_ira_sp(rute_ira = "data_raw/ira",
#' #                         year_range = 2018:2019,
#' #                         diagnosis = diagnosis)
#' # ira_ira %>% count(ano)
#' # ira_ira %>% count(semana)
#' #
#' # #eda
#' # diagnosis <- "daa" #col, dis, cop
#' # eda_eda <- basal_eda_sp(rute_eda = "data_raw/eda",
#' #                         year_range = 2018:2019,
#' #                         diagnosis = diagnosis)
#' # eda_eda %>% count(type,ano)
#' # eda_eda %>% count(semana)
#'

basal_ira_sp <- function(rute_ira,year_range=2013:2019,diagnosis="ira") {
  #list files with dbf extension
  list.files(path = rute_ira,
             pattern = "(.DBF|.dbf)",
             full.names = T) %>%
    #make a tibble from it
    enframe(name = NULL) %>%
    #create year
    mutate(year=str_replace(value,"(.+)(sp|SP)(..)\\.(dbf|DBF)","20\\3")) %>%
    mutate(year=as.numeric(year)) %>%
    #define test year
    mutate(type=if_else(is.na(year),"test","reference")) %>%
    #keep basal and test
    filter(magrittr::is_in(year,year_range)|type=="test") %>%
    #read file names
    mutate(read_dbf=map(.x = value,
                        .f = ~read.dbf(file = .x) %>%
                          as_tibble() %>%
                          janitor::clean_names() %>%
                          select(ubigeo, ano, semana, starts_with(diagnosis)))) %>%
    unnest(cols = c(read_dbf)) %>%
    select(-value) %>%
    #sum all values per ubigeo
    group_by(type, year,ubigeo, ano, semana) %>%
    summarise_at(.vars = vars(starts_with(diagnosis)),.funs = sum) %>%
    ungroup() %>%
    select(-year) %>%
    #make horizontal rowwise sums
    mutate(row_sum=pmap_int(.l = select(.,starts_with(diagnosis)),.f = sum))
}

#' @describeIn basal_ira_sp remove replicates
#' @inheritParams basal_ira_sp
#' @param rute_eda ruta a la carpeta con todas las bases de datos crudas de EDA

basal_eda_sp <- function(rute_eda,year_range=2013:2019,diagnosis="eda") {
  list.files(path = rute_eda,
             pattern = "(.DBF|.dbf)",
             full.names = T) %>%
    enframe(name = NULL) %>%
    mutate(year=str_replace(value,"(.+)(sp|SP)(....)\\.(dbf|DBF)","\\3")) %>%
    mutate(year=as.numeric(year)) %>%
    mutate(type=if_else(is.na(year),"test","reference")) %>%
    filter(magrittr::is_in(year,year_range)|type=="test") %>%
    mutate(read_dbf=map(.x = value,
                        .f = ~read.dbf(file = .x) %>%
                          as_tibble() %>%
                          janitor::clean_names() %>%
                          select(ubigeo, ano, semana, starts_with(diagnosis)))) %>%
    unnest(cols = c(read_dbf)) %>%
    select(-value) %>%
    group_by(type, year,ubigeo, ano, semana) %>%
    summarise_at(.vars = vars(starts_with(diagnosis)),.funs = sum) %>%
    ungroup() %>%
    select(-year) %>%
    mutate(row_sum=pmap_int(.l = select(.,starts_with(diagnosis)),.f = sum))
}


