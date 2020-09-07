#' .Leer bases de EDA e IRA
#'
#' Funciones para leer y resumin bases de EDA e IRA por ubigeo.
#'
#' @describeIn cdc_read_ira leer y resumir base IRA
#'
#' @param rute_ira ruta a la carpeta con todas las bases de datos crudas de IRA
#' @param year_range rango de años a usar como basal
#' @param diagnosis elegir el diagnostico a evaluar. || para IRA: ira, neu, ngr, hos, dih, deh, sob. || para EDA: daa, col, dis, cop.
#'
#' @import dplyr
#' @import foreign
#' @import tidyr
#' @import janitor
#'
#' @return leer y resumir de EDA e IRA.
#'
#' @export cdc_read_ira
#' @export cdc_read_eda
#' @export xinternal01
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
#' #
#' # # rule 01
#' # # file names with this structure and extension:
#' # # ira_sp(YY).dbf
#' # # where YY is the last two digits of the year
#' # # e.g. data-raw/iras/IRA_SP00.DBF
#' #
#' # # rule 02
#' # # for diagnosis, use this valid string alternatives
#' # diagnosis <- "ira" #ira, neu, ngr, hos, dih, deh, sob
#' # # for multiple diagnosis, use a reprex
#' # # diagnosis <- "ira|neu"
#' #
#' # read timeserie
#' # default: resumen por ubigeo
#' # iradb <- cdc_read_ira(rute_ira = "data-raw/iras", #
#' #                       year_range = 2018:2019,
#' #                       diagnosis = diagnosis)
#' # # iradb %>% count(ano,type) %>% avallecam::print_inf()
#'
#' # puedes agregar otras covariables categóricas
#' # iradb <- cdc_read_ira(rute_ira = "data-raw/iras", #
#' #                       year_range = 2018:2019,
#' #                       diagnosis = diagnosis,
#' #                       extra_covariate = c("etniaproc"))
#'
#' ## IRA DATA DICTIONARY
#'
#' ## value      <chr> "data-raw/iras/ira_
#' ## year       <dbl> 2016, 2016, 2016, 2
#' ## type       <chr> "reference", "refer
#' ## ano        <int> 2016, 2016, 2016, 2
#' ## semana     <int> 51, 51, 48, 45, 43,
#' ## sub_reg_nt <fct> 23, 18, 21, 21, 21,
#' ## red        <fct> 01, 02, 01, 09, 06,
#' ## microred   <fct> 00, 02, 00, 00, 00,
#' ## e_salud    <fct> 230103C101, 180301A
#' ## ubigeo     <fct> 230103, 180301, 210
#' ## ira_m2     <int> 2, 1, 1, 3, 2, 4, 1
#' ## ira_2_11   <int> 17, 14, 13, 2, 4, 1
#' ## ira_1_4a   <int> 53, 37, 13, 3, 13,
#' ## neu_2_11   <int> 1, 0, 0, 0, 0, 1, 0
#' ## neu_1_4a   <int> 0, 1, 1, 1, 1, 0, 1
#' ## hos_m2     <int> 0, 0, 0, 0, 0, 3, 0
#' ## hos_2_11   <int> 1, 0, 0, 0, 0, 1, 0
#' ## hos_1_4a   <int> 0, 1, 1, 0, 1, 0, 1
#' ## ngr_m2     <int> 0, 0, 0, 0, 0, 3, 0
#' ## ngr_2_11   <int> 0, 0, 0, 0, 0, 0, 0
#' ## ngr_1_4a   <int> 0, 0, 0, 0, 0, 0, 0
#' ## dih_m2     <int> 0, 0, 0, 0, 0, 0, 0
#' ## dih_2_11   <int> 0, 0, 0, 0, 0, 0, 0
#' ## dih_1_4a   <int> 0, 0, 0, 1, 0, 0, 0
#' ## deh_m2     <int> 0, 0, 0, 0, 0, 0, 0
#' ## deh_2_11   <int> 0, 0, 0, 0, 0, 0, 0
#' ## deh_1_4a   <int> 0, 0, 0, 0, 0, 0, 0
#' ## sob_2a     <int> 0, 1, 0, 0, 0, 0, 0
#' ## sob_2_4a   <int> 0, 2, 0, 0, 0, 0, 0
#' ## fecha_ing  <date> 2017-05-04, 2016-1
#' ## clave      <fct> 201651230103C101230
#' ## migrado    <fct> NA, NA, NA, NA, NA,
#' ## verifica   <fct> NA, NA, NA, NA, NA,
#' ## etapa      <fct> NA, NA, NA, NA, NA,
#' ## ira_5_9a   <int> 0, 0, 0, 0, 0, 0, 0
#' ## ira_10_19a <int> NA, NA, NA, NA, NA,
#' ## ira_20_59a <int> NA, NA, NA, NA, NA,
#' ## ira_60a    <int> 0, 0, 0, 0, 0, 0, 0
#' ## neu_5_9a   <int> 0, 0, 0, 0, 0, 0, 0
#' ## neu_60a    <int> 0, 0, 1, 0, 0, 0, 0
#' ## hos_5_9a   <int> 0, 0, 0, 0, 0, 0, 0
#' ## hos_60a    <int> 0, 0, 1, 0, 0, 0, 0
#' ## ngr_5_9a   <int> 0, 0, 0, 0, 0, 0, 0
#' ## ngr_60a    <int> 0, 0, 0, 0, 0, 0, 0
#' ## dih_5_9a   <int> 0, 0, 0, 0, 0, 0, 0
#' ## dih_60a    <int> 0, 0, 0, 0, 0, 0, 0
#' ## deh_5_9a   <int> 0, 0, 0, 0, 0, 0, 0
#' ## deh_60a    <int> 0, 0, 0, 0, 0, 0, 0
#' ## sob_5_9a   <int> 0, 0, 0, 0, 0, 0, 0
#' ## sob_10_19a <int> NA, NA, NA, NA, NA,
#' ## sob_20_59a <int> NA, NA, NA, NA, NA,
#' ## sob_60a    <int> 0, 0, 0, 0, 0, 0, 0
#' ## estado     <fct> NA, NA, NA, NA, NA,
#' ## localcod   <fct> NA, NA, NA, NA, NA,
#' ## neu_10_19  <int> 0, 0, 0, 0, 0, 0, 0
#' ## neu_20_59  <int> 0, 0, 0, 0, 0, 2, 3
#' ## hos_10_19  <int> 0, 0, 0, 0, 0, 0, 0
#' ## hos_20_59  <int> 0, 0, 0, 0, 0, 1, 3
#' ## dih_10_19  <int> 0, 0, 0, 0, 0, 0, 0
#' ## dih_20_59  <int> 0, 0, 0, 0, 0, 0, 0
#' ## deh_10_19  <int> 0, 0, 0, 0, 0, 0, 0
#' ## deh_20_59  <int> 0, 0, 0, 0, 0, 0, 0
#' ## etniaproc  <fct> NA, 1, 1, NA, NA, 1
#' ## etnias     <fct> NA, 0, 0, NA, NA, 0
#' ## procede    <fct> NA, 1, 1, NA, NA, 1
#' ## otroproc   <fct> 0, 0, 0, 0, 0, 0, 0
#'
#' #
#' # rule 01
#' # file names with this structure and extension:
#' # eda_sp_(YYYY).dbf
#' # where YYYY is the year
#' # e.g. data-raw/edas/eda_sp_2000.dbf
#' #
#' # # rule 02
#' # # for diagnosis, use this valid string alternatives
#' # diagnosis <- "daa" #col, dis, cop
#' # # for multiple diagnosis, use a reprex
#' # # diagnosis <- "daa|dis"
#'
#' # # read timeserie
#' # # default: resumen por ubigeo
#' # edadb <- cdc_read_eda(rute_eda = "data-raw/edas/",
#' #                       year_range = 2016:2019,
#' #                       diagnosis = diagnosis)
#'
#' # # edadb %>% count(ano,type) %>% avallecam::print_inf()
#'
#' ## EDA DATA DICTIONARY
#'
#' ## value      <chr> "data-raw/edas/eda_sp_
#' ## year       <dbl> 2016, 2016, 2016, 2016
#' ## type       <chr> "reference", "referenc
#' ## ano        <int> 2016, 2016, 2016, 2016
#' ## semana     <int> 50, 51, 52, 52, 46, 50
#' ## sub_reg_nt <fct> 50, 16, 16, 25, 36, 05
#' ## red        <fct> 00, 03, 01, 03, 01, 10
#' ## microred   <fct> 00, 11, 04, 03, 06, 97
#' ## e_salud    <fct> 150105A101, 160404A302
#' ## ubigeo     <fct> 220901, 160404, 160103
#' ## daa_c1     <int> 0, 0, 0, 3, 1, 1, 6, 1
#' ## daa_c1_4   <int> 1, 3, 1, 2, 0, 0, 9, 2
#' ## daa_c5     <int> 0, 3, 0, 1, 0, 7, 3, 3
#' ## daa_d1     <int> 0, 0, 0, 0, 1, 0, 1, 0
#' ## daa_d1_4   <int> 1, 1, 1, 0, 0, 0, 0, 2
#' ## daa_d5     <int> 0, 0, 0, 1, 0, 1, 0, 1
#' ## daa_h1     <int> 0, 0, 0, 0, 0, 0, 0, 0
#' ## daa_h1_4   <int> 0, 0, 1, 0, 0, 0, 0, 0
#' ## daa_h5     <int> 0, 0, 0, 0, 0, 0, 0, 0
#' ## col_c1     <int> 0, 0, 0, 0, 0, 0, 0, 0
#' ## col_c1_4   <int> 0, 0, 0, 0, 0, 0, 0, 0
#' ## col_c5     <int> 0, 0, 0, 0, 0, 0, 0, 0
#' ## col_d1     <int> 0, 0, 0, 0, 0, 0, 0, 0
#' ## col_d1_4   <int> 0, 0, 0, 0, 0, 0, 0, 0
#' ## col_d5     <int> 0, 0, 0, 0, 0, 0, 0, 0
#' ## col_h1     <int> 0, 0, 0, 0, 0, 0, 0, 0
#' ## col_h1_4   <int> 0, 0, 0, 0, 0, 0, 0, 0
#' ## col_h5     <int> 0, 0, 0, 0, 0, 0, 0, 0
#' ## dis_c1     <int> 0, 0, 0, 0, 0, 0, 0, 0
#' ## dis_c1_4   <int> 0, 0, 0, 1, 0, 2, 0, 0
#' ## dis_c5     <int> 0, 0, 0, 1, 0, 0, 0, 0
#' ## dis_d1     <int> 0, 0, 0, 0, 0, 0, 0, 0
#' ## dis_d1_4   <int> 0, 0, 0, 0, 0, 0, 0, 0
#' ## dis_d5     <int> 0, 0, 0, 0, 0, 0, 0, 0
#' ## dis_h1     <int> 0, 0, 0, 0, 0, 0, 0, 0
#' ## dis_h1_4   <int> 0, 0, 0, 0, 0, 0, 0, 0
#' ## dis_h5     <int> 0, 0, 0, 0, 0, 0, 0, 0
#' ## cop_t1     <int> 0, 0, 0, 0, 0, 0, 0, 0
#' ## cop_t1_4   <int> 0, 0, 0, 0, 0, 0, 0, 0
#' ## cop_t5     <int> 0, 0, 0, 0, 0, 0, 0, 0
#' ## cop_p1     <int> 0, 0, 0, 0, 0, 0, 0, 0
#' ## cop_p1_4   <int> 0, 0, 0, 0, 0, 0, 0, 0
#' ## cop_p5     <int> 0, 0, 0, 0, 0, 0, 0, 0
#' ## cop_s1     <int> 0, 0, 0, 0, 0, 0, 0, 0
#' ## cop_s1_4   <int> 0, 0, 0, 0, 0, 0, 0, 0
#' ## cop_s5     <int> 0, 0, 0, 0, 0, 0, 0, 0
#' ## fecha_ing  <date> 2016-12-19, 2016-12-2
#' ## clave      <fct> 201650150105A101220901
#' ## migrado    <fct> NA, NA, NA, NA, NA, NA
#' ## verifica   <fct> NA, NA, NA, NA, NA, NA
#' ## etapa      <fct> NA, NA, NA, NA, NA, NA
#' ## estado     <fct> NA, NA, NA, NA, NA, NA
#' ## etniaproc  <fct> NA, 1, 1, 4, 1, 3, 1,
#' ## etnias     <fct> NA, 0, 0, 61, NA, 8, 0
#' ## procede    <fct> NA, 3, 3, 3, 3, 1, 1,
#' ## otroproc   <fct> 0, 0, 0, 0, 0, 0, 0, 0
#'

cdc_read_ira <- function(rute_ira,year_range=2013:2019,diagnosis="ira",extra_covariate=NULL) {
  #list files with dbf extension
  list.files(path = rute_ira,
             pattern = "(.DBF|.dbf)", #archivos deben ser DBF
             full.names = T) %>%
    #make a tibble from it
    enframe(name = NULL) %>%

    # use common workflow
    xinternal01(year_range = year_range,diagnosis = diagnosis,extra_covariate = extra_covariate)
}


#' @describeIn cdc_read_ira leer y resumir base de EDA
#' @inheritParams cdc_read_ira
#' @param rute_eda ruta a la carpeta con todas las bases de datos crudas de EDA

cdc_read_eda <- function(rute_eda,year_range=2013:2019,diagnosis="daa",extra_covariate=NULL) {
  #list files with dbf extension
  list.files(path = rute_eda,
             pattern = "(.DBF|.dbf)", #archivos deben ser DBF
             full.names = T) %>%
    #make a tibble from it
    enframe(name = NULL) %>%

    # use common workflow
    xinternal01(year_range = year_range,diagnosis = diagnosis,extra_covariate = extra_covariate)
}

#' @describeIn cdc_read_ira internal function for EDA and IRA
#' @inheritParams cdc_read_ira
#' @param data data

xinternal01 <- function(data,year_range,diagnosis,extra_covariate) {
  data %>%
    #create year
    # ARCHIVOS DEBEN CUMPLIR CON EL REPREX
    # mutate(year=str_replace(value,"(.+)(sp|SP)(..)\\.(dbf|DBF)","20\\3")) %>%
    # mutate(year=as.numeric(year)) %>%
    mutate(year=str_replace(value,"(.+)(..)\\.(dbf|DBF)","20\\2")) %>% # equivalent to cdc_read_eda
    # mutate(year=if_else(str_length(year)==2,str_c("20",year),year)) %>%
    mutate(year=as.numeric(year)) %>%

    #keep basal and test
    # basal years are defined as a vector in arguments
    filter(magrittr::is_in(year,year_range)) %>%
    # filter(magrittr::is_in(year,year_range)|type=="test") %>%
    #define test year
    # always is going to be the last year of the defined range
    mutate(type=if_else(year==max(year),"test","reference")) %>%
    # mutate(type=if_else(is.na(year),"test","reference")) %>%

    #read file names
    mutate(read_dbf=map(.x = value,
                        .f = ~foreign::read.dbf(file = .x) %>%
                          as_tibble() %>%
                          janitor::clean_names() %>%
                          # use select helper to identify different damages
                          select(ubigeo, ano, semana, matches(diagnosis), any_of(extra_covariate))
    )
    ) %>%
    unnest(cols = c(read_dbf)) %>% #glimpse()
    # # remove file rutes
    select(-value) %>%
    #sum all values per ubigeo-year-week
    # note: original registries are at a ee.ss level
    group_by(across(-matches(diagnosis))) %>%
    # group_by(type, year,ubigeo, ano, semana) %>%
    summarise_at(.vars = vars(matches(diagnosis)),.funs = sum, na.rm=T) %>%
    ungroup() %>%
    select(-year) %>%
    #make horizontal rowwise sums
    mutate(row_sum=pmap_int(.l = select(.,matches(diagnosis)),.f = sum, na.rm=T))
}

