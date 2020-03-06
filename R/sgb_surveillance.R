#' @title sgb surveillance specific functions
#'
#' @description clean sgb surveillance dataset
#'
#' @describeIn sgb_replicate_filter filter replicates and store them in excel file
#'
#' @param data raw data input file
#' @param output_rute output rute name
#'
#' @import dplyr
#'
#' @return replicates identification and removal functions
#'
#' @export sgb_replicate_filter
#' @export sgb_replicate_remove
#'
#' @examples
#'
#'

sgb_replicate_filter <- function(data,output_rute) {
  data %>%
    #dar orden y fila de identificacion
    arrange(ano,dni,desc(fecha_digitado_web)) %>%
    rownames_to_column() %>%
    group_by(ano,dni) %>% #apepat,apemat,nombres
    filter(n()>1) %>%
    ungroup() %>%
    #count(tipo_dx)
    #select(fecha_digitado_web,fecha_ini,ano:sexo) %>% view()
    write_xlsx(output_rute)
}

#' @describeIn sgb_replicate_filter remove replicates
#' @inheritParams sgb_replicate_filter
#' @param input_rute input of reviewed rpelicate output with column "eliminar_casos"

sgb_replicate_remove <- function(data,input_rute) {

  eliminar_replicas <- read_excel(input_rute) %>%
    #count(eliminar_casos)
    filter(eliminar_casos=="eliminar") %>%
    arrange(ano,dni,desc(fecha_digitado_web)) %>%
    select(eliminar_casos,noti_clave) %>%
    filter(!is.na(noti_clave))

  data %>%
    #dar orden y fila de identificacion
    arrange(ano,dni,desc(fecha_digitado_web)) %>%
    rownames_to_column() %>%
    #retirar replicas: 1839
    distinct(ano,dni,.keep_all = TRUE) %>% #apepat,apemat
    #retirar identificados de forma manua: 1836
    filter(!is_in(noti_clave,eliminar_replicas$noti_clave))
}
