# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

library(tidyverse)
library(RPostgreSQL)
library(rgdal)
library(rgeos)
library(sp)

#' @title get.data
#' @description Realiza consultas SQL e obtém os dados necessários
#' dos bancos de dados do TCE. Realiza também joins e manipulações com os
#' dados obtidos para gerar dataframes necessários posteriormente.
#' @param con1 Uma conexão com a base de dados vigia do TCE.
#' @param con2 Uma conexão com a base de dados cabobranco do TCE.
#' @param mapa_paraiba ShapeFile do mapa da Paraíba.
#' @export
get.data <- function(con1, con2, mapa_paraiba) {
  obra <<- dbGetQuery(con1, "select * from t_obra")
  acompanhamento <<- dbGetQuery(con1, "select * from t_acompanhamento")
  complexo <<- dbGetQuery(con1, "select * from t_complexo")
  evolucao <<- dbGetQuery(con1, "select * from t_evolucao")

  jurisdicionado_db2 <<- dbGetQuery(con2, "select * from t_jurisdicionado")
  localidade <<- dbGetQuery(con2, "select * from t_localidade")

  obra.georref <- acompanhamento %>%
      left_join(obra, by = c("fk_obra" = "id")) %>%
      left_join(localidade, by = c("fk_localidade" = "id"))

  obra.filtrada <- obra.georref %>%
      filter(tipo_georeferenciamento != 0)

  obra.georref.corrigido <- obra.filtrada %>%
      rowwise() %>%
      mutate(
          valor_georeferenciamento = ifelse(tipo_georeferenciamento != 1 & tipo_georeferenciamento != 3,
                                            valor_georeferenciamento,
                                            centroide(valor_georeferenciamento))
      ) %>%
      filter(
          tipo_georeferenciamento != 2
      ) %>%
      separate(
          valor_georeferenciamento,
          c("lat", "lon"),
          sep = ",",
          remove = FALSE,
          convert = TRUE
      ) %>%
      filter(
          is.dentro.pb(lat,lon)
      ) %>%
      filter(
        !is.na(lat) & !is.na(lon) & codigo_ibge != 0 & !is.na(codigo_ibge)
      ) %>%
      mutate(
        dentro_municipio =  is.dentro.municipio(lat, lon, codigo_ibge, mapa_paraiba)
      )

  obra.georref.centroide.sumarizado <<- obra.georref.corrigido %>%
      group_by(fk_obra) %>%
      mutate(lat = mean(lat), lon = mean(lon)) %>%
      select(-valor_georeferenciamento) %>%
      filter(!duplicated(fk_obra))
}

#' @title get.data.faltantes
#' @description Realiza consultas SQL e obtém os dados necessários
#' dos bancos de dados do TCE. Realiza também joins e manipulações com os
#' dados obtidos para gerar dataframes necessários posteriormente.
#' @param con1 Uma conexão com a base de dados vigia do TCE.
#' @param con2 Uma conexão com a base de dados cabobranco do TCE.
#' @param mapa_paraiba ShapeFile do mapa da Paraíba.
#' @export
get.data.faltantes <- function(con1, con2, mapa_paraiba) {
    get.data(con1, con2, mapa_paraiba)
    obra.fotos <<- dbGetQuery(con1, "select o.id, fa.num_foto as num_fotos_acompanhamento, fm.num_foto as num_fotos_medicao
                                     from t_obra o,
                                     (select fk_obra, count(arquivo) as num_foto from t_foto_acompanhamento group by fk_obra) fa, 
                                     (select fk_obra, count(arquivo) as num_foto from t_foto_medicao group by fk_obra) fm 
                                     where o.id = fa.fk_obra and o.id = fm.fk_obra")
    obra.medicao <<- dbGetQuery(con1, "select fk_obra, planilha_medicoes, situacao, data_final from t_medicao")
    obra.art <<- dbGetQuery(con1, "select fk_obra, numero_art from t_regularidade")
    obra.proj.basico <<- dbGetQuery(con1, "select fk_obra, projeto_basico from t_recurso_proprio")
} 

#' @title get.georreferencia.inputada
#' @description Retorna um dataframe das obras da Paraíba. Adiciona localização no centro do
#' município para obras que não especificam o georreferenciamento e para obras que especificam
#' o georreferenciamento fora da Paraíba ou fora do município especificado. Adiciona também a coluna
#' is.inputado para indicar quais obras tiveram as coordenadas alteradas.
#' @param obra Dataframe que representa a tabela obra da base de dados
#' @param localidade Dataframe que representa a tabela localidade da base de dados
#' @param tipos.das.obras Dataframe que representa a tabela tipo_obra da base de dados
#' @param municipios Dataframe com os municipios da Paraíba
#' @param obra.georref.centroide.sumarizado Um dataframe com apenas as obras georreferenciadas
#' onde a localização é sumarizada para o centroide dos pontos caso haja mais de um.
#' @param ano.inicio Ano mínimo das obras, só serão retornadas obras deste ano em diante.
#' @export
get.georreferencia.inputada <- function(obra, localidade, tipos.das.obras, municipios, obra.georref.centroide.sumarizado, ano.inicio) {
    obra %>%
        left_join(localidade, by = c("fk_localidade" = "id")) %>%
        left_join(evolucao, by = c("id" = "fk_obra")) %>%
        mutate(ano = lubridate::year(data_inicio_obra)) %>%
        filter(
            ano >= ano.inicio
        ) %>%
        left_join(tipos.das.obras, by = c("fk_tipo_obra" = "id")) %>%
        left_join(municipios, by = "codigo_ibge") %>%
        left_join(obra.georref.centroide.sumarizado %>% select(fk_obra,lat,lon, dentro_municipio), by = c("id" = "fk_obra")) %>%
        mutate(
            lat = ifelse(is.na(lat.y), lat.x, lat.y),
            lon = ifelse(is.na(lon.y), lon.x, lon.y),
            is.inputado = ((is.na(lat.y) & is.na(lon.y)) | (!is.na(dentro_municipio) & dentro_municipio == FALSE))
        ) %>% select(-lat.x, -lat.y, -lon.x, -lon.y)
}

