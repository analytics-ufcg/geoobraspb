library(tidyverse)
library(RPostgreSQL)
library(rgdal)
library(rgeos)
library(sp)

is.dentro.pb <- function(lat,lon) {
  lon.min <- -38.770132
  lat.min <- -8.316999
  lon.max <- -34.786624
  lat.max <- -5.995412
  return(lon.min < lon && lon < lon.max && lat.min < lat && lat < lat.max)
}

is.dentro.municipio <- function(lat, lon, codigo_ibge, mapa_paraiba) {
  mapa_municipio <- subset(mapa_paraiba, GEOCODIG_M == codigo_ibge)
  point <- data.frame(lat = lon, lon = lat)
  point_spatial <- SpatialPoints(point, proj4string = CRS(proj4string(mapa_paraiba)))
  return(rgeos::gContains(mapa_municipio, point_spatial))
}

coord_divide <- function(value, parameter) {
  return(
    ifelse(value < parameter,
           coord_divide(value / 10, parameter),
           value)
  )
}

corrige.coords <- function(coord1, coord2){
  coord1 <- dplyr::if_else(coord1 > 0, coord1 * -1, coord1)
  coord2 <- dplyr::if_else(coord2 > 0, coord2 * -1, coord2)

  lat <- max(coord1, coord2)
  lon <- min(coord1, coord2)

  lat <- coord_divide(lat, -10)
  lon <- coord_divide(lon, -100)

  return(c(lat = lat, lon = lon))
}

format.geo <- function(mat) {
  mat.pb <- data.frame(mat) %>%
    rowwise() %>%
    mutate(
      lat = corrige.coords(X1, X2)["lat"],
      lon = corrige.coords(X1, X2)["lon"]
    ) %>%
    rowwise() %>%
    filter(is.dentro.pb(lat, lon))

  coord1 <- mean(mat.pb$X1)
  coord2 <- mean(mat.pb$X2)

  coords <- corrige.coords(coord1, coord2)

  lat <- coords[1]
  lon <- coords[2]

  paste(lat,lon, sep = ",")
}

centroide <- function(localizacao) {

  coords <- tryCatch(
    {
      parsed.mat <- matrix(jsonlite::fromJSON(localizacao), ncol = 2)

      return(format.geo(parsed.mat))
    },
    error=function(cond) {
      tryCatch(
        {
          lista.loc.format <- strsplit(localizacao, "\\],")[[1]]
          lista.loc.format <- str_replace_all(lista.loc.format, "\\[|\\]", "")
          lista.coord <- unlist(strsplit(lista.loc.format, ","))
          lista.coord <- trimws(lista.coord, which = "both")
          lista.coord.format <- lapply(lista.coord, function(d){
            as.numeric(unlist(strsplit(d, " "))[1])
          })
          parsed.matrix <- matrix(unlist(lista.coord.format), ncol = 2, byrow = TRUE)

          return(format.geo(parsed.matrix))
        },
        error = function(cond) {
          return(localizacao)
        }
      )
      return(localizacao)
    }
  )
  coords
}

get.mapa.paraiba.georref <- function(mapa_paraiba, municipios.georref.porc) {
  mapa_paraiba_georreferenciada <- mapa_paraiba

  mapa_paraiba_georreferenciada@data <- mapa_paraiba_georreferenciada@data %>%
    left_join(municipios.georref.porc,
              by = c("GEOCODIG_M" = "codigo_ibge"))

  mapa_paraiba_georreferenciada
}

get.porc.municipios.georref <- function(dado, municipio.selecionado = 'João Pessoa', ano.inicial = 0, ano.final = 3000) {
  dado %>%
    filter(ano >= ano.inicial,
           ano <= ano.final) %>%
    group_by(codigo_ibge, nome.x) %>%
    summarise(
      possui.georref.mas.tem.coordenadas.fora.municipio =
        sum(
          ifelse(!is.na(dentro_municipio) & dentro_municipio == FALSE, 1, 0)
        ),
      total.obras = n(),
      qtde.georref = sum(!is.inputado),
      porc.georref = (qtde.georref / total.obras) * 100
    ) %>%
    mutate(
      cor.borda = if_else(nome.x == municipio.selecionado, "blue", "black"),
      largura.borda = if_else(nome.x == municipio.selecionado, 5, 1)
    )
}

get.top.10.tipo.obra <- function(dado) {
  dado %>%
    group_by(tipo_obra) %>%
    summarise(quantidade.tipo.obra = n()) %>%
    top_n(10, quantidade.tipo.obra) %>%
    arrange(tipo_obra) %>%
    pull(tipo_obra)
}

get.custos.efetivos <- function(dado) {
  dado.filtrado <- dado %>%
    filter(valor_obra > 1000,
           dimensao > 50) %>%
    rename(nome = nome.x,
           tipo_obra = nome.y) %>%
    filter(tipo_obra != "OUTRAS")

  top.10.tipo.obra <- get.top.10.tipo.obra(dado.filtrado)

  dado.filtrado %>%
    filter(tipo_obra %in% top.10.tipo.obra)
}

get.custo.efetivo.tipo.obra <- function(dado, municipio.selecionado, tipo.obra = "PAVIMENTAÇÃO PARALEPÍPEDO", ano.inicial = 0, ano.final = 3000) {
  dado %>%
    filter(
      tipo_obra == tipo.obra,
      ano >= ano.inicial,
      ano <= ano.final
    ) %>%
    select(valor_obra, dimensao, nome, codigo_ibge) %>%
    mutate(custo.efetivo = valor_obra/dimensao) %>%
    group_by(nome, codigo_ibge) %>%
    summarise(
      custo.efetivo = median(custo.efetivo),
      custo.efetivo.log = log(custo.efetivo)
    ) %>%
    mutate(
      cor.borda = if_else(nome == municipio.selecionado, "blue", "black"),
      largura.borda = if_else(nome == municipio.selecionado, 5, 1)
    )
}

get.mapa.paraiba.custo.efetivo <- function(mapa_paraiba, municipios.custo.efetivo) {
  mapa_paraiba_custo_efetivo <- mapa_paraiba

  mapa_paraiba_custo_efetivo@data <- mapa_paraiba_custo_efetivo@data %>%
    left_join(municipios.custo.efetivo,
              by = c("GEOCODIG_M" = "codigo_ibge"))

  mapa_paraiba_custo_efetivo
}

get.popup.georref <- function(nome.munic, total.obras, qtde.georref, porc.georref, qtde.coordenadas.fora.municipio) {
  paste0("Município: ",
         nome.munic,
         "</br>Total de obras: ",
         total.obras,
         "</br>Quantidade de obras georreferenciadas: ",
         qtde.georref,
         "</br>Obras georreferenciadas (%): ",
         round(porc.georref, 2), "%",
         "</br>Obras com coordenadas fora do município: ",
         qtde.coordenadas.fora.municipio)
}

paleta.de.cores <- function(paleta = "YlOrRd", dado, reverse = FALSE) {
  colors <- colorNumeric(paleta, domain = c(min(dado, na.rm = T), max(dado, na.rm = T)), reverse = reverse)
}

adiciona.poligonos.e.legenda <- function(mapa, cores, valor.municipio, tooltip, janela, titulo, tag_grupo, cor.borda = "black", largura.borda = 1) {
  addPolygons(mapa,
              opacity = 0.5,
              weight = largura.borda,
              fillColor = cores(valor.municipio),
              color = cor.borda,
              label = tooltip,
              popup = janela,
              fillOpacity = 1,
              group = tag_grupo) %>%
    addLegend(position = "bottomright", pal = cores, values = valor.municipio,
              title = titulo,
              opacity = 1)
}

cria.mapa <- function(dado, valor.municipio, tooltip, janela, cores, titulo, tag_grupo, cor.borda = "black", largura.borda = 1) {
  dado %>%
    leaflet() %>%
    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
    adiciona.poligonos.e.legenda(cores, valor.municipio, tooltip, janela, titulo, tag_grupo, cor.borda, largura.borda)
}

dygraph.tipo.obra <- function(dado, tipo.obra) {
  renderDygraph({
    dado %>%
      filter(tipo_obra == tipo.obra) %>%
      mutate(custo.efetivo = valor_obra/dimensao) %>%
      group_by(ano) %>%
      summarise(
        custo.efetivo = median(custo.efetivo)
      ) %>%
      select(ano, custo.efetivo) %>%
      dygraph() %>%
      dyRangeSelector() %>%
      dyLegend(show = "never")
  })
}

plot.ranking.georref <- function(dado, municipio) {
  municipio.selecionado <- dado %>% filter(nome.x == municipio)

  top.24.selecionado <- dado %>%
    arrange(-porc.georref) %>%
    head(24) %>%
    rbind(municipio.selecionado) %>%
    distinct() %>%
    mutate(class = ifelse(nome.x == municipio, "selecionado", "top 24"))

  plot <- top.24.selecionado %>%
    ggplot(aes(x = reorder(nome.x, porc.georref),
               y = porc.georref,
               fill = porc.georref)) +
    geom_bar(stat="identity") +
    guides(fill=FALSE, colour = FALSE) +
    labs(x = "Município",
         y = "Obras georreferenciadas (%)") +
    scale_fill_distiller(palette = "YlOrRd") +
    coord_flip() +
    theme(legend.position="bottom")

  top.25 <- dado %>% arrange(-porc.georref) %>% head(25)

  if ((top.25 %>% filter(municipio == nome.x) %>% ungroup() %>% count()) == 0) {
    plot <- plot +
      labs(title = "Top 24 municípios que mais \ngeorreferenciam + selecionado") +
      facet_grid(class ~ ., scales = "free_y", space = "free_y")
  } else {
    plot <- plot +
      labs(title = "Top 25 municípios que mais \ngeorreferenciam")
  }

  plot +
    geom_text(
      data = filter(top.24.selecionado, municipio == nome.x),
      aes(label = "selecionado"),
      y = max(top.25$porc.georref) / 2
    ) +
    theme_bw()
}

plot.ranking.tipo.obra <- function(dado, municipio) {
  municipio.selecionado <- dado %>% filter(nome == municipio)

  top.24.selecionado <- dado %>%
    arrange(custo.efetivo) %>%
    head(24) %>%
    rbind(municipio.selecionado) %>%
    distinct() %>%
    mutate(class = ifelse(nome == municipio, "selecionado", "top 24"))

  plot <- top.24.selecionado %>%
    ggplot(aes(x = reorder(nome, -custo.efetivo),
               y = custo.efetivo,
               fill = custo.efetivo.log)) +
    geom_bar(stat="identity") +
    guides(fill=FALSE, colour = FALSE) +
    labs(x = "Município",
         y = "Custo efetivo por m2") +
    scale_fill_distiller(palette = "YlOrRd", direction = 1) +
    coord_flip() +
    theme(legend.position="bottom")

  top.25 <- dado %>% arrange(custo.efetivo) %>% head(25)

  if ((top.25 %>% filter(municipio == nome) %>% ungroup() %>% count()) == 0) {
    plot <- plot +
      labs(title = "Top 24 municípios com menor \ncusto efetivo + selecionado") +
      facet_grid(class ~ ., scales = "free_y", space = "free_y")
  } else {
    plot <- plot +
      labs(title = "Top 25 municípios com menor \ncusto efetivo")
  }

  plot +
    geom_text(
      data = filter(top.24.selecionado, municipio == nome),
      aes(label = "selecionado"),
      y = max(top.25$custo.efetivo) / 2
    ) +
    theme_bw()
}

cidade.default <- function(dado, nome) {
    dado %>%
        arrange_(nome) %>%
        head(1) %>%
        pull(nome)
}

add.borda <- function(dado, municipio.selecionado, cor.destacada = "blue", cor.default = "black", borda.destacada = 5, borda.default = 1) {
    dado %>%
        mutate(
            cor.borda = if_else(nome == municipio.selecionado, cor.destacada, cor.default),
            largura.borda = if_else(nome == municipio.selecionado, borda.destacada, borda.default)
        )
}

