if (!require("leaflet")) install.packages("leaflet"); library("leaflet")
if (!require("shinydashboard")) install.packages("shinydashboard"); library("shinydashboard")
if (!require("rlang")) install.packages("rlang"); library("rlang")
if (!require("dplyr")) install.packages("dplyr"); library("dplyr")
if (!require("raster")) install.packages("raster"); library("raster")
if (!require("shiny")) install.packages("shiny"); library("shiny")
if (!require("reshape")) install.packages("reshape"); library("reshape")
if (!require("ggplot2")) install.packages("ggplot2"); library("ggplot2")
if (!require("plotly")) install.packages("plotly"); library("plotly")
if (!require("DT")) install.packages("DT"); library("DT")
if (!require("sf")) install.packages("sf"); library("sf")
if (!require("stringr")) install.packages("stringr"); library("stringr")
if (!require("lubridate")) install.packages("lubridate"); library("lubridate")
if (!require("tidyr")) install.packages("tidyr"); library("tidyr")
if (!require("shinyjs")) install.packages("shinyjs"); library("shinyjs")
if (!require("heatmaply")) install.packages("heatmaply"); library("heatmaply")
if (!require("shinyHeatmaply")) install.packages("shinyHeatmaply"); library("shinyHeatmaply")
if (!require("catmaply")) install.packages("catmaply"); library("catmaply")
if (!require("RColorBrewer")) install.packages("RColorBrewer"); library("RColorBrewer")
if (!require("tidyverse")) install.packages("tidyverse"); library("tidyverse")
if (!require("colorspace")) install.packages("colorspace"); library("colorspace")
if (!require("readxl")) install.packages("readxl"); library("readxl")
if (!require("writexl")) install.packages("writexl"); library("writexl")
if (!require("leaflet.extras")) install.packages("leaflet.extras"); library("leaflet.extras")
if (!require("shinythemes")) install.packages("shinythemes"); library("shinythemes")
if (!require("visNetwork")) install.packages("visNetwork"); library("visNetwork")

library(visNetwork)
library(readxl)
library(tidyr)
library(dplyr)
library(shiny)
library(bslib)
library(leaflet)
library(lubridate)
library(leaflet.extras)
library(tidyverse)
library(DT)
library(sf)


# ============================================================================================================================================================
#                                                          Importación de capas y datos
# ============================================================================================================================================================

options(shiny.reactlog = TRUE) 
options(show.error.messages = TRUE)
cat("\014")  
rm(list = ls())
options(scipen = 999)    
options(expressions = 500000)
Nombres <- "COMPLETOS"

CategoriasICA <- read.csv("ERA_CategoriasICA.csv", fileEncoding = "utf-8", stringsAsFactors = FALSE, check.names = FALSE)
ICA_datos     <- read.csv("ERA_ICA_datos.csv", fileEncoding = "utf-8", stringsAsFactors = FALSE, check.names = FALSE)
ICA_estaciones<- read.csv("ERA_ICA_estaciones.csv", fileEncoding = "utf-8", stringsAsFactors = FALSE, check.names = FALSE)
CategoriasIMA <- read.csv("ERA_CategoriasIMA.csv", fileEncoding = "utf-8", stringsAsFactors = FALSE, check.names = FALSE)
IMA_datos     <- read.csv("ERA_IMA_datos.csv", fileEncoding = "utf-8", stringsAsFactors = FALSE, check.names = FALSE)
IMA_estaciones<- read.csv("ERA_IMA_estaciones.csv", fileEncoding = "utf-8", stringsAsFactors = FALSE, check.names = FALSE)

RedHidrica <- read_sf(dsn = "RedHidrica.shp")
RedHidrica <- st_zm(RedHidrica, drop = TRUE, what = "ZM")
River      <- RedHidrica 

Municipios <- read_sf("MunicipiosRisaralda.shp")
ICA_datos       <- merge(ICA_datos, ICA_estaciones, by = "Estacion")
ICA_datos$Fecha <- as.Date(mdy(ICA_datos$Fecha))

d <- ICA_datos
d$Year <- year(d$Fecha)
d$Val  <- 1
d <- aggregate(d[, "Val", drop = FALSE], list(Estacion = d$Estacion, Year = d$Year, NOMBRE = d$NOMBRE), sum)
d$Val  <- 1
d <- aggregate(d[, "Val", drop = FALSE], list(Estacion = d$Estacion, NOMBRE = d$NOMBRE), sum)

ICA_Tendencias <- d[d$Val > 2, ]
ICA_datos$Categoria_ICA_IDEAM  <- "Sin informacion"
ICA_datos$Categoria_ICA_CARDER <- "Sin informacion"
CatICA <- CategoriasICA[CategoriasICA$Variable == "ICA IDEAM", ]

for (i in 1:nrow(CatICA)) {
  m_ideam  <- !is.na(ICA_datos$`ICA-5 [IDEAM]`) & ICA_datos$`ICA-5 [IDEAM]` > CatICA$Vmin[i] & ICA_datos$`ICA-5 [IDEAM]` <= CatICA$Vmax[i]
  m_carder <- !is.na(ICA_datos$`ICA-6 [CARDER]`) & ICA_datos$`ICA-6 [CARDER]` > CatICA$Vmin[i] & ICA_datos$`ICA-6 [CARDER]` <= CatICA$Vmax[i]
  ICA_datos$Categoria_ICA_IDEAM[m_ideam]   <- CatICA$Categoria[i]
  ICA_datos$Categoria_ICA_CARDER[m_carder] <- CatICA$Categoria[i]
}

ICA_datos$Codigo <- ICA_datos$Estacion
ICA_datos1 <- ICA_datos[, c("Estacion", "Codigo", "NOMBRE", "Fecha","ICA-5 [IDEAM]", "Categoria_ICA_IDEAM","ICA-6 [CARDER]", "Categoria_ICA_CARDER","LATITUD", "LONGITUD", "ALTITUD", "CORRIENTE", "CUENCA")]
ICA_estaciones$Codigo <- ICA_estaciones$Estacion
ICA_datos1 <- unique(ICA_datos1)

if (Nombres == "COMPLETOS") {
  ICA_datos1$Estacion     <- paste0(ICA_datos1$Estacion, "-", ICA_datos1$NOMBRE)
  ICA_Tendencias$Estacion <- paste0(ICA_Tendencias$Estacion, "-", ICA_Tendencias$NOMBRE)
  ICA_estaciones$Estacion <- paste0(ICA_estaciones$Estacion, "-", ICA_estaciones$NOMBRE)
}

ICA_estaciones <- ICA_estaciones[, c("Estacion", "Codigo", "NOMBRE", "LATITUD", "LONGITUD", "ALTITUD", "CORRIENTE", "CUENCA")]
IMA_datos       <- merge(IMA_datos, IMA_estaciones, by = "Estacion")
IMA_datos$Fecha <- as.Date(mdy(IMA_datos$Fecha))

d <- IMA_datos
d$Year <- year(d$Fecha)
d$Val  <- 1
d <- aggregate(d[, "Val", drop = FALSE], list(Estacion = d$Estacion, Year = d$Year, NOMBRE = d$NOMBRE), sum)
d$Val  <- 1
d <- aggregate(d[, "Val", drop = FALSE], list(Estacion = d$Estacion, NOMBRE = d$NOMBRE), sum)

IMA_Tendencias <- d[d$Val > 2, ]
IMA_datos$Categoria_IMA <- "Sin informacion"
CatIMA <- CategoriasIMA[CategoriasIMA$Variable == "IMA", ]

for (i in 1:nrow(CatIMA)) {
  m_ima <- !is.na(IMA_datos$IMA) & IMA_datos$IMA > CatIMA$Vmin[i] & IMA_datos$IMA <= CatIMA$Vmax[i]
  IMA_datos$Categoria_IMA[m_ima] <- CatIMA$Categoria[i]
}

IMA_datos$Codigo <- IMA_datos$Estacion
IMA_datos1 <- IMA_datos[, c("Estacion", "Codigo", "NOMBRE", "Fecha", "IMA", "Categoria_IMA","Familias", "LATITUD", "LONGITUD", "ALTITUD", "CORRIENTE", "CUENCA")]
IMA_estaciones$Codigo <- IMA_estaciones$Estacion
IMA_datos1 <- unique(IMA_datos1)

if (Nombres == "COMPLETOS") {
  IMA_datos1$Estacion     <- paste0(IMA_datos1$Estacion, "-", IMA_datos1$NOMBRE)
  IMA_Tendencias$Estacion <- paste0(IMA_Tendencias$Estacion, "-", IMA_Tendencias$NOMBRE)
  IMA_estaciones$Estacion <- paste0(IMA_estaciones$Estacion, "-", IMA_estaciones$NOMBRE)
}

IMA_estaciones <- IMA_estaciones[, c("Estacion", "Codigo", "NOMBRE", "LATITUD", "LONGITUD", "ALTITUD", "CORRIENTE", "CUENCA")]
rm(d, i, CatICA, CatIMA, m_ideam, m_carder, m_ima)
gc(verbose = FALSE)

Perfiles_AbscisaInicio = read.csv("ERA_Perfiles_AbscisaInicio.csv", fileEncoding="utf-8", stringsAsFactors=F, check.names=F)
Perfilesshp = read_sf(dsn="CALIDAD_RIOS_TRAMOS.shp")
Perfilesshp = st_zm(Perfilesshp, drop = T, what = "ZM")
Perfilesshp = Perfilesshp[order(Perfilesshp$Codrio),]
Perfilesshp$Abscisakm = NA
Perfilesshp$N = 1:nrow(Perfilesshp)

for (i in 1:length(unique(Perfilesshp$NOMBRE))){
  Corriente = unique(Perfilesshp$NOMBRE)[i]
  Perfilesshp1 = Perfilesshp[Perfilesshp$NOMBRE==Corriente,]
  Perfilesshp$Abscisakm[Perfilesshp1$N[1]] = Perfiles_AbscisaInicio[Perfiles_AbscisaInicio$Name==Corriente,"Abscisa_Inicio_Km"] + Perfilesshp1$Length[1]
  for (j in 2:nrow(Perfilesshp1)){
    Perfilesshp$Abscisakm[Perfilesshp1$N[j]] = Perfilesshp$Abscisakm[Perfilesshp1$N[j]-1] + Perfilesshp1$Length[j]
  }
}

Perfilesshp$ID = paste0(Perfilesshp$ESTWQ, "-", Perfilesshp$Codrio)
PerfilesCapasMapa <- read.csv("ERA_PerfilesCapasMapa.csv", fileEncoding = "utf-8", stringsAsFactors = FALSE, check.names = FALSE, skip = 1)

Perfiles_Parametros <- read.csv("ERA_Perfiles_Parametros.csv", fileEncoding = "utf-8", stringsAsFactors = FALSE, check.names = FALSE)
Perfiles_Parametros <- Perfiles_Parametros[!is.na(Perfiles_Parametros$Incluir), ]
Perfiles_Parametros <- Perfiles_Parametros[order(Perfiles_Parametros$Incluir), ]

Perfiles_Datos <- read.csv("ERA_Perfiles_WQ_Depurados.csv", stringsAsFactors = FALSE, fileEncoding = "utf-8", check.names = FALSE)
Perfiles_Datos <- Perfiles_Datos[, c("Fecha", "NOMBRE_ESTACION", "CODIGO", "Tramo", Perfiles_Parametros$Parametros)]
Perfiles_Datos <- unique(Perfiles_Datos)
Perfiles_Datos <- Perfiles_Datos[!is.na(Perfiles_Datos$Tramo), ]

Perfiles_Datos$ID <- paste0(Perfiles_Datos$CODIGO, "-", Perfiles_Datos$Tramo)
Perfiles_Datos <- merge(Perfiles_Datos, Perfilesshp, by.x = "ID", by.y = "ID")
Perfiles_Datos$AbcisaNombre <- paste0("Km ", round(Perfiles_Datos$Abscisakm, 1), " - ", Perfiles_Datos$NOMBRE_ESTACION)
Perfiles_Datos <- merge(Perfiles_Datos, ICA_estaciones[, c("Codigo", "LONGITUD", "LATITUD", "NOMBRE")], by.x = "CODIGO", by.y = "Codigo")

names(Perfiles_Datos)[names(Perfiles_Datos) == "NOMBRE.y"] <- "NOMBRE_COMPLETO"
names(Perfiles_Datos)[names(Perfiles_Datos) == "NOMBRE.x"] <- "NOMBRE"

PerfilesMetas <- read.csv("ERA_Perfiles_Metas.csv", stringsAsFactors = FALSE, check.names = FALSE)
PerfilesMetas$ID <- paste0(PerfilesMetas$Estacion, "-", PerfilesMetas$`Tramo rio`)
PerfilesMetas <- merge(Perfilesshp, PerfilesMetas, by.x = "ESTWQ", by.y = "Estacion", all.y = TRUE)
PerfilesMetas <- PerfilesMetas[order(PerfilesMetas$Meta, PerfilesMetas$Rio, PerfilesMetas$Abscisakm), ]
PerfilesMetas$AbcisaNombre <- paste0("Km ", round(PerfilesMetas$Abscisakm, 1), " - ", PerfilesMetas$NOMBRE_ESTACION)
PerfilesMetas <- PerfilesMetas[!is.na(PerfilesMetas$Abscisakm), ]

VALORES <- list()
rm(Corriente, Perfilesshp1)
gc(verbose = FALSE)

tramos_escenarios_shp <- read_sf(dsn = "TRAMOS_PORH.shp")
tramos_escenarios     <- st_zm(tramos_escenarios_shp, drop = TRUE, what = "ZM")
tramos_escenarios     <- st_transform(tramos_escenarios, crs = 4326)

if ("distancia" %in% names(tramos_escenarios)) {
  tramos_escenarios$distancia <- as.numeric(as.character(tramos_escenarios$distancia))
}

meses_cols <- c("ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "sep", "oct", "nov", "dic")
for (m in meses_cols) {
  if (m %in% names(tramos_escenarios)) {
    tramos_escenarios[[m]] <- as.numeric(as.character(tramos_escenarios[[m]]))
  }
}

tramos_escenarios_long <- tramos_escenarios %>%
  st_drop_geometry() %>%
  mutate(
    distancia = as.numeric(distancia),
    etiqueta_tramo = paste0(RIO, " | ", TRAMO, " (", Descripci, ")")
  ) %>%
  pivot_longer(
    cols = all_of(meses_cols),
    names_to = "mes_id",
    values_to = "escenario_valor"
  ) %>%
  mutate(
    mes_nombre = factor(
      mes_id,
      levels = meses_cols,
      labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", 
                 "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
    ),
    mes_num = as.numeric(mes_nombre)
  )

norm_txt <- function(x) {
  x <- stringi::stri_trans_general(as.character(x), "Latin-ASCII")
  toupper(gsub("[^A-Za-z0-9]", "", x))
}

buscar_col <- function(nms, candidatos) {
  n_norm <- norm_txt(nms)
  for (cnd in candidatos) {
    i <- match(norm_txt(cnd), n_norm)
    if (!is.na(i)) return(nms[i])
  }
  NA_character_
}

col_o_na <- function(df, col) {
  if (is.na(col)) rep(NA_character_, nrow(df)) else as.character(df[[col]])
}

horizontes_metas <- c("Corto Plazo (5 años)", "Mediano Plazo (10 años)", "Largo Plazo (20 años)")
colores_horizontes_metas <- setNames(c("#1022e8", "#10d6e8", "#1080e8"), horizontes_metas)
rios_meta <- c("Río Otún" = "RO", "Quebrada Dos Quebradas" = "Q2Q")

normalizar_horizonte <- function(x) {
  n <- norm_txt(x)
  dplyr::case_when(
    grepl("^CORTO", n)   ~ horizontes_metas[1],
    grepl("^MEDIANO", n) ~ horizontes_metas[2],
    grepl("^LARGO", n)   ~ horizontes_metas[3],
    TRUE ~ NA_character_
  )
}

codigo_rio_meta <- function(x) {
  n <- norm_txt(x)
  dplyr::case_when(
    grepl("OTUN", n)              ~ "RO",
    grepl("DOSQUEBRADAS|Q2Q", n)  ~ "Q2Q",
    TRUE ~ NA_character_
  )
}

llave_desde_id_meta <- function(x) {
  id  <- norm_txt(x)
  cod <- dplyr::case_when(startsWith(id, "OTR") ~ "RO", startsWith(id, "Q2Q") ~ "Q2Q", TRUE ~ NA_character_)
  num <- ifelse(grepl("[0-9]$", id), sub(".*[^0-9]([0-9]+)$", "\\1", id), NA_character_)
  ifelse(!is.na(cod) & !is.na(num), paste0(cod, "_TR", num), NA_character_)
}


tramos_meta_shp <- read_sf("Tramos_PORH_estaciones_codrio_estqw_usos.shp")
tramos_meta_shp <- st_zm(tramos_meta_shp, drop = TRUE, what = "ZM")

tramos_meta_shp$long_km <- as.numeric(sf::st_length(tramos_meta_shp)) / 1000
tramos_meta_shp <- st_transform(tramos_meta_shp, crs = 4326)

nms_shp     <- names(tramos_meta_shp)
col_rio     <- buscar_col(nms_shp, c("RIO"))
col_tramo   <- buscar_col(nms_shp, c("TRAMO"))
col_idmeta  <- buscar_col(nms_shp, c("ID-Meta", "ID_Meta", "IDMeta"))
col_down    <- buscar_col(nms_shp, c("Downstream"))
col_eini    <- buscar_col(nms_shp, c("ESTInicio"))
col_efin    <- buscar_col(nms_shp, c("ESTFinal"))
if (is.na(col_rio) || is.na(col_tramo)) stop("El shapefile de tramos debe tener las columnas RIO y TRAMO.")

tramos_meta_shp$rio_shp   <- as.character(tramos_meta_shp[[col_rio]])
tramos_meta_shp$tramo_shp <- suppressWarnings(as.integer(as.character(tramos_meta_shp[[col_tramo]])))
tramos_meta_shp$est_ini   <- col_o_na(tramos_meta_shp, col_eini)
tramos_meta_shp$est_fin   <- col_o_na(tramos_meta_shp, col_efin)
tramos_meta_shp$down_shp  <- suppressWarnings(as.numeric(col_o_na(tramos_meta_shp, col_down)))
tramos_meta_shp$cod_rio   <- codigo_rio_meta(tramos_meta_shp$rio_shp)

llave_id <- if (!is.na(col_idmeta)) llave_desde_id_meta(tramos_meta_shp[[col_idmeta]]) else rep(NA_character_, nrow(tramos_meta_shp))
llave_rt <- ifelse(!is.na(tramos_meta_shp$cod_rio) & !is.na(tramos_meta_shp$tramo_shp),
                   paste0(tramos_meta_shp$cod_rio, "_TR", tramos_meta_shp$tramo_shp), NA_character_)
tramos_meta_shp$llave <- dplyr::coalesce(llave_rt, llave_id)

tramos_meta_shp <- tramos_meta_shp %>%
  arrange(cod_rio, desc(down_shp), tramo_shp) %>%
  group_by(cod_rio) %>%
  mutate(abscisa_fin = cumsum(long_km),
         abscisa_ini = abscisa_fin - long_km) %>%
  ungroup()

perfiles_metas_excel <- read_excel("ERA_Perfiles_Metas_Nuevas_Final.xlsx") %>%
  select(-contains("Unnamed"))

cols_metadatos <- c("Identificador", "Meta", "Rio", "Tramo", "Uso")
params_actual  <- setdiff(names(perfiles_metas_excel), cols_metadatos)

perfiles_metas_excel <- perfiles_metas_excel %>%
  mutate(
    across(all_of(params_actual), ~ suppressWarnings(as.numeric(as.character(.x)))),
    across(all_of(params_actual), ~ dplyr::na_if(.x, 0)),
    Uso       = trimws(as.character(Uso)),
    Horizonte = normalizar_horizonte(Meta),
    llave     = paste0(codigo_rio_meta(Rio), "_TR", Tramo)
  )

metas_ant_raw <- read.csv("ERA_Perfiles_Metas.csv", stringsAsFactors = FALSE,
                          check.names = FALSE, fileEncoding = "UTF-8")
cols_meta_ant <- c("Meta", "Rio", "Tramo", "Nombre Tramo", "Estacion", "Tramo rio")

ph_txt <- gsub(",", ".", ifelse(is.na(metas_ant_raw$pH), "", as.character(metas_ant_raw$pH)))
ph_num <- regmatches(ph_txt, gregexpr("[0-9]+\\.?[0-9]*", ph_txt))
metas_ant_raw$`pH min` <- vapply(ph_num, function(v) if (length(v) >= 1) as.numeric(v[1]) else NA_real_, numeric(1))
metas_ant_raw$pH       <- vapply(ph_num, function(v) if (length(v) >= 2) as.numeric(v[2]) else NA_real_, numeric(1))

params_ant <- setdiff(names(metas_ant_raw), cols_meta_ant)

perfiles_metas_antiguas <- metas_ant_raw %>%
  mutate(
    across(all_of(params_ant), ~ suppressWarnings(as.numeric(as.character(.x)))),
    Horizonte = normalizar_horizonte(Meta),
    llave     = paste0(codigo_rio_meta(Rio), "_TR", Tramo)
  ) %>%
  distinct(llave, Horizonte, .keep_all = TRUE)

metas_actual_long <- perfiles_metas_excel %>%
  select(llave, Horizonte, all_of(params_actual)) %>%
  pivot_longer(all_of(params_actual), names_to = "Parametro", values_to = "meta_actual")

metas_anterior_long <- perfiles_metas_antiguas %>%
  select(llave, Horizonte, all_of(params_ant)) %>%
  pivot_longer(all_of(params_ant), names_to = "Parametro", values_to = "meta_anterior")

metas_comparacion_long <- full_join(metas_actual_long, metas_anterior_long,
                                    by = c("llave", "Horizonte", "Parametro")) %>%
  mutate(diferencia = meta_actual - meta_anterior)

params_con_datos <- metas_comparacion_long %>%
  group_by(Parametro) %>%
  summarise(ok = any(!is.na(meta_actual) | !is.na(meta_anterior)), .groups = "drop") %>%
  filter(ok) %>% pull(Parametro)
parametros_calidad <- intersect(c(params_actual, params_ant), params_con_datos)

info_tramos_meta <- perfiles_metas_excel %>%
  distinct(llave, Rio, Tramo, Uso) %>%
  rename(rio_meta = Rio, tramo_meta = Tramo, uso_meta = Uso)

tabla_tramos_meta <- tramos_meta_shp %>%
  st_drop_geometry() %>%
  filter(!is.na(llave)) %>%
  arrange(cod_rio, abscisa_ini) %>%
  group_by(llave, cod_rio) %>%
  summarise(abs_ini = min(abscisa_ini), abs_fin = max(abscisa_fin),
            est_ini_tramo = dplyr::first(est_ini), est_fin_tramo = dplyr::last(est_fin),
            .groups = "drop") %>%
  left_join(info_tramos_meta, by = "llave")

# ==============================================================================
# MODELO DE CALIDAD: ANÁLISIS DE TRAMOS (ANTERIORES vs ACTUALES)
#   Anterior: CALIDAD_RIOS_TRAMOS.shp | Actual: Tramos_PORH_estaciones_codrio_estqw_usos.shp
#   Estaciones: PUNTOS_OTUN.shp y PUNTOS_Q2Q.shp
# ==============================================================================

# Estaciones con cambio identificadas (selección hecha en ArcGIS Pro sobre PUNTOS_OTUN).
# Edite esta lista si cambia la selección: se cruza por código normalizado (SUP - 60 = SUP60).
codigos_cambio_identificados <- c("SUP - 60", "SUP 584", "SUP - 558", "SUP - 25", "SUP - 191", "SUP - 80")

largo_km <- function(x) {
  y <- tryCatch(st_transform(x, 9377), error = function(e) st_transform(x, 3116))
  as.numeric(sf::st_length(y)) / 1000
}

# Corrige textos con codificación mala (CP1252 leído como UTF-8, o "Ã³"-mojibake)
fix_enc <- function(x) {
  x <- as.character(x)
  bad <- !is.na(x) & !validUTF8(x)
  x[bad] <- iconv(x[bad], "CP1252", "UTF-8")
  moj <- !is.na(x) & grepl("\u00c3|\u00c2", x)
  if (any(moj)) {
    y <- iconv(x[moj], "UTF-8", "CP1252")
    ok <- !is.na(y)
    Encoding(y) <- "UTF-8"
    x[moj][ok] <- y[ok]
  }
  x
}

# Busca una columna cuyo nombre (solo letras) empieza por un prefijo: "camp" -> Campaña / Campana
col_prefijo <- function(nms, prefijo) {
  k <- which(startsWith(tolower(gsub("[^A-Za-z]", "", nms, useBytes = TRUE)), prefijo))
  if (length(k)) nms[k[1]] else NA_character_
}

html_esc <- function(x) htmltools::htmlEscape(ifelse(is.na(x) | !nzchar(as.character(x)), "-", as.character(x)))
lbl_rio  <- function(cod) names(rios_meta)[match(cod, rios_meta)]

# --- Capa ANTERIOR ------------------------------------------------------------
tramos_ant_shp <- read_sf("CALIDAD_RIOS_TRAMOS.shp") %>%
  st_zm(drop = TRUE, what = "ZM") %>%
  st_transform(4326)
na_ <- names(tramos_ant_shp)
c_cod_a <- buscar_col(na_, c("Codrio", "CodRio"))
c_trm_a <- buscar_col(na_, c("TRAMO"))
c_est_a <- buscar_col(na_, c("ESTWQ", "ESTQW"))
c_rio_a <- buscar_col(na_, c("RIO"))
c_nom_a <- buscar_col(na_, c("NOMBRE"))
c_uso_a <- buscar_col(na_, c("USO"))
if (is.na(c_trm_a)) stop("CALIDAD_RIOS_TRAMOS.shp debe tener la columna TRAMO.")

tramos_ant_shp$codrio  <- suppressWarnings(as.integer(as.character(col_o_na(tramos_ant_shp, c_cod_a))))
tramos_ant_shp$tramo_n <- suppressWarnings(as.integer(gsub("\\D", "", col_o_na(tramos_ant_shp, c_trm_a))))
tramos_ant_shp$est_ant <- fix_enc(col_o_na(tramos_ant_shp, c_est_a))
tramos_ant_shp$uso_ant <- fix_enc(col_o_na(tramos_ant_shp, c_uso_a))
tramos_ant_shp$cod_rio <- dplyr::coalesce(
  codigo_rio_meta(col_o_na(tramos_ant_shp, c_rio_a)),
  codigo_rio_meta(col_o_na(tramos_ant_shp, c_nom_a)),
  dplyr::case_when(tramos_ant_shp$codrio %/% 100 == 8 ~ "RO",
                   tramos_ant_shp$codrio %/% 100 == 3 ~ "Q2Q")
)
tramos_ant_shp$long_km <- largo_km(tramos_ant_shp)
tramos_ant_shp$qw_n    <- norm_txt(tramos_ant_shp$est_ant)

# --- Capa ACTUAL (reutiliza tramos_meta_shp ya cargado arriba) -----------------
tramos_act_shp <- tramos_meta_shp
na2 <- names(tramos_act_shp)
c_cod_n  <- buscar_col(na2, c("CodRio", "Codrio"))
c_desc_n <- col_prefijo(na2, "descrip")
c_qw_n   <- buscar_col(na2, c("ESTQW", "ESTWQ"))

tramos_act_shp$codrio   <- suppressWarnings(as.integer(as.character(col_o_na(tramos_act_shp, c_cod_n))))
tramos_act_shp$desc_seg <- fix_enc(col_o_na(tramos_act_shp, c_desc_n))
tramos_act_shp$est_qw   <- fix_enc(col_o_na(tramos_act_shp, c_qw_n))
tramos_act_shp$cod_rio  <- dplyr::coalesce(tramos_act_shp$cod_rio, sub("_TR.*$", "", tramos_act_shp$llave))
tramos_act_shp$long_km2 <- largo_km(tramos_act_shp)
tramos_act_shp$ini_n    <- norm_txt(tramos_act_shp$est_ini)
tramos_act_shp$fin_n    <- norm_txt(tramos_act_shp$est_fin)

# Tramo anterior de cada segmento actual: se cruza por CodRio (las divisiones nuevas
# conservan el CodRio del segmento anterior); si no hay coincidencia, por cercanía espacial
map_ant <- tramos_ant_shp %>%
  st_drop_geometry() %>%
  filter(!is.na(codrio), !is.na(tramo_n)) %>%
  group_by(codrio) %>%
  summarise(tramo_ant = dplyr::first(tramo_n), .groups = "drop")
tramos_act_shp <- dplyr::left_join(tramos_act_shp, map_ant, by = "codrio")

falt <- which(is.na(tramos_act_shp$tramo_ant))
for (i in falt) {
  try({
    cand <- which(tramos_ant_shp$cod_rio == tramos_act_shp$cod_rio[i] & !is.na(tramos_ant_shp$tramo_n))
    if (length(cand) > 0) {
      cen <- suppressWarnings(sf::st_centroid(sf::st_geometry(tramos_act_shp)[i]))
      d   <- as.numeric(sf::st_distance(cen, sf::st_geometry(tramos_ant_shp)[cand]))
      tramos_act_shp$tramo_ant[i] <- tramos_ant_shp$tramo_n[cand[which.min(d)]]
    }
  }, silent = TRUE)
}
tramos_act_shp$cambio_tramo <- !is.na(tramos_act_shp$tramo_ant) & !is.na(tramos_act_shp$tramo_shp) &
  tramos_act_shp$tramo_ant != tramos_act_shp$tramo_shp

# --- Estaciones (puntos) ------------------------------------------------------
leer_puntos <- function(f, corriente) {
  if (!file.exists(f)) return(NULL)
  p  <- read_sf(f) %>% st_zm(drop = TRUE, what = "ZM") %>% st_transform(4326)
  nm <- names(p)
  c_cod <- buscar_col(nm, c("codigo", "cod"))
  if (is.na(c_cod)) return(NULL)
  cod <- trimws(fix_enc(p[[c_cod]]))
  # Código para el popup: se usa tal cual viene en la columna 'codigo' del shapefile
  generico <- is.na(cod) | !nzchar(cod) | !grepl("[0-9]", cod)
  cod_popup <- cod
  message("[Puntos] ", basename(f), " | columnas: ", paste(setdiff(nm, attr(p, "sf_column")), collapse = ", "))
  if (any(generico)) message("[Puntos] ", basename(f), " | puntos con código sin número en el shapefile: ",
                             paste(unique(paste0(cod[generico], " (", trimws(fix_enc(col_o_na(p, col_prefijo(nm, "descrip"))))[generico], ")")), collapse = "; "))
  sf::st_sf(
    data.frame(
      codigo   = cod,
      codigo_popup = ifelse(is.na(cod_popup) | !nzchar(cod_popup), "Sin código", cod_popup),
      descrip  = fix_enc(col_o_na(p, col_prefijo(nm, "descrip"))),
      campana  = fix_enc(col_o_na(p, col_prefijo(nm, "camp"))),
      tipo     = fix_enc(col_o_na(p, buscar_col(nm, c("Tipo")))),
      corriente = corriente,
      stringsAsFactors = FALSE
    ),
    geometry = sf::st_geometry(p)
  )
}
puntos_est <- do.call(rbind, Filter(Negate(is.null), list(
  leer_puntos("PUNTOS_OTUN.shp", "Río Otún"),
  leer_puntos("PUNTOS_Q2Q.shp", "Quebrada Dos Quebradas")
)))
if (is.null(puntos_est) || nrow(puntos_est) == 0) stop("No se pudieron leer PUNTOS_OTUN.shp / PUNTOS_Q2Q.shp (columna 'codigo').")

puntos_est$cod_norm <- norm_txt(puntos_est$codigo)
puntos_est$cambio   <- puntos_est$cod_norm %in% norm_txt(codigos_cambio_identificados)

# Simbología de las estaciones por TIPO (topología: estaciones, tributarios, vertimientos, extracciones).
# EDITABLE: cambie color o forma (circulo, triangulo, cuadrado, rombo). "Estacion calidad" y
# "Estación calidad" (con y sin tilde) se agrupan en una sola categoría.
simbologia_puntos <- data.frame(
  tipo_norm = c("ESTACIONCALIDAD", "TRIBUTARIO", "VERTIMIENTO", "EXTRACCION"),
  etiqueta  = c("Estación de calidad", "Tributario", "Vertimiento", "Extracción"),
  color     = c("#846039", "#E0BC73", "#AB89C4", "#6B1073"),
  forma     = c("circulo", "triangulo", "cuadrado", "rombo"),
  stringsAsFactors = FALSE
)
icono_svg <- function(forma, color, tam = 20) {
  fig <- switch(forma,
    circulo   = "<circle cx='10' cy='10' r='7.5'",
    triangulo = "<polygon points='10,2 18.5,17.5 1.5,17.5'",
    cuadrado  = "<rect x='3' y='3' width='14' height='14'",
    rombo     = "<polygon points='10,1.5 18.5,10 10,18.5 1.5,10'",
    "<circle cx='10' cy='10' r='7.5'")
  svg <- paste0("<svg xmlns='http://www.w3.org/2000/svg' width='", tam, "' height='", tam,
                "' viewBox='0 0 20 20'>", fig, " fill='", color, "' stroke='#222222' stroke-width='1.6'/></svg>")
  paste0("data:image/svg+xml;charset=utf-8,", utils::URLencode(svg, reserved = TRUE))
}
k_simb <- match(norm_txt(puntos_est$tipo), simbologia_puntos$tipo_norm)
puntos_est$tipo_etq <- ifelse(is.na(k_simb), ifelse(is.na(puntos_est$tipo), "Sin tipo", puntos_est$tipo),
                              simbologia_puntos$etiqueta[k_simb])
puntos_est$icono <- vapply(seq_len(nrow(puntos_est)), function(i) {
  k <- k_simb[i]
  if (is.na(k)) icono_svg("circulo", "#9e9e9e") else icono_svg(simbologia_puntos$forma[k], simbologia_puntos$color[k])
}, character(1))
leyenda_puntos_html <- paste0(
  "<div style='background:rgba(255,255,255,0.92);padding:6px 10px;border-radius:4px;box-shadow:0 0 6px rgba(0,0,0,0.4);font:12px sans-serif;line-height:22px'>",
  "<b>Estaciones y elementos</b>",
  paste0("<div><img src=\"", mapply(icono_svg, simbologia_puntos$forma, simbologia_puntos$color, 16),
         "\" width='16' height='16' style='vertical-align:middle;margin-right:6px'>", simbologia_puntos$etiqueta, "</div>", collapse = ""),
  "</div>")

# Tipos de elementos presentes (orden de la simbología), para el filtro del mapa de tramos con vertimientos y abstracciones
choices_tipos_elem <- unique(c(simbologia_puntos$etiqueta[simbologia_puntos$etiqueta %in% puntos_est$tipo_etq],
                               sort(unique(puntos_est$tipo_etq))))

tramos_act_df <- sf::st_drop_geometry(tramos_act_shp)
tramos_ant_df <- sf::st_drop_geometry(tramos_ant_shp)
tab_puntos    <- sf::st_drop_geometry(puntos_est)
tab_puntos    <- tab_puntos[!duplicated(tab_puntos$cod_norm), ]

# Texto de una estación con toda su información (unión por código)
fmt_est <- function(cod) {
  vapply(as.character(cod), function(cc) {
    if (is.na(cc) || !nzchar(trimws(cc))) return("-")
    k <- match(norm_txt(cc), tab_puntos$cod_norm)
    if (is.na(k)) return(html_esc(cc))
    paste0("<b>", html_esc(tab_puntos$codigo[k]), "</b>", if (tab_puntos$cambio[k]) " \u26a0" else "",
           " &ndash; ", html_esc(tab_puntos$descrip[k]),
           " <i>(Campa\u00f1a: ", html_esc(tab_puntos$campana[k]), "; ", html_esc(tab_puntos$tipo[k]), ")</i>")
  }, character(1), USE.NAMES = FALSE)
}

# Relación de cada estación con los tramos (modelo actual y anterior)
puntos_est$rel_act <- vapply(puntos_est$cod_norm, function(cn) {
  a <- tramos_act_df[which(tramos_act_df$ini_n == cn), ]
  b <- tramos_act_df[which(tramos_act_df$fin_n == cn), ]
  r <- c(if (nrow(a)) paste0("Inicio de ", lbl_rio(a$cod_rio), " - Tramo ", a$tramo_shp),
         if (nrow(b)) paste0("Fin de ", lbl_rio(b$cod_rio), " - Tramo ", b$tramo_shp))
  if (length(r)) paste(unique(r), collapse = "; ") else "No delimita segmentos"
}, character(1), USE.NAMES = FALSE)
puntos_est$rel_ant <- vapply(puntos_est$cod_norm, function(cn) {
  a <- tramos_ant_df[which(tramos_ant_df$qw_n == cn), ]
  if (nrow(a)) paste(unique(paste0("Estaci\u00f3n de ", lbl_rio(a$cod_rio), " - Tramo ", a$tramo_n)), collapse = "; ")
  else "Sin registro"
}, character(1), USE.NAMES = FALSE)
puntos_est$popup <- paste0(
  "<div style='min-width:240px'><b style='font-size:14px'>", html_esc(puntos_est$codigo), "</b>",
  ifelse(puntos_est$cambio, " <span style='background:#00e5ff;padding:1px 6px;border-radius:3px;font-size:11px'>Cambio identificado</span>", ""),
  "<br><b>Descripci\u00f3n:</b> ", html_esc(puntos_est$descrip),
  "<br><b>Campa\u00f1a:</b> ", html_esc(puntos_est$campana),
  "<br><b>Tipo:</b> ", html_esc(puntos_est$tipo),
  "<br><b>Corriente:</b> ", html_esc(puntos_est$corriente),
  "<hr style='margin:5px 0'><b>Modelo actual:</b> ", html_esc(puntos_est$rel_act),
  "<br><b>Modelo anterior:</b> ", html_esc(puntos_est$rel_ant), "</div>"
)

# Etiqueta visible en el mapa de comparación de tramos: código + descripción de la estación
puntos_est$etiqueta_mapa <- vapply(seq_len(nrow(puntos_est)), function(i) {
  d <- trimws(as.character(puntos_est$descrip[i]))
  cod <- htmltools::htmlEscape(as.character(puntos_est$codigo[i]))
  if (is.na(d) || !nzchar(d)) cod else paste0(cod, " ", htmltools::htmlEscape(d))
}, character(1))

# Nombre permanente (código - descripción) para los mapas del Modelo de Calidad del Agua
puntos_est$etiqueta_nombre <- vapply(seq_len(nrow(puntos_est)), function(i) {
  d <- trimws(as.character(puntos_est$descrip[i]))
  cod <- htmltools::htmlEscape(trimws(as.character(puntos_est$codigo_popup[i])))
  if (is.na(d) || !nzchar(d)) paste0("<b>", cod, "</b>")
  else paste0("<b>", cod, "</b> - ", htmltools::htmlEscape(d))
}, character(1))
# Popup simplificado para la sección Modelo de Calidad del Agua: solo código y descripción
puntos_est$popup_simple <- paste0(
  "<div style='min-width:220px'>",
  "<b>Código de la estación:</b> ", html_esc(puntos_est$codigo_popup), "<br>",
  "<b>Descripción:</b> ", html_esc(puntos_est$descrip), "</div>")
# Etiqueta solo al pasar el cursor (los nombres fijos se retiraron por congestión visual)
opciones_lbl_estacion <- labelOptions(direction = "right", textsize = "11px")

# --- Comparativo de longitudes por tramo --------------------------------------
long_ant <- tramos_ant_df %>%
  filter(!is.na(cod_rio), !is.na(tramo_n)) %>%
  group_by(cod_rio, tramo = tramo_n) %>% summarise(km_ant = sum(long_km), .groups = "drop")
long_act <- tramos_act_df %>%
  filter(!is.na(cod_rio), !is.na(tramo_shp)) %>%
  group_by(cod_rio, tramo = tramo_shp) %>% summarise(km_act = sum(long_km2), .groups = "drop")
comp_tramos <- full_join(long_ant, long_act, by = c("cod_rio", "tramo")) %>%
  mutate(km_ant = dplyr::coalesce(km_ant, 0), km_act = dplyr::coalesce(km_act, 0),
         dif_km = km_act - km_ant,
         etiqueta = paste0(ifelse(cod_rio == "RO", "Otún", "Q2Q"), " - Tramo ", tramo)) %>%
  arrange(match(cod_rio, c("RO", "Q2Q")), tramo) %>%
  as.data.frame()

hover_tramo <- function(version, i, est) {
  est <- est[!is.na(est) & nzchar(trimws(est))]
  paste0("<b>", lbl_rio(comp_tramos$cod_rio[i]), " - Tramo ", comp_tramos$tramo[i], "</b> (", version, ")<br>",
         "Anterior: ", sprintf("%.2f", comp_tramos$km_ant[i]), " km | Actual: ", sprintf("%.2f", comp_tramos$km_act[i]), " km<br>",
         "Diferencia (actual - anterior): ", sprintf("%+.2f", comp_tramos$dif_km[i]), " km<br>",
         "<b>Estaciones del tramo:</b><br>",
         if (length(est)) paste0("&bull; ", fmt_est(est), collapse = "<br>") else "-")
}
comp_tramos$hover_ant <- vapply(seq_len(nrow(comp_tramos)), function(i) {
  d <- tramos_ant_df[which(tramos_ant_df$cod_rio == comp_tramos$cod_rio[i] & tramos_ant_df$tramo_n == comp_tramos$tramo[i]), ]
  d <- d[order(d$codrio), ]
  hover_tramo("Anterior", i, unique(d$est_ant))
}, character(1))
comp_tramos$hover_act <- vapply(seq_len(nrow(comp_tramos)), function(i) {
  d <- tramos_act_df[which(tramos_act_df$cod_rio == comp_tramos$cod_rio[i] & tramos_act_df$tramo_shp == comp_tramos$tramo[i]), ]
  d <- d[order(-d$down_shp), ]
  hover_tramo("Actual", i, unique(as.vector(rbind(d$est_ini, d$est_fin))))
}, character(1))

# --- Tabla de verificación: segmentos con diferencias -------------------------
cambio_norm <- norm_txt(codigos_cambio_identificados)
qw_ant_set  <- unique(tramos_ant_df$qw_n)
tabla_cambios_tramos <- tramos_act_df %>%
  arrange(match(cod_rio, c("RO", "Q2Q")), desc(down_shp)) %>%
  mutate(
    fin_nuevo = !is.na(fin_n) & nzchar(fin_n) & !(fin_n %in% qw_ant_set),
    marcadas  = mapply(function(a, b) {
      x <- c(a, b); x <- x[!is.na(x) & norm_txt(x) %in% cambio_norm]; paste(unique(x), collapse = ", ")
    }, est_ini, est_fin)
  ) %>%
  filter(cambio_tramo | fin_nuevo | nzchar(marcadas)) %>%
  transmute(
    `Corriente` = lbl_rio(cod_rio), `CodRio` = codrio,
    `Tramo anterior` = tramo_ant, `Tramo actual` = tramo_shp,
    `¿Cambió de tramo?` = ifelse(cambio_tramo, "Sí", "No"),
    `Longitud (km)` = round(long_km2, 2),
    `Estación inicio` = est_ini, `Estación final` = est_fin,
    `¿Estación de cierre nueva?` = ifelse(fin_nuevo, "Sí", "No"),
    `Estaciones con cambio identificado` = marcadas
  )
message("[Tramos] Segmentos con cambio de tramo: ", sum(tramos_act_shp$cambio_tramo),
        " | sin tramo anterior: ", sum(is.na(tramos_act_shp$tramo_ant)),
        " | estaciones: ", nrow(puntos_est), " (con cambio: ", sum(puntos_est$cambio), ")")



# ==============================================================================
# MODELO DE CALIDAD: MAPA DE CALIDAD DEL AGUA (METAS ACTUALES vs SIMULACIÓN QUAL2K)
# ==============================================================================
col_estado <- c("Incumple" = "#d73027", "Igual" = "#ffd400", "Cumple" = "#1a9850", "Sin meta" = "#9e9e9e")

# Equivalencias Qual2k -> parámetro de las metas (EDITABLE).
#  col_q2k: nombre de la columna en Res1-Q2K_Calibrado.xlsx
#  factor : conversión de unidades (Qual2k entrega N y P en ugN/L y ugP/L -> 0.001 para mg/L)
#  sentido: "max"   = el simulado debe ser MENOR o igual a la meta (DBO, DQO, nutrientes, SST...)
#           "min"   = el simulado debe ser MAYOR o igual a la meta (OD)
#           "rango" = debe estar entre 'pH min' y 'pH' (pH)
mapa_q2k <- data.frame(
  parametro = c("Temperatura °C", "Conductividad Electrica (us/cm)", "OD (mg/L)", "DBO (mg/L)", "DQO (mg/L)",
                "pH", "Nitrogeno Total (mgN/L)", "Amoniaco total (mg/L)", "Nitratos+Nitritos (mg/L)",
                "Fosforo Total (mg/L)", "SST (mg/L)", "Alcalinidad (mg/L CaCO3)"),
  col_q2k   = c("Temperatura °C", "Conductividad Electrica (us/cm)", "OD (mg/L)", "DBO (mg/L)", "DQO (mg/L)",
                "pH", "Nitrogeno Total (mgN/L)", "Amoniaco total (mg/L)", "Nitratos+Nitritos (mg/L)",
                "Fosforo Total (mg/L)", "SST (mg/L)", "Alcalinidad (mg/L CaCO3)"),
  # meta: parámetro de ERA_Perfiles_Metas_Nuevas_Final.xlsx con el que se compara la variable simulada
  meta      = c("Temperatura °C", "Conductividad Electrica (us/cm)", "OD (mg/L)", "DBO (mg/L)", "DQO (mg/L)",
                "pH", "Nitrogeno Total (mgN/L)", "Nitrogeno Amoniacal (mg/L)", "Nitratos (mg/L)",
                "Fosforo Total (mg/L)", "SST (mg/L)", "Alcalinidad (mg/L CaCO3)"),
  factor    = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),   # el archivo reconfigurado ya viene en mg/L
  sentido   = c("max", "max", "min", "max", "max", "rango", "max", "max", "max", "max", "max", "max"),
  stringsAsFactors = FALSE
)

# Cruce de estaciones tolerante a diferencias de escritura (SUB-191 = SUP191, Q2Q Desem = Q2Q Desemb)
norm_est2 <- function(x) {
  x <- norm_txt(x)
  x <- sub("^SUB", "SUP", x)
  sub("^Q2QDESEM$", "Q2QDESEMB", x)
}

q2k_raw <- read_excel("Res1-Q2K_Calibrado.xlsx")
c_id_q  <- buscar_col(names(q2k_raw), c("ID-PORH", "ID_PORH", "IDPORH"))
c_ini_q <- buscar_col(names(q2k_raw), c("ESTInicio"))
c_fin_q <- buscar_col(names(q2k_raw), c("ESTFinal"))
if (is.na(c_id_q)) stop("Res1-Q2K_Calibrado.xlsx debe tener la columna ID-PORH.")
q2k_raw$id_porh <- suppressWarnings(as.integer(as.character(q2k_raw[[c_id_q]])))
# pH: el archivo reconfigurado trae 'pH min' y 'pH max'; se usa su promedio como valor simulado
if (is.na(buscar_col(names(q2k_raw), "pH")) && !is.na(buscar_col(names(q2k_raw), "pH max"))) {
  .ph_a <- suppressWarnings(as.numeric(as.character(q2k_raw[[buscar_col(names(q2k_raw), "pH min")]])))
  .ph_b <- suppressWarnings(as.numeric(as.character(q2k_raw[[buscar_col(names(q2k_raw), "pH max")]])))
  q2k_raw[["pH"]] <- rowMeans(cbind(.ph_a, .ph_b), na.rm = TRUE)
  q2k_raw[["pH"]][is.nan(q2k_raw[["pH"]])] <- NA_real_
}

mapa_q2k$col_real <- vapply(mapa_q2k$col_q2k, function(cn) buscar_col(names(q2k_raw), cn), character(1))
for (cn in stats::na.omit(unique(mapa_q2k$col_real))) q2k_raw[[cn]] <- suppressWarnings(as.numeric(as.character(q2k_raw[[cn]])))

# Subtramos del shapefile actual + resultados Qual2k
tramos_q2k_shp <- tramos_act_shp
c_id_s <- buscar_col(names(tramos_q2k_shp), c("id-porh", "id_porh", "idporh"))
tramos_q2k_shp$id_porh <- if (!is.na(c_id_s)) suppressWarnings(as.integer(as.character(tramos_q2k_shp[[c_id_s]]))) else NA_integer_

ix   <- match(tramos_q2k_shp$id_porh, q2k_raw$id_porh)
miss <- is.na(ix)
if (any(miss) && !is.na(c_ini_q) && !is.na(c_fin_q)) {
  k_s <- paste(norm_est2(tramos_q2k_shp$est_ini), norm_est2(tramos_q2k_shp$est_fin))
  k_q <- paste(norm_est2(q2k_raw[[c_ini_q]]), norm_est2(q2k_raw[[c_fin_q]]))
  ix[miss] <- match(k_s[miss], k_q)
}
cols_q  <- unique(stats::na.omit(mapa_q2k$col_real))
q2k_seg <- dplyr::bind_cols(
  sf::st_drop_geometry(tramos_q2k_shp)[, c("id_porh", "llave", "cod_rio", "tramo_shp", "est_ini", "est_fin",
                                          "abscisa_ini", "abscisa_fin", "long_km2", "desc_seg")],
  q2k_raw[ix, cols_q, drop = FALSE]
)
q2k_seg$idx <- seq_len(nrow(q2k_seg))

mapa_q2k_ok <- mapa_q2k[!is.na(mapa_q2k$col_real) & mapa_q2k$meta %in% params_actual, ]
choices_param_q2k <- unique(mapa_q2k_ok$parametro)
message("[Q2K] Subtramos con resultado Qual2k: ", sum(!is.na(ix)), " de ", nrow(q2k_seg),
        " | parámetros comparables: ", length(choices_param_q2k))

# ====================================================================================================================================================
#     Situación actual: caudales mensuales (analisis_hidrologico.xlsx) y escenarios Qual2k (clima medio / clima seco) - datos
# ====================================================================================================================================================

meses_hid <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto",
               "Septiembre", "Octubre", "Noviembre", "Diciembre")

# Código SUP dentro de un texto ("Sup 104: Río Barbo", "SUP581. Quebrada San José") -> "SUP104"
codigo_sup <- function(x) {
  x <- as.character(x)
  m <- regexpr("(?i)SUP\\s*-?\\s*[0-9]+", x, perl = TRUE)
  out <- rep(NA_character_, length(x))
  out[m > 0] <- toupper(gsub("[^A-Za-z0-9]", "", regmatches(x, m)))
  out
}

# Lee una hoja (Promedio / Seca): Name, No., Headwater, km, Abstraction, Inflow, Description, Enero..Diciembre
leer_hoja_hidro <- function(archivo, hoja) {
  raw <- tryCatch(suppressMessages(read_excel(archivo, sheet = hoja, col_names = FALSE, .name_repair = "minimal")),
                  error = function(e) NULL)
  if (is.null(raw) || nrow(raw) < 3 || ncol(raw) < 19) return(NULL)
  raw <- as.data.frame(raw, stringsAsFactors = FALSE)
  i_m <- which(apply(raw, 1, function(r) any(grepl("^enero", tolower(trimws(as.character(r)))))))[1]
  if (is.na(i_m) || i_m >= nrow(raw)) return(NULL)
  d <- raw[(i_m + 1):nrow(raw), , drop = FALSE]
  d <- d[!is.na(d[[1]]) & nzchar(trimws(as.character(d[[1]]))), , drop = FALSE]
  if (nrow(d) == 0) return(NULL)
  num <- function(v) suppressWarnings(as.numeric(as.character(v)))
  list(
    meta = data.frame(
      nombre  = trimws(as.character(d[[1]])),
      codigo  = codigo_sup(d[[1]]),
      cod_rio = ifelse(grepl("^OTUN", norm_txt(d[[3]])), "RO", "Q2Q"),
      km      = num(d[[4]]),
      desc    = trimws(as.character(d[[7]])),
      stringsAsFactors = FALSE),
    m = matrix(unlist(lapply(8:19, function(j) num(d[[j]]))), ncol = 12)
  )
}

hid_archivo <- "analisis_hidrologico.xlsx"
hid_ok <- FALSE
est_hid <- NULL; est_prom <- NULL; est_seca <- NULL
hid_sin_ubicar <- character(0)
if (file.exists(hid_archivo)) {
  hojas_h <- tryCatch(excel_sheets(hid_archivo), error = function(e) character(0))
  h_prom  <- grep("prom", hojas_h, ignore.case = TRUE, value = TRUE)[1]
  h_seca  <- grep("seca", hojas_h, ignore.case = TRUE, value = TRUE)[1]
  hp <- if (!is.na(h_prom)) leer_hoja_hidro(hid_archivo, h_prom) else NULL
  hs <- if (!is.na(h_seca)) leer_hoja_hidro(hid_archivo, h_seca) else NULL
  if (!is.null(hp) && !is.null(hs)) {
    # Las dos hojas se cruzan por la descripción de la corriente (la columna Name puede traer códigos desactualizados)
    k_p <- norm_txt(hp$meta$desc); k_s <- norm_txt(hs$meta$desc)
    llaves <- unique(c(k_p, k_s))
    i_p <- match(llaves, k_p); i_s <- match(llaves, k_s)
    est_hid <- data.frame(
      desc    = dplyr::coalesce(hs$meta$desc[i_s], hp$meta$desc[i_p]),
      nombre  = dplyr::coalesce(hs$meta$nombre[i_s], hp$meta$nombre[i_p]),
      codigo  = dplyr::coalesce(hs$meta$codigo[i_s], hp$meta$codigo[i_p]),
      cod_rio = dplyr::coalesce(hs$meta$cod_rio[i_s], hp$meta$cod_rio[i_p]),
      km      = dplyr::coalesce(hs$meta$km[i_s], hp$meta$km[i_p]),
      stringsAsFactors = FALSE)
    dif_cod <- which(!is.na(i_p) & !is.na(i_s) & !is.na(hp$meta$codigo[i_p]) & !is.na(hs$meta$codigo[i_s]) &
                       hp$meta$codigo[i_p] != hs$meta$codigo[i_s])
    for (j in dif_cod) message("[Hidrología] '", est_hid$desc[j], "': código distinto entre hojas (Promedio ",
                               hp$meta$codigo[i_p[j]], " / Seca ", hs$meta$codigo[i_s[j]], "). Se usa el de la hoja Seca.")
    est_prom <- hp$m[i_p, , drop = FALSE]
    est_seca <- hs$m[i_s, , drop = FALSE]

    # Ubicación (PUNTOS_OTUN / PUNTOS_Q2Q por código) y tramo (segmento más cercano de la misma corriente)
    est_hid$cod_norm <- norm_txt(est_hid$codigo)
    kp <- match(est_hid$cod_norm, puntos_est$cod_norm)
    cxy <- sf::st_coordinates(puntos_est)
    est_hid$lon <- cxy[kp, 1]; est_hid$lat <- cxy[kp, 2]
    est_hid$tramo <- NA_integer_
    for (rr in unique(est_hid$cod_rio)) {
      ii   <- which(est_hid$cod_rio == rr & !is.na(est_hid$lon))
      segs <- tramos_act_shp[!is.na(tramos_act_shp$cod_rio) & tramos_act_shp$cod_rio == rr & !is.na(tramos_act_shp$tramo_shp), ]
      if (!length(ii) || !nrow(segs)) next
      pts <- sf::st_as_sf(est_hid[ii, c("lon", "lat")], coords = c("lon", "lat"), crs = 4326)
      nn  <- sf::st_nearest_feature(pts, segs)
      est_hid$tramo[ii] <- segs$tramo_shp[nn]
    }
    hid_sin_ubicar <- est_hid$codigo[is.na(est_hid$tramo)]
    hid_ok <- any(!is.na(est_hid$tramo))
    message("[Hidrología] Estaciones: ", nrow(est_hid), " | ubicadas en un tramo: ", sum(!is.na(est_hid$tramo)),
            if (length(hid_sin_ubicar)) paste0(" | sin ubicación: ", paste(hid_sin_ubicar, collapse = ", ")) else "")
  }
} else {
  message("[Hidrología] No se encontró '", hid_archivo, "' en la carpeta de la app.")
}
# --- Drenajes (Drenajes_otun_q2q.shp): NOMBRE_GEO se muestra en el popup de las estaciones de caudal ----------------
drenajes_shp <- NULL
f_dren <- list.files(".", pattern = "^Drenajes_otun_q2q\\.shp$", ignore.case = TRUE)
if (length(f_dren)) {
  drenajes_shp <- tryCatch({
    x <- read_sf(f_dren[1]) %>% st_zm(drop = TRUE, what = "ZM") %>% st_transform(4326)
    c_nom <- buscar_col(names(x), c("NOMBRE_GEO", "NOMBREGEO"))
    nm <- if (is.na(c_nom)) rep(NA_character_, nrow(x)) else fix_enc(trimws(as.character(x[[c_nom]])))
    nm[!is.na(nm) & !nzchar(nm)] <- NA_character_
    x$nombre_geo <- nm
    x[, "nombre_geo"]
  }, error = function(e) { message("[Drenajes] No se pudo leer: ", conditionMessage(e)); NULL })
  if (!is.null(drenajes_shp)) message("[Drenajes] ", nrow(drenajes_shp), " drenajes, ", sum(!is.na(drenajes_shp$nombre_geo)), " con NOMBRE_GEO")
} else {
  message("[Drenajes] No se encontró 'Drenajes_otun_q2q.shp' en la carpeta de la app.")
}
if (hid_ok) {
  est_hid$drenaje <- NA_character_; est_hid$drenaje_m <- NA_real_
  if (!is.null(drenajes_shp)) {
    try({
      dn <- drenajes_shp[!is.na(drenajes_shp$nombre_geo), ]
      ii <- which(!is.na(est_hid$lon))
      if (nrow(dn) && length(ii)) {
        pts <- sf::st_as_sf(est_hid[ii, c("lon", "lat")], coords = c("lon", "lat"), crs = 4326)
        crs_m <- tryCatch({ sf::st_transform(pts, 9377); 9377 }, error = function(e) 3116)
        pts_m <- sf::st_transform(pts, crs_m); dn_m <- sf::st_transform(dn, crs_m)
        nn <- sf::st_nearest_feature(pts_m, dn_m)
        est_hid$drenaje[ii]   <- dn$nombre_geo[nn]
        est_hid$drenaje_m[ii] <- as.numeric(sf::st_distance(pts_m, dn_m[nn, ], by_element = TRUE))
      }
    }, silent = TRUE)
  }
}

# Capa "Quebradas" para los mapas del Modelo de Calidad del Agua (debajo de tramos y estaciones)
agregar_quebradas <- function(m, pane) {
  if (is.null(drenajes_shp) || !nrow(drenajes_shp)) return(m)
  nom <- ifelse(is.na(drenajes_shp$nombre_geo), "Quebrada sin nombre", drenajes_shp$nombre_geo)
  m %>%
    addMapPane(pane, zIndex = 400) %>%
    addPolylines(data = drenajes_shp, group = "Quebradas", color = "#4a90c2", weight = 1.3, opacity = 0.7,
                 label = nom,
                 popup = paste0("<b>Quebrada:</b> ", htmltools::htmlEscape(nom)),
                 highlightOptions = highlightOptions(weight = 3.5, color = "#0b5394", bringToFront = FALSE),
                 options = pathOptions(pane = pane))
}
grupos_quebradas <- if (is.null(drenajes_shp)) character(0) else "Quebradas"

choices_rio_hid <- if (hid_ok) rios_meta[rios_meta %in% est_hid$cod_rio[!is.na(est_hid$tramo)]] else rios_meta

# --- Escenarios Qual2k: clima medio (promedio) y clima seco (bajo) -------------------------------------------
leer_escenario_q2k <- function(patron) {
  f <- list.files(".", pattern = patron, ignore.case = TRUE)
  f <- f[!grepl("^~\\$", f)]
  if (!length(f)) return(NULL)
  x <- tryCatch(suppressMessages(read_excel(f[1], .name_repair = "minimal")), error = function(e) NULL)
  if (is.null(x)) return(NULL)
  cid <- buscar_col(names(x), c("ID-PORH", "ID_PORH", "IDPORH"))
  if (is.na(cid)) return(NULL)
  x <- as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)
  x$id_porh <- suppressWarnings(as.integer(as.character(x[[cid]])))
  message("[Escenarios Q2K] ", f[1], ": ", nrow(x), " subtramos")
  x
}
esc_q2k <- list(
  medio = leer_escenario_q2k("^Escenario_actual.*medio.*\\.xlsx$"),
  seco  = leer_escenario_q2k("^Escenario_actual.*seco.*\\.xlsx$")
)
esc_ok <- !is.null(esc_q2k$medio) && !is.null(esc_q2k$seco)

# Índice de cada subtramo del shapefile en la tabla del escenario (ID-PORH; si falta, por estaciones inicio/fin)
indice_escenario <- function(x) {
  ix <- match(q2k_seg$id_porh, x$id_porh)
  miss <- is.na(ix)
  ci <- buscar_col(names(x), c("ESTInicio")); cf <- buscar_col(names(x), c("ESTFinal", "ESTfinal"))
  if (any(miss) && !is.na(ci) && !is.na(cf)) {
    k_s <- paste(norm_est2(q2k_seg$est_ini), norm_est2(q2k_seg$est_fin))
    k_q <- paste(norm_est2(x[[ci]]), norm_est2(x[[cf]]))
    ix[miss] <- match(k_s[miss], k_q)
  }
  ix
}
ix_esc <- if (esc_ok) lapply(esc_q2k, indice_escenario) else NULL

# Equivalencias columna del escenario -> parámetro de las metas (EDITABLE). Los escenarios ya vienen en mg/L.
#  meta   : nombre del parámetro en ERA_Perfiles_Metas_Nuevas_Final.xlsx (NA = sin meta, solo se grafica)
#  sentido: "max" simulado <= meta | "min" simulado >= meta | "rango" entre 'pH min' y 'pH' | "ninguno"
mapa_esc <- data.frame(
  etiqueta = c("Caudal (m3/s)", "Temperatura °C", "Conductividad Electrica (us/cm)", "SST (mg/L)", "OD (mg/L)",
               "DBO (mg/L)", "DQO (mg/L)", "pH", "Nitrogeno Amoniacal (mg/L)", "Nitratos (mg/L)",
               "Nitrogeno Total (mgN/L)", "Fosforo Total (mg/L)", "Alcalinidad (mg/L CaCO3)"),
  col_esc  = c("Caudal (m3/s)", "Temperatura °C", "Conductividad Electrica (us/cm)", "SST (mg/L)", "OD (mg/L)",
               "DBO (mg/L)", "DQO (mg/L)", "pH", "Amoniaco total (mg/L)", "Nitratos+Nitritos (mg/L)",
               "Nitrogeno Total (mgN/L)", "Fosforo Total (mg/L)", "Alcalinidad (mg/L CaCO3)"),
  meta     = c(NA, "Temperatura °C", "Conductividad Electrica (us/cm)", "SST (mg/L)", "OD (mg/L)",
               "DBO (mg/L)", "DQO (mg/L)", "pH", "Nitrogeno Amoniacal (mg/L)", "Nitratos (mg/L)",
               "Nitrogeno Total (mgN/L)", "Fosforo Total (mg/L)", "Alcalinidad (mg/L CaCO3)"),
  sentido  = c("ninguno", "max", "max", "max", "min", "max", "max", "rango", "max", "max", "max", "max", "max"),
  stringsAsFactors = FALSE
)
if (esc_ok) {
  disp <- vapply(seq_len(nrow(mapa_esc)), function(i) {
    if (mapa_esc$etiqueta[i] == "pH") !is.na(buscar_col(names(esc_q2k$medio), "pH max")) && !is.na(buscar_col(names(esc_q2k$seco), "pH max"))
    else !is.na(buscar_col(names(esc_q2k$medio), mapa_esc$col_esc[i])) && !is.na(buscar_col(names(esc_q2k$seco), mapa_esc$col_esc[i]))
  }, logical(1))
  mapa_esc <- mapa_esc[disp, , drop = FALSE]
}

valor_esc <- function(x, ix, etiqueta, col_esc) {
  num <- function(v) suppressWarnings(as.numeric(as.character(v)))
  if (identical(etiqueta, "pH")) {
    a <- num(x[[buscar_col(names(x), "pH min")]]); b <- num(x[[buscar_col(names(x), "pH max")]])
    v <- rowMeans(cbind(a, b), na.rm = TRUE); v[is.nan(v)] <- NA_real_
  } else {
    v <- num(x[[buscar_col(names(x), col_esc)]])
  }
  v[ix]
}

estado_vec <- function(sim, lo, hi, tol) {
  below <- !is.na(lo) & sim < lo - tol * abs(lo)
  above <- !is.na(hi) & sim > hi + tol * abs(hi)
  edge  <- (!is.na(lo) & abs(sim - lo) <= tol * abs(lo)) | (!is.na(hi) & abs(sim - hi) <= tol * abs(hi))
  tiene <- !is.na(sim) & (!is.na(lo) | !is.na(hi))
  ifelse(!tiene, "Sin meta", ifelse(below | above, "Incumple", ifelse(edge, "Igual", "Cumple")))
}
choices_var_esc <- if (esc_ok) mapa_esc$etiqueta else "Caudal (m3/s)"

# --- Diagrama y topología del modelo (HTML de draw.io embebido) ---------------------------------------------
html_topologia <- local({
  f <- c("Top_calidad_ROtun.html", "Top_calidad_R_Otun.html", "www/Top_calidad_ROtun.html")
  f <- f[file.exists(f)]
  if (!length(f)) {
    message("[Topología] No se encontró 'Top_calidad_ROtun.html' en la carpeta de la app.")
    NULL
  } else paste(readLines(f[1], warn = FALSE, encoding = "UTF-8"), collapse = "\n")
})

# ====================================================================================================================================================
#                                      Diagrama de Sol (Componente Programático) - datos
# ====================================================================================================================================================

preparar_sunburst <- function(archivo = "Dsol_PORH2_colores.csv", sep_lin = 20) {
  if (!file.exists(archivo)) return(NULL)
  dsb <- read.csv(archivo, fileEncoding = "UTF-8", stringsAsFactors = FALSE, check.names = FALSE)
  rownames(dsb) <- NULL
  # Quiebre de líneas en las etiquetas
  for (i in seq_len(nrow(dsb))) {
    t   <- as.vector(strsplit(dsb$labels[i], " ")[[1]])
    t_l <- cumsum(nchar(t))
    lim <- seq_along(t) * sep_lin
    text <- t[1]
    j_t_l <- 1
    if (length(t_l) > 1) {
      for (j in 2:length(t_l)) {
        if (j == length(t_l) && t_l[length(t_l)] >= lim[j_t_l]) {
          text <- paste0(text, " ", t[j])
        } else if (t_l[j] > lim[j_t_l]) {
          text <- paste0(text, " ", t[j], "<br>")
          j_t_l <- j_t_l + 1
        } else {
          text <- paste0(text, " ", t[j])
        }
      }
    }
    dsb$labels[i] <- text
  }
  na.exclude(dsb)
}
dataSBcontprog <- preparar_sunburst()
if (is.null(dataSBcontprog)) message("[Sunburst] No se encontró 'Dsol_PORH2_colores.csv' en la carpeta de la app.")

# ============================================================================================================================================================
#                                                          Interfaz de Usuario (UI)
# ============================================================================================================================================================

ui <- page_sidebar(
  theme = bs_theme(version = 5, bootswatch = "darkly"),
  window_title = "PORH-Tool",
  
  tags$head(
    tags$style(HTML("
      /* Encabezado siempre blanco, en cualquier tema */
      .navbar:has(.porh-header), .bslib-page-title:has(.porh-header), header:has(.porh-header),
      .porh-header {
        background-color: #ffffff !important;
        color: #212529 !important;
        --bs-navbar-color: #212529; --bs-navbar-brand-color: #212529;
      }
      .navbar:has(.porh-header), .bslib-page-title:has(.porh-header), header:has(.porh-header) {
        border-bottom: 1px solid #dee2e6 !important;
        box-shadow: 0 1px 3px rgba(0,0,0,.08);
      }
      .porh-header, .porh-header * { color: #212529; }
      .porh-header .selectize-input, .porh-header .selectize-input input { color: #212529 !important; }
      .logo-chip { display:flex; align-items:center; gap:14px; margin-left:18px; }
      .logo-chip img { height:56px; width:auto; display:block; }
      .logo-chip .logo-sep { width:1px; height:40px; background:#cfcfcf; }
      @media (max-width: 768px) { .logo-chip img { height:38px; } .logo-chip { gap:8px; margin-left:8px; } }
      /* Nombres permanentes de estaciones en los mapas del Modelo de Calidad */
      .lbl-estacion {
        font: 600 10px/1.15 system-ui, -apple-system, sans-serif !important;
        color: #1b1b1b !important; background: transparent !important; border: none !important;
        box-shadow: none !important; white-space: nowrap !important;
        text-shadow: -1px -1px 0 #fff, 1px -1px 0 #fff, -1px 1px 0 #fff, 1px 1px 0 #fff, 0 0 3px #fff;
      }
      .lbl-estacion b { font-weight: 800; }
    ")),
    tags$style(HTML("
      html, body, 
      .bslib-page-sidebar,
      .bslib-sidebar-layout,
      .bslib-sidebar-layout > .main,
      .bslib-sidebar-layout .sidebar-content,
      .main-content,
      .sidebar {
        scrollbar-width: none !important;
        -ms-overflow-style: none !important;
      }

      ::-webkit-scrollbar,
      html::-webkit-scrollbar,
      body::-webkit-scrollbar,
      .bslib-sidebar-layout::-webkit-scrollbar,
      .bslib-sidebar-layout > .main::-webkit-scrollbar,
      .bslib-sidebar-layout .sidebar-content::-webkit-scrollbar,
      .main-content::-webkit-scrollbar,
      .sidebar::-webkit-scrollbar {
        display: none !important;
        width: 0px !important;
        height: 0px !important;
        background: transparent !important;
      }

      .modal-fullscreen-custom .modal-dialog {
        max-width: 100vw !important;
        max-height: 100vh !important;
        margin: 0 !important;
      }
      .modal-fullscreen-custom .modal-content {
        height: 100vh !important;
        border-radius: 0 !important;
        border: none !important;
      }
      .nav-btn-link {
        display: block;
        width: 100%;
        text-align: left;
        background: none;
        border: none;
        padding: 8px 12px;
        color: inherit;
        border-radius: 6px;
        transition: background 0.2s;
      }
      .nav-btn-link:hover {
        background-color: rgba(255, 255, 255, 0.1);
      }
      
      .dataTable, .dataTables_wrapper {
        font-size: 0.78rem !important;
        color: #e0e0e0;
      }
      
      .dataTable th, .dataTable td {
        padding: 4px 8px !important;
        vertical-align: middle !important;
      }
      
      .dataTables_length, .dataTables_filter, 
      .dataTables_info, .dataTables_paginate {
        font-size: 0.75rem !important;
      }
      
      .dataTables_paginate .paginate_button {
        padding: 2px 8px !important;
        font-size: 0.75rem !important;
      }

      .dataTables_wrapper .dataTables_length {
        float: left !important;
        margin-bottom: 0.8rem !important;
        display: flex !important;
        align-items: center !important;
      }

      .dataTables_wrapper .dataTables_length select {
        margin: 0 0.4rem !important;
        padding: 0.2rem 1.6rem 0.2rem 0.6rem !important;
        font-size: 0.8rem !important;
        height: auto !important;
        min-width: 70px !important;
        width: auto !important;
      }

      .dataTables_wrapper .dt-buttons {
        float: right !important;
        margin-bottom: 0.8rem !important;
        margin-left: 0.8rem !important;
      }

      .dataTables_wrapper .dt-buttons .dt-button,
      .dataTables_wrapper .dt-buttons .btn {
        margin-left: 0.25rem !important;
        font-size: 0.75rem !important;
        padding: 0.2rem 0.5rem !important;
        border-radius: 4px !important;
      }

      .dataTables_wrapper .dataTables_filter {
        float: right !important;
        margin-bottom: 0.8rem !important;
      }

      .dataTables_wrapper .dataTables_filter input {
        margin-left: 0.5rem !important;
        padding: 0.2rem 0.5rem !important;
        font-size: 0.8rem !important;
        border-radius: 4px !important;
      }

      .dataTables_wrapper::after {
        content: '';
        display: table;
        clear: both;
      }
    "))
  ),  
  
  title = div(
    class = "porh-header d-flex justify-content-between align-items-center w-100 pe-1",
    style = "font-family: 'Georgia', serif; font-weight: bold; font-size: 20px;",
    div(
      class = "d-flex align-items-center",
      span("PORH-Tool"),
      div(class = "logo-chip",
          tags$img(src = "logo_sei.png",    alt = "Stockholm Environment Institute"),
          div(class = "logo-sep"),
          tags$img(src = "logo_carder.png", alt = "CARDER - Corporaci\u00f3n Aut\u00f3noma Regional de Risaralda"))
    ),
    
    div(
      class = "d-flex align-items-center gap-3 fs-6 font-sans-serif",
      style = "font-family: system-ui, -apple-system, sans-serif; font-weight: normal;",
      
      selectInput(
        inputId = "theme_choice",
        label = NULL,
        choices = c(
          "Default"   = "default",
          "Cyborg"    = "cyborg",
          "Darkly"    = "darkly",
          "Flatly"    = "flatly",
          "Lumen"     = "lumen",
          "Slate"     = "slate",
          "Superhero" = "superhero",
          "Yeti"      = "yeti"
        ),
        selected = "default",
        width = "120px"
      )
    )
  ),
  
  # ==============================================================================
  # Sidebar (Panel izquierdo de navegación)
  # ==============================================================================
  
  
  sidebar = sidebar(
    title = "Panel de Navegación",
    width = 350,
    div(
      class = "d-flex flex-column gap-1 mb-3",
      
      actionButton("btn_nav_objetivos_calidad_agua", label = tagList(icon("bullseye"), "Objetivos de Calidad del Agua"), class = "nav-btn-link text-start"),
      actionButton("btn_nav_modelo_calidad_agua", label = tagList(icon("diagram-project"), "Modelo de Calidad del Agua"), class = "nav-btn-link text-start"),
      actionButton("btn_nav_situacion_actual", label = tagList(icon("droplet"), "Situación Actual"), class = "nav-btn-link text-start"),
      actionButton("btn_nav_simulacion_escenarios", label = tagList(icon("sliders"), "Simulación de Escenarios"), class = "nav-btn-link text-start"),
      actionButton("btn_nav_componente_programatico", label = tagList(icon("sun"), "Contenido Programático"), class = "nav-btn-link text-start")
    ),
    hr(),
    div(
      class = "d-flex flex-column gap-1 mb-3",
      
      actionButton("btn_nav_estaciones", label = tagList(icon("location-dot"), " Información de las estaciones"), class = "nav-btn-link text-start"),
      # Oculto por solicitud (no se borra): quitar el div con display:none para volver a mostrarlo
      div(style = "display:none;",
          actionButton("btn_nav_perfiles", label = tagList(icon("chart-line"), " Perfiles de calidad"), class = "nav-btn-link text-start"))
    ),
    
    hr(),
    div(
      class = "d-flex flex-column gap-1",
      actionButton("btn_nav_intro", label = tagList(icon("book-open"), " Introducción"), class = "nav-btn-link"),
      actionButton("btn_nav_equipo", label = tagList(icon("users"), " Equipo de trabajo"), class = "nav-btn-link"),
      actionButton("btn_nav_creditos", label = tagList(icon("award"), " Créditos y agradecimientos"), class = "nav-btn-link"),
      actionButton("btn_nav_contacto", label = tagList(icon("headset"), " Contacto y soporte"), class = "nav-btn-link")
    )
  ),
  
  # ==============================================================================
  # Sidebar (Panel derecho de consultas)
  # ==============================================================================
  
  layout_sidebar(
    fillable = TRUE,
    sidebar = sidebar(
      title = "Panel de Consultas",
      position = "right",
      width = 350,
      open = "desktop",
      
      div(
        class = "p-2",
        fileInput(
          inputId = "cargar_zip",
          label = "Subir datos GIS:",
          accept = c(".zip", "application/zip", "application/x-zip-compressed"),
          buttonLabel = icon("upload"),
          placeholder = "Esperando importación"
        ),
        p("Importa un archivo comprimido (.zip) de tipo Shapefile, GeoJSON o KML.", class = "small text-secondary"),
        
        dateRangeInput(
          inputId = "FechaICA",
          label = "Seleccione la Fecha a visualizar en el mapa:",
          start = min(ICA_datos1$Fecha, na.rm = TRUE),
          end = max(ICA_datos1$Fecha, na.rm = TRUE),
          language = "es"
        ),
        
        selectInput(
          inputId = "EstacionICA",
          label = "Seleccione la estación",
          choices = c("Todas", unique(as.character(ICA_datos1$Estacion))),
          selected = "Todas"
        ),
        
        # Oculto: el índice ahora lo define la pestaña activa del mapa (ICA-5 [IDEAM] / ICA-6 [CARDER])
        div(style = "display:none;",
          selectInput(
            inputId = "TipoICA",
            label = "Seleccione el Índice ICA:",
            choices = c("Ambos", "ICA-5 [IDEAM]", "ICA-6 [CARDER]"),
            selected = "Ambos"
          )
        ),
        
        sliderInput(
          inputId = "radioICA",
          label = "Fije el radio de los puntos ICA en el mapa",
          min = 0,
          max = 2000,
          value = 100,
          step = 50
        ),
        
        # Controles de "Perfiles de calidad": ocultos junto con esa sección (no se borran)
        div(style = "display:none;",
          hr(),
          selectInput(
            inputId = "RioPerfil",
            label = "Seleccione el Rio",
            choices = sort(unique(Perfilesshp$NOMBRE)),
            selected = sort(unique(Perfilesshp$NOMBRE))[6]
          ),
          sliderInput(
            inputId = "radioPerfil",
            label = "Radio de puntos de perfiles:",
            min = 50,
            max = 1000,
            value = 300,
            step = 50
          )
        )
      )
    ),
    
    
    # ==============================================================================
    # UI- Sección de información de las estaciones 
    # ==============================================================================
    
    
    navset_hidden(
      id = "secciones_main",

    # ==============================================================================
    # UI- Sección de Objetivos de Calidad del Agua
    # ==============================================================================
    
    nav_panel_hidden(
      value = "sec_objetivos_calidad_agua",
      div(
        h3("Objetivos de Calidad del Agua"),
        hr(),
        br()
      ),
      
      card(
        full_screen = TRUE,
        class = "shadow-sm border-0 mb-4",
        card_header(
          class = "d-flex align-items-center justify-content-between",
          div(icon("map-location-dot"), "Objetivos de Calidad del Agua"),
          tags$button(
            class = "btn btn-sm p-0 text-reset border-0 opacity-75 opacity-100-hover me-4",
            type = "button",
            `data-bs-toggle` = "collapse",
            `data-bs-target` = "#collapse_objetivos_calidad_agua",
            `aria-expanded` = "true",
            icon("minus")
          )
        ),
        div(
          id = "collapse_objetivos_calidad_agua",
          class = "collapse show",
          card_body(
            padding = 0,
            navset_card_tab(
              id = "tabs_objetivos",
              nav_panel(
                title = "Identificación de usos del agua por tramo de río",
                div(
                  class = "p-0",
                  card(
                    full_screen = TRUE,
                    class = "border-0 mb-0",
                    card_body(class = "p-0", uiOutput("powerbi_usos"))
                  )
                )
              ),
              nav_panel(
                title = "Comparación de tramos y de usos del agua",
                  div(
                    class = "p-3",
                    div(
                      class = "row g-3 align-items-end mb-2",
                      div(class = "col-md-3",
                          selectInput("sel_rio_tramos", "Corriente:", choices = c("Ambas" = "ALL", rios_meta), selected = "ALL")),
                      div(class = "col-md-3", checkboxInput("chk_puntos_tramos", "Mostrar puntos (estaciones)", value = TRUE)),
                      div(class = "col-md-3", checkboxInput("chk_solo_cambio_tramos", "Solo puntos con cambio", value = FALSE)),
                      div(class = "col-md-3", checkboxInput("chk_resaltar_tramos", "Resaltar segmentos con cambio de tramo", value = TRUE))
                    ),
                    leafletOutput("mapa_analisis_tramos", height = "560px"),
                    tags$p(class = "small text-secondary mt-2 mb-3",
                           "Arrastre la barra central para comparar: a la izquierda los tramos anteriores y a la derecha los actuales. ",
                           "Haga clic en un tramo o en una estación para ver su información."),
                    hr(),
                    h5("Comparación de longitudes por tramo"),
                    plotlyOutput("grafico_tramos_distancia", height = "400px"),
                    tags$p(class = "small text-secondary",
                           "Pase el cursor sobre una barra para ver las estaciones del tramo; haga clic para ubicarlo en el mapa."),
                    hr(),
                    h5("Segmentos con diferencias entre versiones"),
                    DT::DTOutput("tabla_cambios_tramos")
                  )
              ),              
              nav_panel(
                title = "Perfil comparativo de metas por tramo",
                div(
                  class = "p-3",
            tags$head(
              tags$style(HTML("
          .selectize-dropdown-content {
            max-height: 200px !important;
            overflow-y: auto !important;
          }
          .selectize-input {
            font-size: 11px !important;
            min-height: 31px !important;
            padding: 3px 6px !important;
            max-height: 75px !important;
            overflow-y: auto !important;
          }
          .selectize-control.multi .selectize-input > div {
            padding: 1px 4px !important;
            margin: 2px 2px !important;
            font-size: 10.5px !important;
            border-radius: 3px !important;
          }
          .selectize-dropdown .option {
            font-size: 11px !important;
            padding: 4px 8px !important;
          }
        "))
            ),
              card(
              full_screen = TRUE,
              class = "shadow-sm border-0 mb-4",
              card_header(
                class = "d-flex align-items-center justify-content-between",
                div(icon("bullseye"), " Mapa de Metas de Calidad de Agua"),
                tags$button(
                  class = "btn btn-sm p-0 text-reset border-0 opacity-75 opacity-100-hover me-4",
                  type = "button",
                  `data-bs-toggle` = "collapse",
                  `data-bs-target` = "#collapse_mapa_metas_body",
                  `aria-expanded` = "true",
                  icon("minus")
                )
              ),
              div(
                id = "collapse_mapa_metas_body",
                class = "collapse show",
                div(
                  class = "bg-light border-bottom py-2 px-3 d-flex align-items-center gap-3 text-black",
                  span(class = "fw-bold", "Seleccione una opción:"),
                  div(
                    style = "margin-bottom: -14px;",
                    radioButtons(
                      inputId = "sel_version_metas",
                      label = NULL,
                      choices = c("Metas actuales" = "actual",
                                  "Metas anteriores" = "anterior",
                                  "Diferencia (actual - anterior)" = "diferencia"),
                      selected = "actual",
                      inline = TRUE
                    )
                  )
                ),
                
                card_body(
                  padding = 0,
                  leafletOutput("mapa_metas_agua", height = "600px")
                ),
                
                card_footer(
                  class = "bg-light pt-2 pb-5 px-3",
                  div(
                    class = "d-flex align-items-center justify-content-between flex-wrap gap-2",
                    
                    div(
                      class = "d-flex align-items-center gap-2 w-100 justify-content-between",
                      
                      div(
                        class = "d-flex align-items-center gap-2",
                        span(class = "text-black", "Parámetro:"),
                        span(
                          id = "lbl_parametro_activo_metas",
                          class = "text-black fs-8 px-2 py-2",
                          textOutput("txt_parametro_activo_metas", inline = FALSE)
                        )
                      ),
   
                      div(
                        class = "btn-group shadow-sm me-2",
                        role = "group",
                        `aria-label` = "Controles de animación de parámetros metas",
                        actionButton(
                          inputId = "btn_prev_param_metas",
                          label = NULL,
                          icon = icon("step-backward"),
                          class = "btn btn-outline-secondary btn-sm",
                          title = "Parámetro anterior"
                        ),
                        actionButton(
                          inputId = "btn_play_param_metas",
                          label = " Play",
                          icon = icon("play"),
                          class = "btn btn-success btn-sm fw-bold",
                          title = "Iniciar/Pausar recorrido automático de parámetros"
                        ),
                        actionButton(
                          inputId = "btn_next_param_metas",
                          label = NULL,
                          icon = icon("step-forward"),
                          class = "btn btn-outline-secondary btn-sm",
                          title = "Parámetro siguiente"
                        )
                      ),
  
                      selectInput(
                        inputId = "sel_parametro_manual_metas",
                        label = NULL,
                        choices = NULL,
                        width = "280px"
                      ),
  
                      selectInput(
                        inputId = "sel_meta_horizonte_metas",
                        label = NULL,
                        choices = c(
                          "Corto Plazo (5 años)" = "Corto Plazo (5 años)",
                          "Mediano Plazo (10 años)" = "Mediano Plazo (10 años)",
                          "Largo Plazo (20 años)" = "Largo Plazo (20 años)"
                        ),
                        selected = "Corto Plazo (5 años)",
                        width = "220px"
                      )
                    )
                  )
                )
              )
            ),
            
            card(
              full_screen = TRUE,
              class = "shadow-sm border-0 mb-4",
              card_header(
                class = "d-flex align-items-center justify-content-between",
                div(icon("chart-line"), " Comparativo de Metas: Anteriores vs Actuales (Perfil por Parámetro)"),
                tags$button(
                  class = "btn btn-sm p-0 text-reset border-0 opacity-75 opacity-100-hover me-4",
                  type = "button",
                  `data-bs-toggle` = "collapse",
                  `data-bs-target` = "#collapse_grafico_metas_body",
                  `aria-expanded` = "true",
                  icon("minus")
                )
              ),
              div(
                id = "collapse_grafico_metas_body",
                class = "collapse show",
                card_body(
                  div(
                    class = "row g-3 align-items-end mb-2",
                    div(class = "col-md-2",
                        selectInput("sel_rio_grafico_metas", "Corriente:", choices = rios_meta, selected = "RO")),
                    div(class = "col-md-4",
                        checkboxGroupInput("chk_horizontes_metas", "Horizontes:",
                                           choices = horizontes_metas, selected = horizontes_metas, inline = TRUE)),
                    div(class = "col-md-2",
                        checkboxGroupInput("chk_series_metas", "Metas:",
                                           choices = c("Actuales" = "actual", "Anteriores" = "anterior"),
                                           selected = c("actual", "anterior"), inline = TRUE)),
                    div(class = "col-md-2",
                        checkboxInput("chk_hist_metas", "Mostrar datos históricos", value = TRUE)),
                    div(class = "col-md-2",
                        checkboxInput("chk_log_metas", "Eje Y logarítmico", value = FALSE))
                  ),
                  plotlyOutput("grafico_metas_tramos", height = "500px")
                )
              )
            )
                )
              )
            )
          )
        )
      )
    ),
    
    # ==============================================================================
    # UI- Sección de Modelo de Calidad del Agua
    # ==============================================================================
    

    nav_panel_hidden(
      value = "sec_modelo_calidad_agua",
      div(
        h3("Modelo de Calidad del Agua"),
        hr(),
        br()
      ),
      
      card(
        full_screen = TRUE,
        class = "shadow-sm border-0 mb-4",
        card_header(
          class = "d-flex align-items-center justify-content-between",
          div(icon("map-location-dot"), "Modelo de Calidad del Agua"),
          tags$button(
            class = "btn btn-sm p-0 text-reset border-0 opacity-75 opacity-100-hover me-4",
            type = "button",
            `data-bs-toggle` = "collapse",
            `data-bs-target` = "#collapse_modelo_calidad_agua",
            `aria-expanded` = "true",
            icon("minus")
          )
        ),
        div(
          id = "collapse_modelo_calidad_agua",
          class = "collapse show",
          card_body(
            padding = 0,
            navset_card_tab(
              id = "tabs_modelo",
              nav_panel(
                title = "Mapa de tramos con vertimientos y abstracciones",
                div(
                  class = "p-3",
                  div(
                    class = "row g-3 align-items-end mb-2",
                    div(class = "col-md-2",
                        selectInput("sel_corr_elem", "Corriente:", choices = c("Ambas" = "ALL", rios_meta), selected = "ALL")),
                    div(class = "col-md-3",
                        selectInput("sel_simb_tramos", "Simbología de tramos según:",
                                    choices = c("Tramo" = "tramo", "Río" = "rio"), selected = "tramo")),
                    div(class = "col-md-3",
                        selectInput("sel_simb_puntos", "Simbología de estaciones y elementos según:",
                                    choices = c("Tipo" = "tipo", "Campaña" = "campana"), selected = "tipo")),
                    div(class = "col-md-4",
                        checkboxGroupInput("chk_tipos_elem", "Elementos a mostrar:",
                                           choices = choices_tipos_elem, selected = choices_tipos_elem, inline = TRUE))
                  ),
                  leafletOutput("mapa_tramos_elementos", height = "600px"),
                  tags$p(class = "small text-secondary mt-2 mb-0",
                         "Cambie la columna con la que se colorean los tramos (tramo, río) y las estaciones y elementos (tipo, campaña); ",
                         "las leyendas se actualizan automáticamente. Haga clic en un tramo o en un elemento para ver su información. ",
                         "Use el control de capas (arriba a la derecha) para ocultar tramos o elementos y cambiar el mapa base.")
                )
              ),
              nav_panel(
                title = "Diagrama y topología del modelo",
                div(
                  class = "p-2",
                  uiOutput("diagrama_topologia")
                )
              ),              
              nav_panel(
                title = "Mapa y perfil de calidad del agua por cumplimiento de metas",
                  div(
                    class = "p-3",
                    div(
                      class = "row g-3 align-items-end mb-2",
                      div(class = "col-md-3", checkboxInput("chk_est_q2k", "Activar estaciones", value = TRUE)),
                      div(class = "col-md-4",
                          selectInput("sel_param_q2k", "Parámetro:", choices = choices_param_q2k,
                                      selected = if ("OD (mg/L)" %in% choices_param_q2k) "OD (mg/L)" else choices_param_q2k[1])),
                      div(class = "col-md-3",
                          selectInput("sel_hor_q2k", "Escenario:", choices = horizontes_metas, selected = horizontes_metas[1])),
                      div(class = "col-md-2",
                          numericInput("num_tol_q2k", "Tolerancia (%) valor medio", value = 5, min = 0, max = 50, step = 1))
                    ),
                    leafletOutput("mapa_modelo_calidad_agua", height = "560px"),
                    tags$p(class = "small text-secondary mt-2 mb-3",
                           "Cada subtramo (ESTInicio - ESTFinal) se colorea comparando el resultado del Qual2k con la meta actual de su tramo. ",
                           "Haga clic en un subtramo para ver el detalle."),
                    hr(),
                    h5("Metas actuales vs simulación Qual2k"),
                    div(class = "row g-3 align-items-end mb-2",
                        div(class = "col-md-4",
                            selectInput("sel_rio_graf_q2k", "Corriente del gráfico:", choices = rios_meta, selected = "RO"))),
                    plotlyOutput("grafico_calidad_q2k", height = "460px"),
                    hr(),
                    h5("Resultados Metas Actuales vs Modelo Qual2k"),
                    DT::DTOutput("tabla_calidad_q2k")
                  )
              )
            )
          )
        )
      )
    ),
    
    
    # ==============================================================================
    # UI- Sección de Situación Actual
    # ==============================================================================
    

    nav_panel_hidden(
      value = "sec_situacion_actual",
      div(
        h3("Situación Actual"),
        hr(),
        br()
      ),
      
      card(
        full_screen = TRUE,
        class = "shadow-sm border-0 mb-4",
        card_header(
          class = "d-flex align-items-center justify-content-between",
          div(icon("map-location-dot"), "Situación Actual de la Calidad del Agua"),
          tags$button(
            class = "btn btn-sm p-0 text-reset border-0 opacity-75 opacity-100-hover me-4",
            type = "button",
            `data-bs-toggle` = "collapse",
            `data-bs-target` = "#collapse_situacion_actual",
            `aria-expanded` = "true",
            icon("minus")
          )
        ),
        div(
          id = "collapse_situacion_actual",
          class = "collapse show",
          card_body(
            padding = 0,
            navset_card_tab(
              id = "tabs_situacion",
              nav_panel(
                title = "Análisis Hidrológico",
                div(
                  class = "p-3",

                  # ---------------- Mapa comparativo de caudales mensuales ----------------
                  h5(icon("water"), " Caudales mensuales por tramo: escenario medio vs. escenario seco"),
                  uiOutput("msg_hidro"),
                  div(
                    class = "row g-3 align-items-end mb-2",
                    div(class = "col-md-2",
                        selectInput("sel_rio_hid", "Corriente:", choices = choices_rio_hid)),
                    div(class = "col-md-3",
                        selectInput("sel_tramo_hid", "Tramo (con estaciones):", choices = NULL)),
                    div(class = "col-md-2",
                        selectInput("sel_mes_hid", "Mes:", choices = meses_hid, selected = meses_hid[1])),
                    div(class = "col-md-2",
                        div(
                          class = "btn-group shadow-sm mb-3", role = "group",
                          `aria-label` = "Controles de animación de meses",
                          actionButton("btn_prev_mes_hid", label = NULL, icon = icon("step-backward"),
                                       class = "btn btn-outline-secondary btn-sm", title = "Mes anterior"),
                          actionButton("btn_play_mes_hid", label = " Play", icon = icon("play"),
                                       class = "btn btn-success btn-sm fw-bold",
                                       title = "Iniciar/Pausar recorrido automático de los meses"),
                          actionButton("btn_next_mes_hid", label = NULL, icon = icon("step-forward"),
                                       class = "btn btn-outline-secondary btn-sm", title = "Mes siguiente")
                        )),
                    div(class = "col-md-3",
                        checkboxInput("chk_drenajes_hid", "Mostrar quebradas", value = TRUE))
                  ),
                  div(
                    class = "row g-2",
                    div(class = "col-lg-6", leafletOutput("mapa_hid_promedio", height = "480px")),
                    div(class = "col-lg-6", leafletOutput("mapa_hid_seca", height = "480px"))
                  ),
                  tags$p(class = "small text-secondary mt-2 mb-2",
                         "Cada círculo es una estación SUP de aporte (tributario) del tramo: el tamaño y el color indican el caudal del mes ",
                         "seleccionado, con la misma escala en los dos mapas para poder compararlos. Haga clic en una estación para ver la quebrada ",
                         "más cercana. Los mapas se mueven de forma sincronizada."),
                  uiOutput("resumen_hid"),
                  plotlyOutput("grafico_hid_mensual", height = "400px"),
                  tags$p(class = "small text-secondary",
                         "Línea continua = caudales medios (hoja Promedio); línea punteada = caudales bajos (hoja Seca). ",
                         "La línea vertical marca el mes seleccionado.")
                )
              ),
              nav_panel(
                title = "Análisis de la calidad del agua",
                div(
                  class = "p-3",

                  # ---------------- Mapa de cumplimiento de metas por escenario Qual2k ----------------
                  h5(icon("map-location-dot"), " Mapa de cumplimiento de metas por tramo: clima medio (promedio) vs. clima seco (bajo)"),
                  uiOutput("msg_esc"),
                  div(
                    class = "row g-3 align-items-end mb-2",
                    div(class = "col-md-5",
                        selectInput("sel_var_esc", "Parámetro simulado:", choices = choices_var_esc,
                                    selected = if ("OD (mg/L)" %in% choices_var_esc) "OD (mg/L)" else choices_var_esc[1])),
                    div(class = "col-md-4",
                        selectInput("sel_hor_esc", "Meta (horizonte):", choices = horizontes_metas, selected = horizontes_metas[1])),
                    div(class = "col-md-3",
                        numericInput("num_tol_esc", "Tolerancia (%) valor medio", value = 5, min = 0, max = 50, step = 1))
                  ),
                  div(
                    class = "row g-2",
                    div(class = "col-lg-6", leafletOutput("mapa_esc_medio", height = "520px")),
                    div(class = "col-lg-6", leafletOutput("mapa_esc_seco", height = "520px"))
                  ),
                  tags$p(class = "small text-secondary mt-2 mb-2",
                         "Cada subtramo se colorea comparando el resultado del Qual2k de cada escenario con la meta actual de su tramo ",
                         "(rojo: incumple, amarillo: igual dentro de la tolerancia, verde: cumple). Los parámetros, el horizonte de la meta ",
                         "y la tolerancia también controlan el perfil y la tabla de abajo. Los mapas se mueven de forma sincronizada."),

                  hr(),

                  # ---------------- Perfil comparativo de metas por escenario ----------------
                  h5(icon("chart-line"), " Perfil comparativo de metas por tramo: clima medio (promedio) vs. clima seco (bajo)"),
                  div(
                    class = "row g-3 align-items-end mb-2",
                    div(class = "col-md-3",
                        selectInput("sel_rio_esc", "Corriente:", choices = rios_meta, selected = "RO"))
                  ),
                  plotlyOutput("grafico_perfil_escenarios", height = "500px"),
                  tags$p(class = "small text-secondary mt-1",
                         "Círculos = clima medio (promedio); rombos = clima seco (bajo). El relleno indica el cumplimiento de la meta actual del tramo ",
                         "(verde cumple, amarillo igual dentro de la tolerancia, rojo incumple). La línea negra es la meta."),
                  hr(),
                  h5("Resultados por subtramo: metas actuales vs escenarios Qual2k"),
                  DT::DTOutput("tabla_perfil_escenarios")
                )
              )
            )
          )
        )
      )
    ),
    
    
    
    # ==============================================================================
    # UI- Sección de Simulación de Escenarios
    # ==============================================================================
    

    nav_panel_hidden(
      value = "sec_simulacion_escenarios",
      div(
        h3("Simulación de escenarios"),
        hr(),
        br()
      ),
      
      card(
        full_screen = TRUE,
        class = "shadow-sm border-0 mb-4",
        card_header(
          class = "d-flex align-items-center justify-content-between",
          div(icon("map-location-dot"), "Simulación de escenarios"),
          tags$button(
            class = "btn btn-sm p-0 text-reset border-0 opacity-75 opacity-100-hover me-4",
            type = "button",
            `data-bs-toggle` = "collapse",
            `data-bs-target` = "#collapse_simulacion_escenarios",
            `aria-expanded` = "true",
            icon("minus")
          )
        ),
        div(
          id = "collapse_simulacion_escenarios",
          class = "collapse show",
          card_body(
            padding = 0,
            navset_card_tab(
              id = "tabs_simulacion",
              nav_panel(
                title = "Simulación de escenarios",
                div(
                  class = "p-0",
                  card(
                    full_screen = TRUE,
                    class = "border-0 mb-0",
                    card_body(class = "p-0", uiOutput("powerbi_simulacion"))
                  )
                )
              )
            )
          )
        )
      )
    ),
    
    # ==============================================================================
    # UI- Sección de Componente Programático
    # ==============================================================================
    

    nav_panel_hidden(
      value = "sec_componente_programatico",
      div(
        h3("Componente Programático"),
        hr(),
        br()
      ),
      
      card(
        full_screen = TRUE,
        class = "shadow-sm border-0 mb-4",
        card_header(
          class = "d-flex align-items-center justify-content-between",
          div(icon("map-location-dot"), "Componente Programático"),
          tags$button(
            class = "btn btn-sm p-0 text-reset border-0 opacity-75 opacity-100-hover me-4",
            type = "button",
            `data-bs-toggle` = "collapse",
            `data-bs-target` = "#collapse_componente_programatico",
            `aria-expanded` = "true",
            icon("minus")
          )
        ),
        div(
          id = "collapse_componente_programatico",
          class = "collapse show",
          card_body(
            padding = 0,
            navset_card_tab(
              id = "tabs_programatico",
              nav_panel(
                title = "Diagrama de Sol",
                div(
                  class = "p-3",
                  style = "background: white;",
                  uiOutput("msg_sunburst"),
                  plotlyOutput("SBcontprog", height = "700px")
                )
              )
            )
          )
        )
      )
    ),

      
      nav_panel_hidden(
        value = "sec_estaciones",
        div(
          h3("Información de las estaciones de calidad"),
          hr(),
          br(),
          
          card(
            full_screen = TRUE,
            class = "shadow-sm border-0 mb-4",
            card_header(
              class = "d-flex align-items-center justify-content-between",
              div(icon("map-location-dot"), "Mapa de información por estaciones"),
              tags$button(
                class = "btn btn-sm p-0 text-reset border-0 opacity-75 opacity-100-hover me-4",
                type = "button",
                `data-bs-toggle` = "collapse",
                `data-bs-target` = "#collapse_mapa_body",
                `aria-expanded` = "true",
                icon("minus")
              )
            ),
            div(
              id = "collapse_mapa_body",
              class = "collapse show",
              card_body(
                padding = 0,
                navset_card_tab(
                  id = "tabs_geovisor",
                  nav_panel(
                    title = "ICA-5 [IDEAM]", value = "ICA-5 [IDEAM]",
                    leafletOutput("mapa_ica5_ideam", height = "520px")
                  ),
                  nav_panel(
                    title = "ICA-6 [CARDER]", value = "ICA-6 [CARDER]",
                    leafletOutput("mapa_ica6_carder", height = "520px")
                  )
                )
              )
            )
          ),
          
          card(
            full_screen = TRUE,
            class = "shadow-sm border-0 mb-4",
            card_header(
              class = "d-flex align-items-center justify-content-between",
              div(icon("chart-line"), " Datos ICA"),
              tags$button(
                class = "btn btn-sm p-0 text-reset border-0 opacity-75 opacity-100-hover me-4",
                type = "button",
                `data-bs-toggle` = "collapse",
                `data-bs-target` = "#collapse_graficos_body",
                `aria-expanded` = "true",
                icon("minus")
              )
            ),
            div(
              id = "collapse_graficos_body",
              class = "collapse show",
              card_body(
                uiOutput("contenedor_graficos_dinamico")
              )
            )
          ),
          
          card(
            full_screen = TRUE,
            class = "shadow-sm border-0 mb-4",
            card_header(
              class = "d-flex align-items-center justify-content-between",
              div(icon("chart-line"), " Tendencias ICA (Regresión Lineal)"),
              tags$button(
                class = "btn btn-sm p-0 text-reset border-0 opacity-75 opacity-100-hover me-4",
                type = "button",
                `data-bs-toggle` = "collapse",
                `data-bs-target` = "#collapse_tendencias_body",
                `aria-expanded` = "true",
                icon("minus")
              )
            ),
            div(
              id = "collapse_tendencias_body",
              class = "collapse show",
              card_body(
                div(class = "mb-2", htmlOutput("infotend1")),
                div(class = "mb-3", htmlOutput("infotend")),
                uiOutput("contenedor_tendencias_dinamico")
              )
            )
          ),
          
          div(
            class = "mb-3 text-end",
            downloadButton("DowntableICAmapa", "Descargar datos de la tabla (CSV)", class = "btn-sm btn-outline-primary")
          ),
          
          card(
            full_screen = TRUE,
            class = "shadow-sm border-0 mb-4",
            card_header(
              class = "d-flex align-items-center justify-content-between",
              div(icon("table"), "Tabla y reportes"),
              tags$button(
                class = "btn btn-sm p-0 text-reset border-0 opacity-75 opacity-100-hover me-4",
                type = "button",
                `data-bs-toggle` = "collapse",
                `data-bs-target` = "#collapse_tabla_ica_body",
                `aria-expanded` = "true",
                icon("minus")
              )
            ),
            div(
              id = "collapse_tabla_ica_body",
              class = "collapse show",
              card_body(
                div(
                  class = "table-responsive",
                  DTOutput("tableICAmapa", width = "100%")
                )
              )
            )
          )
        )
      ),
      
      # ==============================================================================
      # UI- Sección de perfiles de calidad
      # ==============================================================================
      
      nav_panel_hidden(
        value = "sec_perfiles",
        div(
          h3("Perfiles de calidad"),
          hr(),
          br(),
          
          card(
            full_screen = TRUE,
            class = "shadow-sm border-0 mb-4",
            card_header(
              class = "d-flex align-items-center justify-content-between",
              div(icon("map-location-dot"), " Geovisor de Perfiles por Corriente"),
              tags$button(
                class = "btn btn-sm p-0 text-reset border-0 opacity-75 opacity-100-hover me-4",
                type = "button",
                `data-bs-toggle` = "collapse",
                `data-bs-target` = "#collapse_mapa_perfil_body",
                `aria-expanded` = "true",
                icon("minus")
              )
            ),
            div(
              id = "collapse_mapa_perfil_body",
              class = "collapse show",
              card_body(
                padding = 0,
                leafletOutput("mapa_perfil", height = "600px")
              )
            )
          ),
          
          card(
            full_screen = TRUE,
            class = "shadow-sm border-0 mb-4",
            card_header(
              class = "d-flex align-items-center justify-content-between",
              div(icon("chart-line"), " Análisis de Perfil por Parámetro"),
              tags$button(
                class = "btn btn-sm p-0 text-reset border-0 opacity-75 opacity-100-hover me-4",
                type = "button",
                `data-bs-toggle` = "collapse",
                `data-bs-target` = "#collapse_grafico_perfil_body",
                `aria-expanded` = "true",
                icon("minus")
              )
            ),
            div(
              id = "collapse_grafico_perfil_body",
              class = "collapse show",
              card_body(
                fluidRow(
                  column(
                    width = 6,
                    selectInput(
                      inputId = "parametro_perfil",
                      label = "Seleccione el Parámetro:",
                      choices = c("Cargando..." = ""),
                      selected = NULL,
                      width = "100%"
                    )
                  ),
                  column(
                    width = 6,
                    div(
                      class = "d-flex align-items-center h-100 mt-3",
                      checkboxInput(
                        inputId = "log_scale_perfil",
                        label = "Eje Y en escala logarítmica",
                        value = FALSE
                      )
                    )
                  )
                ),
                hr(),
                plotlyOutput("grafico_perfil_render", height = "500px")
              )
            )
          ),
          
          card(
            full_screen = TRUE,
            class = "shadow-sm border-0 mb-4",
            card_header(
              class = "d-flex align-items-center justify-content-between",
              div(icon("water"), " Perfil Longitudinal ICA-5 [IDEAM]"),
              tags$button(
                class = "btn btn-sm p-0 text-reset border-0 opacity-75 opacity-100-hover me-4",
                type = "button",
                `data-bs-toggle` = "collapse",
                `data-bs-target` = "#collapse_perfil0_body",
                `aria-expanded` = "true",
                icon("minus")
              )
            ),
            div(
              id = "collapse_perfil0_body",
              class = "collapse show",
              card_body(
                plotlyOutput("Perfil0", height = "500px")
              )
            )
          )
        )
      )
      
      
      
    )
  )
)

# ============================================================================================================================================================
#                                                                       Logica del Servidor
# ============================================================================================================================================================

server <- function(input, output, session) {
  
  VALORES <- reactiveValues()
  
  observeEvent(input$theme_choice, {
    session$setCurrentTheme(
      bs_theme(version = 5, bootswatch = input$theme_choice)
    )
  })
  
  mostrar_seccion_fullscreen <- function(titulo, contenido_ui) {
    showModal(modalDialog(
      title = div(
        class = "d-flex justify-content-between align-items-center w-100",
        h4(titulo, class = "m-0 fw-bold"),
        modalButton("Cerrar", icon = icon("xmark"))
      ),
      contenido_ui,
      size = "xl",
      easyClose = TRUE,
      footer = NULL,
      class = "modal-fullscreen-custom"
    ))
  }
  
  observeEvent(input$btn_nav_dashboard, {
    removeModal()
  })
  
  observeEvent(input$btn_nav_estaciones, {
    nav_select("secciones_main", selected = "sec_estaciones")
  })
  
  observeEvent(input$btn_nav_perfiles, {
    nav_select("secciones_main", selected = "sec_perfiles")
  })
  
  observeEvent(input$btn_nav_objetivos_calidad_agua, {
    nav_select("secciones_main", selected = "sec_objetivos_calidad_agua")
  })
  
  observeEvent(input$btn_nav_modelo_calidad_agua, {
    nav_select("secciones_main", selected = "sec_modelo_calidad_agua")
  })
  
  observeEvent(input$btn_nav_situacion_actual, {
    nav_select("secciones_main", selected = "sec_situacion_actual")
  })
  
  observeEvent(input$btn_nav_simulacion_escenarios, {
    nav_select("secciones_main", selected = "sec_simulacion_escenarios")
  })
  
  observeEvent(input$btn_nav_componente_programatico, {
    nav_select("secciones_main", selected = "sec_componente_programatico")
  })
  
  # ==============================================================================
  # Server Diagrama de Sol (Componente Programático)
  # ==============================================================================
  
  output$msg_sunburst <- renderUI({
    if (is.null(dataSBcontprog))
      div(class = "alert alert-warning",
          "No se encontró el archivo 'Dsol_PORH2_colores.csv' en la carpeta de la aplicación.")
  })
  
  output$SBcontprog <- renderPlotly({
    req(dataSBcontprog)
    m <- list(l = 0, r = 0, b = 0, t = 0, pad = 0)
    plot_ly(dataSBcontprog, ids = ~Id, labels = ~labels, parents = ~parents,
            type = "sunburst",
            insidetextorientation = "horizontal",
            marker = list(colors = ~Color)) %>%
      layout(autosize = TRUE, margin = m)
  })
  
  # ==============================================================================
  # Server Ventanas Flotantes
  # ==============================================================================
    
  observeEvent(input$btn_nav_intro, {
    mostrar_seccion_fullscreen(
      "Introducción al PORH-Tool",
      div(
        class = "p-4",
        h2("Bienvenido a Era App Dashboard", class = "mb-4"),
        p("Esta plataforma permite visualizar datos geoespaciales interactivos y analizar métricas relacionadas con los recursos hídricos.", 
          style = "text-align: justify;"),
        hr(),
        p("Utilice el panel izquierdo para navegar, el panel central para realizar consultas y visualizaciones de datos.", 
          style = "text-align: justify;")
      )
    )
  })
  
  observeEvent(input$btn_nav_equipo, {
    mostrar_seccion_fullscreen(
      "Equipo de Trabajo",
      div(
        class = "p-3 p-md-4",
        h3("Nuestro Equipo", class = "fw-bold mb-4 border-bottom pb-2"),
        div(
          class = "row row-cols-1 row-cols-sm-2 row-cols-md-3 row-cols-lg-5 g-3 justify-content-center",
          div(
            class = "col d-flex align-items-stretch",
            div(
              class = "card w-100 p-3 border rounded-4 shadow-sm d-flex flex-column justify-content-between text-center",
              style = "background: rgba(255, 255, 255, 0.05); backdrop-filter: blur(10px); min-height: 340px;",
              div(
                div(class = "bg-secondary bg-opacity-20 rounded-circle mx-auto mb-3 d-flex align-items-center justify-content-center", style = "width: 96px; height: 96px;", icon("user", class = "fa-2x text-secondary")),
                h5("Integrante 1", class = "fw-bold text-primary mb-1", style = "font-size: 0.95rem;"),
                p("Cargo o Rol Principal", class = "text-muted small mb-3", style = "font-size: 0.78rem;")
              ),
              div(
                class = "border-top pt-2 d-flex flex-column gap-1 small",
                tags$a(href = "#", target = "_blank", class = "text-decoration-none text-info", icon("linkedin"), " LinkedIn"),
                tags$a(href = "#", class = "text-decoration-none text-secondary", icon("envelope"), " Email")
              )
            )
          )
        )
      )
    )
  })
  
  observeEvent(input$btn_nav_creditos, {
    mostrar_seccion_fullscreen(
      "Créditos y Agradecimientos",
      div(
        class = "p-4",
        h3("Reconocimientos"),
        p("Agradecimiento especial a las fuentes de información geográfica y herramientas Open Source utilizadas:"),
        tags$ul(
          tags$li("Leaflet & R-Leaflet Community"),
          tags$li("OpenStreetMap & Esri Tile Providers"),
          tags$li("Librerías sf & bslib de la comunidad de R")
        )
      )
    )
  })
  
  observeEvent(input$btn_nav_contacto, {
    mostrar_seccion_fullscreen(
      "Contacto y Soporte",
      div(
        class = "p-4",
        h3("Soporte Técnico"),
        p("Si tiene inconvenientes con la plataforma o requiere asistencia para cargar capas GIS, contáctenos:"),
        br(),
        div(class = "alert alert-info", icon("envelope"), " Correo electrónico: soporte@sei.org"),
        div(class = "alert alert-secondary", icon("phone"), " Teléfono: +57 (123 4567 8901)")
      )
    )
  })
  
  # ==============================================================================
  # Server- Sección de Información de las estaciones
  # ==============================================================================
  
  ERA_ICA <- reactive({
    estacion_sel <- if (is.null(input$EstacionICA) || input$EstacionICA == "") "Todas" else input$EstacionICA
    
    if (estacion_sel == "Todas") {
      return(ICA_datos1)
    } else {
      return(ICA_datos1[ICA_datos1$Estacion == estacion_sel, ])
    }
  })
  
  # Índice ICA activo según la pestaña del mapa que ve el usuario
  ica_activo <- reactive({
    t <- input$tabs_geovisor
    if (is.null(t) || !(t %in% c("ICA-5 [IDEAM]", "ICA-6 [CARDER]"))) "ICA-5 [IDEAM]" else t
  })

  output$contenedor_tendencias_dinamico <- renderUI({
    if (ica_activo() == "ICA-6 [CARDER]") plotlyOutput("ICAt2", height = "380px")
    else plotlyOutput("ICAt1", height = "380px")
  })

  output$contenedor_graficos_dinamico <- renderUI({
    tipo <- ica_activo()

    if (tipo == "ICA-5 [IDEAM]") {
      fluidRow(
        column(6, plotlyOutput("ICA1", height = "480px")),
        column(6, plotlyOutput("ICA1b", height = "480px"))
      )
    } else if (tipo == "ICA-6 [CARDER]") {
      fluidRow(
        column(6, plotlyOutput("ICA2", height = "480px")),
        column(6, plotlyOutput("ICA2b", height = "480px"))
      )
    } else {
      tagList(
        fluidRow(
          column(6, plotlyOutput("ICA1", height = "450px")),
          column(6, plotlyOutput("ICA1b", height = "450px"))
        ),
        hr(),
        fluidRow(
          column(6, plotlyOutput("ICA2", height = "450px")),
          column(6, plotlyOutput("ICA2b", height = "450px"))
        )
      )
    }
  })
  
  lapply(1:2, function(h) {
    output[[paste0("ICA", h)]] <- renderPlotly({
      ICA_datosT <- ERA_ICA()
      req(nrow(ICA_datosT) > 0)
      
      Estacionf <- if (is.null(input$EstacionICA)) "Todas" else input$EstacionICA
      ICAs <- c("ICA-5 [IDEAM]", "ICA-6 [CARDER]")
      ICAsc <- c("Categoria_ICA_IDEAM", "Categoria_ICA_CARDER")
      cols <- c("Estacion", "Fecha", ICAs[h], ICAsc[h])
      cols1 <- c("Estacion", "Fecha", "Valor", "Categoria")
      
      d <- ICA_datosT[, cols]
      colnames(d) <- cols1
      d <- d[!is.na(d$Valor), ]
      req(nrow(d) > 0)
      d <- d[order(d$Estacion, d$Fecha), ]
      
      CategoriasSub <- CategoriasICA[CategoriasICA$Variable == "ICA IDEAM", ]
      d <- merge(d, CategoriasSub[, c("Categoria", "CodigoColor", "ordenOriginal")], by = "Categoria")
      levels_cat <- CategoriasSub$Categoria
      d$Categoria <- factor(d$Categoria, levels = levels_cat)
      
      palc <- unique(d[, c("Categoria", "CodigoColor", "ordenOriginal")])
      palc <- palc[order(palc$ordenOriginal), ]
      pal <- setNames(palc$CodigoColor, palc$Categoria)
      
      m <- list(l = 50, r = 30, b = 60, t = 60, pad = 4)
      y <- list(title = ICAs[h], range = c(0, 1))
      x <- list(title = "Fecha", type = "date")
      
      plot_ly(
        d, x = ~Fecha, y = ~Valor, type = 'scatter', mode = 'markers', marker = list(size = 10),
        text = ~paste("Estación: ", Estacion, '<br>',
                      "Fecha: ", Fecha, '<br>',
                      ICAs[h], ": ", Valor, '<br>',
                      "Categoría: ", Categoria, '<br>'),
        color = ~Categoria, colors = pal
      ) %>%
        layout(autosize = TRUE, margin = m, showlegend = TRUE,
               title = list(text = paste0("<b>", ICAs[h], "</b><br><sub style='font-size:11px;'>Estación: ", Estacionf, "</sub>")),
               xaxis = x, yaxis = y)
    })
    
    output[[paste0("ICA", h, "b")]] <- renderPlotly({
      ICA_datosT <- ERA_ICA()
      req(nrow(ICA_datosT) > 0)
      
      Estacionf <- if (is.null(input$EstacionICA)) "Todas" else input$EstacionICA
      ICAs <- c("ICA-5 [IDEAM]", "ICA-6 [CARDER]")
      cols <- c("Estacion", "Fecha", ICAs[h])
      cols1 <- c("Estacion", "Fecha", "Valor")
      
      d <- ICA_datosT[, cols]
      colnames(d) <- cols1
      d <- d[!is.na(d$Valor), ]
      req(nrow(d) > 0)
      
      d$Estacion_EjeX <- d$Estacion
      d <- d[order(d$Estacion_EjeX, d$Fecha), ]
      
      if (Estacionf != "Todas") {
        m <- list(l = 50, r = 30, b = 80, t = 60, pad = 4)
        x <- list(
          title = "Estacion",
          type = "category",
          tickangle = 0,
          showticklabels = TRUE,
          tickfont = list(size = 10)
        )
      } else {
        m <- list(l = 50, r = 30, b = 200, t = 60, pad = 4)
        x <- list(
          title = "Estaciones",
          type = "category",
          tickangle = -90,
          showticklabels = TRUE,
          tickmode = "linear",
          dtick = 1,
          tickfont = list(size = 6)
        )
      }
      
      y <- list(title = ICAs[h], range = c(0, 1.05))
      xx <- sort(unique(d$Estacion_EjeX))
      
      CategoriasSub <- CategoriasICA[CategoriasICA$Variable == "ICA IDEAM", ]
      CategoriasSub$CodigoColor1 <- lighten(CategoriasSub$CodigoColor, amount = 0.2, method = "relative")
      
      shapes_list <- list()
      if (length(xx) > 0) {
        for (k in 1:nrow(CategoriasSub)) {
          shapes_list[[k]] <- list(
            type = "rect",
            fillcolor = CategoriasSub$CodigoColor1[k],
            line = list(color = CategoriasSub$CodigoColor1[k]),
            opacity = 0.25,
            x0 = xx[1], x1 = xx[length(xx)], xref = "x",
            y0 = CategoriasSub$Vmin[k], y1 = CategoriasSub$Vmax[k], yref = "y"
          )
        }
      }
      
      plot_ly() %>%
        add_trace(
          data = d, x = ~Estacion_EjeX, y = ~Valor, type = 'box',
          showlegend = FALSE, boxpoints = "all", jitter = 0.5,
          pointpos = 0, marker = list(size = 4, color = "grey")
        ) %>%
        layout(
          autosize = TRUE, margin = m,
          title = list(text = paste0("<b>Distribución ", ICAs[h], "</b><br><sub style='font-size:11px;'>Estación: ", Estacionf, "</sub>")),
          xaxis = x, yaxis = y, shapes = shapes_list
        )
    })
  })
  
  estacion_tendencia_activa <- reactive({
    req(input$EstacionICA)
    
    if (input$EstacionICA == "Todas") {
      est_validas <- sort(unique(ICA_Tendencias$Estacion))
      req(length(est_validas) > 0)
      return(est_validas[1])
    } else {
      return(input$EstacionICA)
    }
  })
  
  datos_tendencias_filtrados <- reactive({
    est_sel <- estacion_tendencia_activa()
    
    df <- ICA_datos1[ICA_datos1$Estacion == est_sel, ]
    req(nrow(df) > 0)
    
    df$Fecha <- as.Date(df$Fecha)
    df <- df[!is.na(df$Fecha), ]
    df <- df[order(df$Fecha), ]
    return(df)
  })
  
  output$infotend1 <- renderUI({
    tot_estaciones <- length(unique(ICA_datos1$Estacion))
    tot_cumplen <- length(unique(ICA_Tendencias$Estacion))
    
    HTML(paste0(
      "<span style='color: #3C8DBC; font-weight: bold;'>En la sección de tendencias solo se evalúan estaciones con más de 3 años de datos.</span><br>",
      "<span style='color: #3C8DBC; font-weight: bold;'>De las ", tot_estaciones, " estaciones registradas, ", tot_cumplen, " cumplen este criterio.</span>"
    ))
  })
  
  output$infotend <- renderUI({
    df <- datos_tendencias_filtrados()
    est_activa <- estacion_tendencia_activa()
    
    HTML(paste0(
      "<span style='color: #3C8DBC;'>Estación visualizada: </span>",
      "<span style='color: red; font-weight: bold;'>", est_activa, "</span>",
      "<span style='color: #3C8DBC;'> | Registros totales: </span>",
      "<span style='color: red; font-weight: bold;'>", nrow(df), "</span>"
    ))
  })
  
  output$ICAt1 <- renderPlotly({
    df <- datos_tendencias_filtrados()
    est_activa <- estacion_tendencia_activa()
    req("ICA-5 [IDEAM]" %in% colnames(df))
    
    df_sub <- df[!is.na(df$`ICA-5 [IDEAM]`), ]
    req(nrow(df_sub) > 1)
    
    fit <- lm(`ICA-5 [IDEAM]` ~ Fecha, data = df_sub)
    df_sub$pred <- predict(fit)
    
    slope_annual <- coef(fit)[2] * 365
    r2 <- summary(fit)$r.squared
    
    col_map <- c("Mala" = "#FF9300", "Regular" = "#FEFF00", "Aceptable" = "#2BD70F", "Buena" = "#3161F3")
    
    plot_ly(data = df_sub, x = ~Fecha) %>%
      add_trace(
        y = ~`ICA-5 [IDEAM]`,
        color = ~Categoria_ICA_IDEAM,
        colors = col_map,
        type = 'scatter',
        mode = 'markers',
        marker = list(size = 9),
        showlegend = TRUE
      ) %>%
      add_trace(
        y = ~pred,
        type = 'scatter',
        mode = 'lines',
        line = list(color = "black", width = 1.2, dash = 'dash'),
        showlegend = FALSE,
        hoverinfo = 'none'
      ) %>%
      layout(
        title = list(
          text = paste0("ICA-5 [IDEAM]<br>Estación: ", est_activa),
          x = 0.5, font = list(size = 14)
        ),
        xaxis = list(title = "Fecha"),
        yaxis = list(title = "ICA-5 [IDEAM]", range = c(0, 1.05)),
        annotations = list(
          list(
            x = min(df_sub$Fecha),
            y = 1.0,
            text = paste0("Cambio anual (Pendiente Regresión Lineal*365) = ", round(slope_annual, 7), "<br>R² Regresión = ", round(r2, 3)),
            showarrow = FALSE,
            xanchor = 'left',
            yanchor = 'top'
          )
        )
      )
  })
  
  output$ICAt2 <- renderPlotly({
    df <- datos_tendencias_filtrados()
    est_activa <- estacion_tendencia_activa()
    req("ICA-6 [CARDER]" %in% colnames(df))
    
    df_sub <- df[!is.na(df$`ICA-6 [CARDER]`), ]
    req(nrow(df_sub) > 1)
    
    fit <- lm(`ICA-6 [CARDER]` ~ Fecha, data = df_sub)
    df_sub$pred <- predict(fit)
    
    slope_annual <- coef(fit)[2] * 365
    r2 <- summary(fit)$r.squared
    
    col_map <- c("Mala" = "#FF9300", "Regular" = "#FEFF00", "Aceptable" = "#2BD70F", "Buena" = "#3161F3")
    
    plot_ly(data = df_sub, x = ~Fecha) %>%
      add_trace(
        y = ~`ICA-6 [CARDER]`,
        color = ~Categoria_ICA_CARDER,
        colors = col_map,
        type = 'scatter',
        mode = 'markers',
        marker = list(size = 9),
        showlegend = TRUE
      ) %>%
      add_trace(
        y = ~pred,
        type = 'scatter',
        mode = 'lines',
        line = list(color = "black", width = 1.2, dash = 'dash'),
        showlegend = FALSE,
        hoverinfo = 'none'
      ) %>%
      layout(
        title = list(
          text = paste0("ICA-6 [CARDER]<br>Estación: ", est_activa),
          x = 0.5, font = list(size = 14)
        ),
        xaxis = list(title = "Fecha"),
        yaxis = list(title = "ICA-6 [CARDER]", range = c(0, 1.05)),
        annotations = list(
          list(
            x = min(df_sub$Fecha),
            y = 1.0,
            text = paste0("Cambio anual (Pendiente Regresión Lineal*365) = ", round(slope_annual, 7), "<br>R² Regresión = ", round(r2, 3)),
            showarrow = FALSE,
            xanchor = 'left',
            yanchor = 'top'
          )
        )
      )
  })
  
  observeEvent(input$cargar_zip, {
    req(input$cargar_zip)
    
    temp_dir <- file.path(tempdir(), paste0("zip_ext_", sample(1000:9999, 1)))
    dir.create(temp_dir, showWarnings = FALSE)
    
    tryCatch({
      unzip(input$cargar_zip$datapath, exdir = temp_dir)
    }, error = function(e) {
      showNotification("Error al descomprimir el archivo ZIP.", type = "error")
      return()
    })
    
    files_extracted <- list.files(temp_dir, full.names = TRUE, recursive = TRUE)
    spatial_file <- files_extracted[grep("\\.(shp|geojson|json|kml|topojson)$", files_extracted, ignore.case = TRUE)][1]
    
    if (is.na(spatial_file)) {
      showNotification("No se encontró ningún archivo vectorial válido dentro del .zip", type = "error")
      return()
    }
    
    tryCatch({
      layer_sf <- sf::st_read(spatial_file, quiet = TRUE) %>%
        sf::st_transform(4326)
      
      layer_sf <- layer_sf %>%
        dplyr::mutate(
          area_m2 = as.numeric(sf::st_area(.)),
          area_ha = round(area_m2 / 10000, 2)
        )
      
      bbox <- sf::st_bbox(layer_sf)
      
      leafletProxy("mapa_ica5_ideam") %>%
        clearGroup("Capa Subida") %>%
        addPolygons(
          data = layer_sf,
          group = "Capa Subida",
          color = "#2563eb",
          weight = 2,
          fillColor = "#3b82f6",
          fillOpacity = 0.4,
          highlightOptions = highlightOptions(
            weight = 3,
            color = "#1d4ed8",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          popup = ~paste0(
            "<div style='font-family: sans-serif; text-align: center; padding: 5px;'>",
            "<strong style='color: #2563eb;'>Capa Importada</strong><br>",
            "<span style='font-size: 15px; font-weight: bold;'>Área: ", format(area_ha, big.mark = "."), " ha</span><br>",
            "<small style='color: #6c757d;'> (", format(round(area_m2), big.mark = "."), " m²)</small>",
            "</div>"
          )
        ) %>%
        flyToBounds(
          lng1 = as.numeric(bbox["xmin"]),
          lat1 = as.numeric(bbox["ymin"]),
          lng2 = as.numeric(bbox["xmax"]),
          lat2 = as.numeric(bbox["ymax"])
        )
      
      showNotification("Capa espacial cargada y desplegada con éxito.", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error al procesar la capa espacial:", e$message), type = "error")
    })
  })
  
  observeEvent(input$FechaICA, {
    req(input$FechaICA)
    
    datos_filtrados_fecha <- ICA_datos1[
      ICA_datos1$Fecha >= input$FechaICA[1] & 
        ICA_datos1$Fecha <= input$FechaICA[2], 
    ]
    
    estaciones_disponibles <- sort(unique(datos_filtrados_fecha$Estacion))
    seleccion_actual <- if (input$EstacionICA %in% estaciones_disponibles) input$EstacionICA else "Todas"
    
    updateSelectInput(
      session, 
      inputId  = "EstacionICA", 
      choices  = c("Todas", estaciones_disponibles), 
      selected = seleccion_actual
    )
  })
  
  observeEvent(list(input$EstacionICA, input$radioICA), {
    req(input$FechaICA, input$EstacionICA)
    
    if (input$EstacionICA != "Todas") {
      estacion_sel <- ICA_datos1[ICA_datos1$Estacion == input$EstacionICA, ]
      
      if (nrow(estacion_sel) > 0) {
        x_lon <- unique(estacion_sel$LONGITUD)[1]
        y_lat <- unique(estacion_sel$LATITUD)[1]
        
        leafletProxy("mapa_ica5_ideam") %>% 
          clearGroup("highlighted_polygon1ICA") %>% 
          fitBounds(x_lon - 0.01, y_lat - 0.01, x_lon + 0.01, y_lat + 0.01) %>%
          addCircles(
            lng = x_lon, lat = y_lat, 
            radius = input$radioICA, 
            weight = 5, color = "black", fill = FALSE, opacity = 1,
            group = "highlighted_polygon1ICA"
          )
      }
    } else {
      leafletProxy("mapa_ica5_ideam") %>% clearGroup("highlighted_polygon1ICA")
    }
  }, ignoreInit = TRUE)
  
  datos_ica_filtrados <- reactive({
    req(input$FechaICA, input$EstacionICA)
    
    sub_datos <- ICA_datos1[
      ICA_datos1$Fecha >= input$FechaICA[1] & 
        ICA_datos1$Fecha <= input$FechaICA[2], 
    ]
    
    req(nrow(sub_datos) > 0)
    
    sub_datos <- aggregate(
      cbind(`ICA-5 [IDEAM]`, `ICA-6 [CARDER]`) ~ Estacion + LATITUD + LONGITUD + CORRIENTE, 
      data = sub_datos, 
      FUN = mean, 
      na.rm = TRUE
    )
    
    if (input$EstacionICA != "Todas") {
      sub_datos <- sub_datos[sub_datos$Estacion == input$EstacionICA, ]
    }
    
    return(sub_datos)
  })
  
  output$mapa_ica5_ideam <- renderLeaflet({
    df_ica <- datos_ica_filtrados()
    req(nrow(df_ica) > 0)
    
    colores_ICA <- c(
      "Muy Mala"  = "#FD1502", "Mala"      = "#FF9300", 
      "Regular"   = "#FEFF00", "Aceptable" = "#2BD70F", 
      "Buena"     = "#3161F3", "NA"        = "#808080"
    )
    
    colores_IMA <- c(
      "Muy Critica" = "#FF0000", "Critica"   = "#FF7F00", 
      "Dudosa"      = "#FFFF00", "Aceptable" = "#7CEB34", 
      "Buena"       = "#0000FF"
    )
    
    levels_cat_ica <- c("Muy Mala", "Mala", "Regular", "Aceptable", "Buena", "NA")
    levels_cat_ima <- c("Muy Critica", "Critica", "Dudosa", "Aceptable", "Buena")
    
    df_ica$Categoria_ICA_IDEAM <- cut(
      df_ica$`ICA-5 [IDEAM]`, 
      breaks = c(-Inf, 0.25, 0.50, 0.70, 0.85, 1.00, Inf), 
      labels = levels_cat_ica, 
      right = TRUE
    )
    
    IMA_datos1$Categoria_IMA <- factor(IMA_datos1$Categoria_IMA, levels = levels_cat_ima)
    factpal_ICA <- colorFactor(palette = unname(colores_ICA[levels_cat_ica]), levels = levels_cat_ica, na.color = "#808080")
    factpal_IMA <- colorFactor(palette = unname(colores_IMA[levels_cat_ima]), levels = levels_cat_ima, na.color = "#808080")
    labsICA <- paste0('<b>Estación ICA: </b>', df_ica$Estacion, '<br>',
                      '<b>ICA IDEAM (Prom): </b>', round(df_ica$`ICA-5 [IDEAM]`, 2), '<br>',
                      '<b>Categoría: </b>', df_ica$Categoria_ICA_IDEAM, '<br>',
                      '<b>Corriente: </b>', df_ica$CORRIENTE)
    
    labsIMA <- paste0('<b>Estación IMA: </b>', IMA_datos1$Estacion, '<br>',
                      '<b>IMA: </b>', round(IMA_datos1$IMA, 2), '<br>',
                      '<b>Categoría: </b>', IMA_datos1$Categoria_IMA, '<br>',
                      '<b>Corriente: </b>', IMA_datos1$CORRIENTE)
    
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri.WorldImagery") %>%
      addTiles(group = "OpenStreetMap") %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap, group = "Esri.NatGeoWorld") %>%
      addProviderTiles(providers$Esri.WorldPhysical, group = "Esri.WorldPhysical") %>%
      setView(lng = -75.7, lat = 4.8, zoom = 10) %>%
      addPolygons(
        data = Municipios, group = "Municipios", stroke = TRUE, color = "black", weight = 1, 
        fillColor = "#e5abf5", fillOpacity = 0.3,
        popup = ~paste0("<b>Municipio: </b>", MPIO_CNMBR)
      ) %>%
      addPolylines(
        data = RedHidrica, group = "Red Hídrica", color = "blue", weight = 1.5, opacity = 0.6
      ) %>%
      addCircles(
        data = df_ica, lng = ~LONGITUD, lat = ~LATITUD, group = "Estaciones ICA",
        radius = input$radioICA, stroke = TRUE, weight = 1, color = "#000000",
        fillColor = ~factpal_ICA(Categoria_ICA_IDEAM), fillOpacity = 0.9,
        popup = lapply(labsICA, htmltools::HTML)
      ) %>%
      addCircleMarkers(
        data = IMA_datos1, lng = ~LONGITUD, lat = ~LATITUD, group = "Estaciones IMA",
        radius = 6, stroke = TRUE, weight = 1, color = "#000000",
        fillColor = ~factpal_IMA(Categoria_IMA), fillOpacity = 0.9,
        popup = lapply(labsIMA, htmltools::HTML)
      ) %>%
      
      addLegend("bottomright", pal = factpal_ICA, values = factor(levels_cat_ica, levels = levels_cat_ica), title = "Índice ICA-5 IDEAM", opacity = 1, group = "Estaciones ICA") %>%
      
      leaflet.extras::addDrawToolbar(
        position = "topleft",
        targetGroup = "Herramientas de dibujo",
        polylineOptions = leaflet.extras::drawPolylineOptions(showLength = TRUE, metric = TRUE),
        polygonOptions = leaflet.extras::drawPolygonOptions(showArea = TRUE, metric = TRUE),
        circleOptions = leaflet.extras::drawCircleOptions(showRadius = TRUE, metric = TRUE),
        rectangleOptions = leaflet.extras::drawRectangleOptions(showArea = TRUE, metric = TRUE),
        markerOptions = leaflet.extras::drawMarkerOptions(),
        circleMarkerOptions = FALSE,
        editOptions = leaflet.extras::editToolbarOptions(selectedPathOptions = leaflet.extras::selectedPathOptions())
      ) %>%
      addLayersControl(
        position = "topright",
        baseGroups = c("Esri.WorldImagery", "OpenStreetMap", "Esri.NatGeoWorld", "Esri.WorldPhysical"),
        overlayGroups = c("Estaciones ICA", "Estaciones IMA", "Red Hídrica", "Municipios", "Herramientas de dibujo", "Capa Subida"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      hideGroup("Municipios") %>%
      addMiniMap(
        toggleDisplay = TRUE, position = "topright") %>%
      
      htmlwidgets::onRender("
        function(el, x) {
          var map = this;
          
          var searchControl = L.control({position: 'bottomleft'});
          searchControl.onAdd = function (map) {
            var div = L.DomUtil.create('div', 'leaflet-bar leaflet-control');
            div.style.backgroundColor = 'white';
            div.style.padding = '3px';
            div.style.display = 'flex';
            div.style.alignItems = 'center';

            div.innerHTML = '<input type=\"text\" class=\"custom-search-input\" placeholder=\"Buscar lugar en Colombia...\" style=\"width: 170px; padding: 2px 5px; border: 1px solid #ccc; outline: none; font-size: 12px; height: 24px;\">' +
                            '<button class=\"custom-search-btn\" title=\"Buscar\" style=\"background: #fff; border: 1px solid #ccc; border-left: none; cursor: pointer; padding: 2px 6px; height: 24px;\">🔍</button>';

            L.DomEvent.disableClickPropagation(div);
            return div;
          };
          searchControl.addTo(map);

          var gpsControl = L.control({position: 'bottomright'});
          var svgTarget = '<svg width=\"16\" height=\"16\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"#444\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\"><circle cx=\"12\" cy=\"12\" r=\"10\"/><line x1=\"12\" y1=\"2\" x2=\"12\" y2=\"6\"/><line x1=\"12\" y1=\"18\" x2=\"12\" y2=\"22\"/><line x1=\"2\" y1=\"12\" x2=\"6\" y2=\"12\"/><line x1=\"18\" y1=\"12\" x2=\"22\" y2=\"12\"/><circle cx=\"12\" cy=\"12\" r=\"3\" fill=\"#444\"/></svg>';
          var svgSpinner = '<svg width=\"16\" height=\"16\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"#2563eb\" stroke-width=\"2.5\" stroke-linecap=\"round\" style=\"animation: spin 1s linear infinite;\"><style>@keyframes spin { 100% { transform: rotate(360deg); } }</style><path d=\"M21 12a9 9 0 1 1-6.219-8.56\"/></svg>';

          gpsControl.onAdd = function (map) {
            var div = L.DomUtil.create('div', 'leaflet-bar leaflet-control');
            div.style.backgroundColor = 'white';
            div.style.padding = '0px';
            div.innerHTML = '<button class=\"gps-location-btn\" title=\"Obtener mi ubicación\" style=\"background: #fff; border: none; cursor: pointer; height: 30px; width: 30px; display: flex; align-items: center; justify-content: center; outline: none;\">' + svgTarget + '</button>';
            
            L.DomEvent.disableClickPropagation(div);
            return div;
          };
          gpsControl.addTo(map);

          var gpsMarker = null;
          var gpsCircle = null;

          var gpsBtn = el.querySelector('.gps-location-btn');
          if (gpsBtn) {
            L.DomEvent.on(gpsBtn, 'click', function() {
              if (!navigator.geolocation) {
                alert('Geolocalización no soportada en este navegador.');
                return;
              }

              gpsBtn.innerHTML = svgSpinner;

              navigator.geolocation.getCurrentPosition(
                function(pos) {
                  gpsBtn.innerHTML = svgTarget;
                  var lat = pos.coords.latitude;
                  var lng = pos.coords.longitude;
                  var acc = pos.coords.accuracy;

                  if (gpsMarker) map.removeLayer(gpsMarker);
                  if (gpsCircle) map.removeLayer(gpsCircle);

                  map.flyTo([lat, lng], 15);

                  gpsCircle = L.circle([lat, lng], {
                    radius: acc,
                    color: '#1d4ed8',
                    fillColor: '#3b82f6',
                    fillOpacity: 0.15,
                    weight: 1.5
                  }).addTo(map);

                  gpsMarker = L.marker([lat, lng])
                    .addTo(map)
                    .bindPopup('<b>Ubicación del usuario</b><br>Margen de precisión: ±' + Math.round(acc) + ' m')
                    .openPopup();
                },
                function(err) {
                  gpsBtn.innerHTML = svgTarget;
                  var msjs = {
                    1: 'Acceso denegado a la ubicación.',
                    2: 'Ubicación no disponible.',
                    3: 'Tiempo de espera agotado al consultar el GPS.'
                  };
                  alert(msjs[err.code] || 'Error al obtener la ubicación.');
                },
                { enableHighAccuracy: true, timeout: 10000, maximumAge: 0 }
              );
            });
          }

          var searchInput = el.querySelector('.custom-search-input');
          var searchBtn = el.querySelector('.custom-search-btn');

          function executeSearch() {
            var query = searchInput.value;
            if (!query) return;
            fetch('https://nominatim.openstreetmap.org/search?format=json&countrycodes=co&q=' + encodeURIComponent(query))
              .then(function(res) { return res.json(); })
              .then(function(data) {
                if (data && data.length > 0) {
                  map.flyTo([parseFloat(data[0].lat), parseFloat(data[0].lon)], 14);
                } else {
                  alert('Lugar no encontrado en Colombia.');
                }
              });
          }

          if (searchBtn && searchInput) {
            L.DomEvent.on(searchBtn, 'click', executeSearch);
            L.DomEvent.on(searchInput, 'keypress', function(e) {
              if (e.keyCode === 13) executeSearch();
            });
          }
        }
      ")
  })
  
  output$mapa_ica6_carder <- renderLeaflet({
    df_ica <- datos_ica_filtrados()
    req(nrow(df_ica) > 0)
    
    colores_ICA <- c(
      "Muy Mala"  = "#FD1502", "Mala"      = "#FF9300", 
      "Regular"   = "#FEFF00", "Aceptable" = "#2BD70F", 
      "Buena"     = "#3161F3", "NA"        = "#808080"
    )
    
    colores_IMA <- c(
      "Muy Critica" = "#FF0000", "Critica"   = "#FF7F00", 
      "Dudosa"      = "#FFFF00", "Aceptable" = "#7CEB34", 
      "Buena"       = "#0000FF"
    )
    
    levels_cat_ica <- c("Muy Mala", "Mala", "Regular", "Aceptable", "Buena", "NA")
    levels_cat_ima <- c("Muy Critica", "Critica", "Dudosa", "Aceptable", "Buena")
    
    df_ica$Categoria_ICA_CARDER <- cut(
      df_ica$`ICA-6 [CARDER]`, 
      breaks = c(-Inf, 0.25, 0.50, 0.70, 0.85, 1.00, Inf), 
      labels = levels_cat_ica, 
      right = TRUE
    )
    
    IMA_datos1$Categoria_IMA <- factor(IMA_datos1$Categoria_IMA, levels = levels_cat_ima)
    
    factpal_ICA <- colorFactor(palette = unname(colores_ICA[levels_cat_ica]), levels = levels_cat_ica, na.color = "#808080")
    factpal_IMA <- colorFactor(palette = unname(colores_IMA[levels_cat_ima]), levels = levels_cat_ima, na.color = "#808080")
    
    labsICA <- paste0('<b>Estación ICA: </b>', df_ica$Estacion, '<br>',
                      '<b>ICA CARDER (Prom): </b>', round(df_ica$`ICA-6 [CARDER]`, 2), '<br>',
                      '<b>Categoría: </b>', df_ica$Categoria_ICA_CARDER, '<br>',
                      '<b>Corriente: </b>', df_ica$CORRIENTE)
    
    labsIMA <- paste0('<b>Estación IMA: </b>', IMA_datos1$Estacion, '<br>',
                      '<b>IMA: </b>', round(IMA_datos1$IMA, 2), '<br>',
                      '<b>Categoría: </b>', IMA_datos1$Categoria_IMA, '<br>',
                      '<b>Corriente: </b>', IMA_datos1$CORRIENTE)
    
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri.WorldImagery") %>%
      addTiles(group = "OpenStreetMap") %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap, group = "Esri.NatGeoWorld") %>%
      addProviderTiles(providers$Esri.WorldPhysical, group = "Esri.WorldPhysical") %>%
      setView(lng = -75.7, lat = 4.8, zoom = 10) %>%
      
      addPolygons(
        data = Municipios, group = "Municipios", stroke = TRUE, color = "black", weight = 1, 
        fillColor = "#e5abf5", fillOpacity = 0.3,
        popup = ~paste0("<b>Municipio: </b>", MPIO_CNMBR)
      ) %>%
      addPolylines(
        data = RedHidrica, group = "Red Hídrica", color = "blue", weight = 1.5, opacity = 0.6
      ) %>%
      addCircles(
        data = df_ica, lng = ~LONGITUD, lat = ~LATITUD, group = "Estaciones ICA",
        radius = input$radioICA, stroke = TRUE, weight = 1, color = "#000000",
        fillColor = ~factpal_ICA(Categoria_ICA_CARDER), fillOpacity = 0.9,
        popup = lapply(labsICA, htmltools::HTML)
      ) %>%
      addCircleMarkers(
        data = IMA_datos1, lng = ~LONGITUD, lat = ~LATITUD, group = "Estaciones IMA",
        radius = 6, stroke = TRUE, weight = 1, color = "#000000",
        fillColor = ~factpal_IMA(Categoria_IMA), fillOpacity = 0.9,
        popup = lapply(labsIMA, htmltools::HTML)
      ) %>%
      
      addLegend("bottomright", pal = factpal_ICA, values = factor(levels_cat_ica, levels = levels_cat_ica), title = "Índice ICA-6 CARDER", opacity = 1, group = "Estaciones ICA") %>%
      
      leaflet.extras::addDrawToolbar(
        position = "topleft",
        targetGroup = "Herramientas de dibujo",
        polylineOptions = leaflet.extras::drawPolylineOptions(showLength = TRUE, metric = TRUE),
        polygonOptions = leaflet.extras::drawPolygonOptions(showArea = TRUE, metric = TRUE),
        circleOptions = leaflet.extras::drawCircleOptions(showRadius = TRUE, metric = TRUE),
        rectangleOptions = leaflet.extras::drawRectangleOptions(showArea = TRUE, metric = TRUE),
        markerOptions = leaflet.extras::drawMarkerOptions(),
        circleMarkerOptions = FALSE,
        editOptions = leaflet.extras::editToolbarOptions(selectedPathOptions = leaflet.extras::selectedPathOptions())
      ) %>%
      addLayersControl(
        position = "topright",
        baseGroups = c("Esri.WorldImagery", "OpenStreetMap", "Esri.NatGeoWorld", "Esri.WorldPhysical"),
        overlayGroups = c("Estaciones ICA", "Estaciones IMA", "Red Hídrica", "Municipios", "Herramientas de dibujo", "Capa Subida"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      hideGroup("Municipios") %>%
      addMiniMap(
        toggleDisplay = TRUE, position = "topright") %>%
      
      htmlwidgets::onRender("
        function(el, x) {
          var map = this;
          
          var searchControl = L.control({position: 'bottomleft'});
          searchControl.onAdd = function (map) {
            var div = L.DomUtil.create('div', 'leaflet-bar leaflet-control');
            div.style.backgroundColor = 'white';
            div.style.padding = '3px';
            div.style.display = 'flex';
            div.style.alignItems = 'center';

            div.innerHTML = '<input type=\"text\" class=\"custom-search-input\" placeholder=\"Buscar lugar en Colombia...\" style=\"width: 170px; padding: 2px 5px; border: 1px solid #ccc; outline: none; font-size: 12px; height: 24px;\">' +
                            '<button class=\"custom-search-btn\" title=\"Buscar\" style=\"background: #fff; border: 1px solid #ccc; border-left: none; cursor: pointer; padding: 2px 6px; height: 24px;\">🔍</button>';

            L.DomEvent.disableClickPropagation(div);
            return div;
          };
          searchControl.addTo(map);

          var gpsControl = L.control({position: 'bottomright'});
          var svgTarget = '<svg width=\"16\" height=\"16\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"#444\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\"><circle cx=\"12\" cy=\"12\" r=\"10\"/><line x1=\"12\" y1=\"2\" x2=\"12\" y2=\"6\"/><line x1=\"12\" y1=\"18\" x2=\"12\" y2=\"22\"/><line x1=\"2\" y1=\"12\" x2=\"6\" y2=\"12\"/><line x1=\"18\" y1=\"12\" x2=\"22\" y2=\"12\"/><circle cx=\"12\" cy=\"12\" r=\"3\" fill=\"#444\"/></svg>';
          var svgSpinner = '<svg width=\"16\" height=\"16\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"#2563eb\" stroke-width=\"2.5\" stroke-linecap=\"round\" style=\"animation: spin 1s linear infinite;\"><style>@keyframes spin { 100% { transform: rotate(360deg); } }</style><path d=\"M21 12a9 9 0 1 1-6.219-8.56\"/></svg>';

          gpsControl.onAdd = function (map) {
            var div = L.DomUtil.create('div', 'leaflet-bar leaflet-control');
            div.style.backgroundColor = 'white';
            div.style.padding = '0px';
            div.innerHTML = '<button class=\"gps-location-btn\" title=\"Obtener mi ubicación\" style=\"background: #fff; border: none; cursor: pointer; height: 30px; width: 30px; display: flex; align-items: center; justify-content: center; outline: none;\">' + svgTarget + '</button>';
            
            L.DomEvent.disableClickPropagation(div);
            return div;
          };
          gpsControl.addTo(map);

          var gpsMarker = null;
          var gpsCircle = null;

          var gpsBtn = el.querySelector('.gps-location-btn');
          if (gpsBtn) {
            L.DomEvent.on(gpsBtn, 'click', function() {
              if (!navigator.geolocation) {
                alert('Geolocalización no soportada en este navegador.');
                return;
              }

              gpsBtn.innerHTML = svgSpinner;

              navigator.geolocation.getCurrentPosition(
                function(pos) {
                  gpsBtn.innerHTML = svgTarget;
                  var lat = pos.coords.latitude;
                  var lng = pos.coords.longitude;
                  var acc = pos.coords.accuracy;

                  if (gpsMarker) map.removeLayer(gpsMarker);
                  if (gpsCircle) map.removeLayer(gpsCircle);

                  map.flyTo([lat, lng], 15);

                  gpsCircle = L.circle([lat, lng], {
                    radius: acc,
                    color: '#1d4ed8',
                    fillColor: '#3b82f6',
                    fillOpacity: 0.15,
                    weight: 1.5
                  }).addTo(map);

                  gpsMarker = L.marker([lat, lng])
                    .addTo(map)
                    .bindPopup('<b>Ubicación del usuario</b><br>Margen de precisión: ±' + Math.round(acc) + ' m')
                    .openPopup();
                },
                function(err) {
                  gpsBtn.innerHTML = svgTarget;
                  var msjs = {
                    1: 'Acceso denegado a la ubicación.',
                    2: 'Ubicación no disponible.',
                    3: 'Tiempo de espera agotado al consultar el GPS.'
                  };
                  alert(msjs[err.code] || 'Error al obtener la ubicación.');
                },
                { enableHighAccuracy: true, timeout: 10000, maximumAge: 0 }
              );
            });
          }

          var searchInput = el.querySelector('.custom-search-input');
          var searchBtn = el.querySelector('.custom-search-btn');

          function executeSearch() {
            var query = searchInput.value;
            if (!query) return;
            fetch('https://nominatim.openstreetmap.org/search?format=json&countrycodes=co&q=' + encodeURIComponent(query))
              .then(function(res) { return res.json(); })
              .then(function(data) {
                if (data && data.length > 0) {
                  map.flyTo([parseFloat(data[0].lat), parseFloat(data[0].lon)], 14);
                } else {
                  alert('Lugar no encontrado en Colombia.');
                }
              });
          }

          if (searchBtn && searchInput) {
            L.DomEvent.on(searchBtn, 'click', executeSearch);
            L.DomEvent.on(searchInput, 'keypress', function(e) {
              if (e.keyCode === 13) executeSearch();
            });
          }
        }
      ")
  })
  
  output$tableICAmapa <- renderDT(server = FALSE, {
    ICAs  <- c("ICA-5 [IDEAM]", "ICA-6 [CARDER]")
    ICAsc <- c("Categoria_ICA_IDEAM", "Categoria_ICA_CARDER")
    
    req(input$FechaICA, input$EstacionICA)
    
    Estacionf <- input$EstacionICA
    Fechaf    <- input$FechaICA
    
    ICA_datosSub <- ICA_datos1[ICA_datos1$Fecha >= Fechaf[1] & ICA_datos1$Fecha <= Fechaf[2], ]
    req(nrow(ICA_datosSub) > 0)
    
    ICA_datosSub1 <- ICA_datosSub
    ICA_datosSub1$N_Datos <- 1
    ICA_datosSub1 <- aggregate(N_Datos ~ Estacion, data = ICA_datosSub1, FUN = sum, na.rm = TRUE) 
    
    ICA_datosSub <- aggregate(
      ICA_datosSub[, ICAs, drop = FALSE],
      by = list(
        Estacion  = ICA_datosSub$Estacion, 
        LATITUD   = ICA_datosSub$LATITUD, 
        LONGITUD  = ICA_datosSub$LONGITUD, 
        CORRIENTE = ICA_datosSub$CORRIENTE
      ),
      FUN = mean, 
      na.rm = TRUE
    )
    
    ICA_datosSub$Categoria_ICA_IDEAM  <- "Sin informacion"
    ICA_datosSub$Categoria_ICA_CARDER <- "Sin informacion"
    Cat <- CategoriasICA
    
    for (i in seq_len(nrow(ICA_datosSub))) {
      if (!is.na(ICA_datosSub$`ICA-5 [IDEAM]`[i])) {
        cat_match <- Cat[Cat$Variable == "ICA IDEAM" & Cat$Vmin <= ICA_datosSub$`ICA-5 [IDEAM]`[i] & Cat$Vmax > ICA_datosSub$`ICA-5 [IDEAM]`[i], "Categoria"]
        if (length(cat_match) > 0) ICA_datosSub$Categoria_ICA_IDEAM[i] <- cat_match[1]
      }
      if (!is.na(ICA_datosSub$`ICA-6 [CARDER]`[i])) {
        cat_match <- Cat[Cat$Variable == "ICA IDEAM" & Cat$Vmin <= ICA_datosSub$`ICA-6 [CARDER]`[i] & Cat$Vmax > ICA_datosSub$`ICA-6 [CARDER]`[i], "Categoria"]
        if (length(cat_match) > 0) ICA_datosSub$Categoria_ICA_CARDER[i] <- cat_match[1]
      }
    }
    
    ICA_datosSub <- merge(ICA_datosSub, ICA_datosSub1, by = "Estacion")
    
    if (Estacionf != "Todas") {
      ICA_datosSub <- ICA_datosSub[ICA_datosSub$Estacion == Estacionf, ]
    }
    
    ICA_datosSub[, ICAs] <- round(ICA_datosSub[, ICAs], 2)
    k_ica <- match(ica_activo(), ICAs)   # solo el índice de la pestaña activa
    ICA_datosSub <- ICA_datosSub[, c("Estacion", "N_Datos", ICAs[k_ica], ICAsc[k_ica], "LATITUD", "LONGITUD", "CORRIENTE")]
    
    col_ndatos <- paste0("N_Datos_", paste(Fechaf, collapse = "_"))
    colnames(ICA_datosSub)[colnames(ICA_datosSub) == "N_Datos"] <- col_ndatos
    
    VALORES[["ICAtablamapa"]] <- ICA_datosSub
    
    DT::datatable(
      ICA_datosSub,
      extensions = 'Buttons',
      class = "table-sm table-striped hover compact w-100",
      filter = list(position = "top"),
      rownames = FALSE,
      options = list(
        pageLength = 10,
        lengthMenu = c(5, 10, 25, 50, 100),
        autoWidth = FALSE,
        scrollX = TRUE,
        dom = 'lBfrtip',
        buttons = list(
          list(extend = 'colvis', text = 'Columnas', className = 'btn-sm btn-outline-secondary'),
          list(extend = 'csv', text = 'Exportar CSV', className = 'btn-sm btn-outline-primary', exportOptions = list(columns = ':visible', modifier = list(page = 'all'))),
          list(extend = 'excel', text = 'Exportar Excel', className = 'btn-sm btn-outline-success', exportOptions = list(columns = ':visible', modifier = list(page = 'all')))
        )
      )
    )
  })
  
  output$DowntableICAmapa <- downloadHandler(
    filename = function() {
      paste0("datos_", if (ica_activo() == "ICA-6 [CARDER]") "ICA6_CARDER_" else "ICA5_IDEAM_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(VALORES[["ICAtablamapa"]], file, row.names = FALSE)
    }
  )
  
  # ==============================================================================
  # Server - Sección de Perfiles de Calidad
  # ==============================================================================
  
  output[["mapa_perfil"]] <- renderLeaflet({
    req(input$radioPerfil)
    Radiof <- input$radioPerfil
    
    shapescontexto <- PerfilesCapasMapa
    shapesContexto <- shapescontexto$ShapesContexto
    NshapesContexto <- shapescontexto$NombresShapesContexto
    CshapesContexto <- shapescontexto$ColorShapesContexto
    EshapesContexto <- shapescontexto$Etiqueta
    
    map <- leaflet() %>%
      addMapPane("principalpuntos", zIndex = 450) %>%
      addMapPane("principallineas", zIndex = 440) %>%
      addMapPane("principalpoligonos", zIndex = 430) %>%
      addMapPane("puntos", zIndex = 420) %>%
      addMapPane("lineas", zIndex = 410) %>%
      addMapPane("poligonos", zIndex = 400)
    
    for (i in 1:length(NshapesContexto)) {
      shp <- read_sf(dsn = paste0(shapesContexto[i]))
      labs <- lapply(seq(nrow(shp)), function(j) {
        paste0('<p>', shp[[EshapesContexto[i]]][j], '<p>')
      })
      
      if (st_geometry_type(shp, by_geometry = FALSE)[1] == "MULTIPOLYGON" | st_geometry_type(shp, by_geometry = FALSE)[1] == "POLYGON") {
        shp <- st_zm(shp, drop = TRUE, what = "ZM")
        map <- map %>% addPolygons(data = shp, group = NshapesContexto[i], weight = 2,
                                   color = CshapesContexto[i], options = pathOptions(pane = "poligonos"),
                                   highlight = highlightOptions(weight = 3, color = "yellow", fillOpacity = 0.3, bringToFront = FALSE),
                                   label = lapply(labs, htmltools::HTML))
      }
      if (st_geometry_type(shp, by_geometry = FALSE)[1] == "POINT") {
        map <- map %>% addCircles(data = as.data.frame(shp), lng = as.data.frame(st_coordinates(shp))[, "X"], lat = as.data.frame(st_coordinates(shp))[, "Y"],
                                  radius = 50, weight = 1, group = NshapesContexto[i],
                                  color = CshapesContexto[i], options = pathOptions(pane = "principalpuntos"),
                                  highlight = highlightOptions(weight = 3, color = "yellow", fillOpacity = 0.3, bringToFront = FALSE),
                                  label = lapply(labs, htmltools::HTML))
      }
      if (st_geometry_type(shp, by_geometry = FALSE)[1] == "MULTILINESTRING" | st_geometry_type(shp, by_geometry = FALSE)[1] == "LINESTRING") {
        shp <- st_zm(shp, drop = TRUE, what = "ZM")
        map <- map %>% addPolylines(data = shp, group = NshapesContexto[i], weight = 1.5,
                                    color = CshapesContexto[i], options = pathOptions(pane = "principallineas"),
                                    highlight = highlightOptions(weight = 2, color = "yellow", fillOpacity = 0.3, bringToFront = FALSE),
                                    label = lapply(labs, htmltools::HTML))
      }
    }
    
    shp <- st_zm(Perfilesshp, drop = TRUE, what = "ZM")
    labs <- lapply(seq(nrow(shp)), function(j) {
      paste0('<p>', "Corriente: ", shp[["NOMBRE"]][j], '<p>',
             "Longitud tramo: ", shp[["Length"]][j], '<p>',
             "Estacion tramo: ", shp[["ESTWQ"]][j], '<p>')
    })
    map <- map %>% addPolylines(data = shp, group = "Rios Perfiles", weight = 3,
                                color = "blue", options = pathOptions(pane = "principallineas"),
                                highlight = highlightOptions(weight = 2, color = "red", fillOpacity = 0.3, bringToFront = FALSE),
                                label = lapply(labs, htmltools::HTML))
    
    estaciones <- unique(ICA_estaciones[, c("NOMBRE", "Codigo", "CORRIENTE", "LONGITUD", "LATITUD")])
    estaciones <- estaciones[!is.element(unique(estaciones$Codigo), sort(unique(Perfiles_Datos$CODIGO))), ]
    estaciones <- estaciones[!is.na(estaciones$LONGITUD), ]
    estaciones <- estaciones[!is.na(estaciones$LATITUD), ]
    
    labs <- lapply(seq(nrow(estaciones)), function(j) {
      paste0('Nombre Estacion : ', estaciones[["NOMBRE"]][j], '<p></p>',
             'Codigo : ', estaciones[["Codigo"]][j], '<p></p>',
             'Corriente : ', estaciones[["CORRIENTE"]][j], '<p></p>',
             "Latitud : ", estaciones[["LATITUD"]][j], '<p></p>',
             "Longitud : ", estaciones[["LONGITUD"]][j], '<p></p>')
    })
    map <- map %>% addCircles(data = estaciones, lng = estaciones$LONGITUD, lat = estaciones$LATITUD,
                              radius = 400, weight = 1, group = "Otras Estaciones", fillOpacity = 0.8,
                              color = "#f5d65d", options = pathOptions(pane = "principalpuntos"),
                              highlight = highlightOptions(weight = 1, color = "yellow", fillOpacity = 1, bringToFront = TRUE),
                              label = lapply(labs, htmltools::HTML))
    
    CategoriasSub <- sort(unique(Perfilesshp$NOMBRE))
    levels <- sort(unique(Perfilesshp$NOMBRE))
    Perfiles_Datos$CORRIENTE <- factor(Perfiles_Datos$NOMBRE, levels)
    
    n <- length(unique(Perfilesshp$NOMBRE))
    qual_col_pals <- brewer.pal.info[brewer.pal.info$category == 'qual', ]
    col_vector <- unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
    factpal <- colorFactor(col_vector[10:(n + 9)], levels)
    
    estaciones <- unique(Perfiles_Datos[, c("AbcisaNombre", "CODIGO", "NOMBRE_ESTACION", "CORRIENTE", "Abscisakm", "LONGITUD", "LATITUD", "NOMBRE_COMPLETO")])
    labs <- lapply(seq(nrow(estaciones)), function(j) {
      paste0('Nombre Completo Estacion : ', estaciones[["AbcisaNombre"]][j], '<p></p>',
             'Codigo : ', estaciones[["CODIGO"]][j], '<p></p>',
             'Corriente : ', estaciones[["CORRIENTE"]][j], '<p></p>',
             'Estacion : ', estaciones[["NOMBRE_COMPLETO"]][j], '<p></p>',
             "Latitud : ", estaciones[["LATITUD"]][j], '<p></p>',
             "Longitud : ", estaciones[["LONGITUD"]][j], '<p></p>',
             "Abscisa Kilometro : ", round(estaciones[["Abscisakm"]][j], 2), '<p></p>')
    })
    
    Nleyenda <- "Estaciones por Corriente"
    map <- map %>% addCircles(data = estaciones, lng = estaciones$LONGITUD, lat = estaciones$LATITUD,
                              radius = Radiof, weight = 1, group = "Estaciones Perfiles", fillOpacity = 0.8, fillColor = factpal(estaciones$CORRIENTE),
                              color = "black", options = pathOptions(pane = "principalpuntos"),
                              highlight = highlightOptions(weight = 10, color = "yellow", fillOpacity = 1, bringToFront = TRUE),
                              label = lapply(labs, htmltools::HTML)) %>%
      addLegend("topright", pal = factpal, values = estaciones$CORRIENTE,
                title = Nleyenda,
                opacity = 1)
    
    map <- map %>% addTiles(group = "OpenStreetMap") %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap, group = "Esri.NatGeoWorld") %>%
      addProviderTiles(providers$Esri.WorldPhysical, group = "Esri.WorldPhysical") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri.WorldImagery") %>%
      
      leaflet.extras::addDrawToolbar(
        position = "topleft",
        targetGroup = "Herramientas de dibujo",
        polylineOptions = leaflet.extras::drawPolylineOptions(showLength = TRUE, metric = TRUE),
        polygonOptions = leaflet.extras::drawPolygonOptions(showArea = TRUE, metric = TRUE),
        circleOptions = leaflet.extras::drawCircleOptions(showRadius = TRUE, metric = TRUE),
        rectangleOptions = leaflet.extras::drawRectangleOptions(showArea = TRUE, metric = TRUE),
        markerOptions = leaflet.extras::drawMarkerOptions(),
        circleMarkerOptions = FALSE,
        editOptions = leaflet.extras::editToolbarOptions(selectedPathOptions = leaflet.extras::selectedPathOptions())
      ) %>%
      
      addLayersControl(
        position = "topright",
        baseGroups = c("OpenStreetMap", "Esri.NatGeoWorld", "Esri.WorldPhysical", "Esri.WorldImagery"),
        overlayGroups = c("Estaciones Perfiles", "Rios Perfiles", "Otras Estaciones", "Herramientas de dibujo", NshapesContexto),
        options = layersControlOptions(collapsed = TRUE)
      ) %>% 
      addMiniMap(toggleDisplay = TRUE, position = "topright") %>%
      
      htmlwidgets::onRender("
      function(el, x) {
        var map = this;
        
        var searchControl = L.control({position: 'bottomleft'});
        searchControl.onAdd = function (map) {
          var div = L.DomUtil.create('div', 'leaflet-bar leaflet-control');
          div.style.backgroundColor = 'white';
          div.style.padding = '3px';
          div.style.display = 'flex';
          div.style.alignItems = 'center';

          div.innerHTML = '<input type=\"text\" class=\"custom-search-input\" placeholder=\"Buscar lugar en Colombia...\" style=\"width: 170px; padding: 2px 5px; border: 1px solid #ccc; outline: none; font-size: 12px; height: 24px;\">' +
                          '<button class=\"custom-search-btn\" title=\"Buscar\" style=\"background: #fff; border: 1px solid #ccc; border-left: none; cursor: pointer; padding: 2px 6px; height: 24px;\">🔍</button>';

          L.DomEvent.disableClickPropagation(div);
          return div;
        };
        searchControl.addTo(map);

        var gpsControl = L.control({position: 'bottomright'});
        var svgTarget = '<svg width=\"16\" height=\"16\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"#444\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\"><circle cx=\"12\" cy=\"12\" r=\"10\"/><line x1=\"12\" y1=\"2\" x2=\"12\" y2=\"6\"/><line x1=\"12\" y1=\"18\" x2=\"12\" y2=\"22\"/><line x1=\"2\" y1=\"12\" x2=\"6\" y2=\"12\"/><line x1=\"18\" y1=\"12\" x2=\"22\" y2=\"12\"/><circle cx=\"12\" cy=\"12\" r=\"3\" fill=\"#444\"/></svg>';
        var svgSpinner = '<svg width=\"16\" height=\"16\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"#2563eb\" stroke-width=\"2.5\" stroke-linecap=\"round\" style=\"animation: spin 1s linear infinite;\"><style>@keyframes spin { 100% { transform: rotate(360deg); } }</style><path d=\"M21 12a9 9 0 1 1-6.219-8.56\"/></svg>';

        gpsControl.onAdd = function (map) {
          var div = L.DomUtil.create('div', 'leaflet-bar leaflet-control');
          div.style.backgroundColor = 'white';
          div.style.padding = '0px';
          div.innerHTML = '<button class=\"gps-location-btn\" title=\"Obtener mi ubicación\" style=\"background: #fff; border: none; cursor: pointer; height: 30px; width: 30px; display: flex; align-items: center; justify-content: center; outline: none;\">' + svgTarget + '</button>';
          
          L.DomEvent.disableClickPropagation(div);
          return div;
        };
        gpsControl.addTo(map);

        var gpsMarker = null;
        var gpsCircle = null;

        var gpsBtn = el.querySelector('.gps-location-btn');
        if (gpsBtn) {
          L.DomEvent.on(gpsBtn, 'click', function() {
            if (!navigator.geolocation) {
              alert('Geolocalización no soportada en este navegador.');
              return;
            }

            gpsBtn.innerHTML = svgSpinner;

            navigator.geolocation.getCurrentPosition(
              function(pos) {
                gpsBtn.innerHTML = svgTarget;
                var lat = pos.coords.latitude;
                var lng = pos.coords.longitude;
                var acc = pos.coords.accuracy;

                if (gpsMarker) map.removeLayer(gpsMarker);
                if (gpsCircle) map.removeLayer(gpsCircle);

                map.flyTo([lat, lng], 15);

                gpsCircle = L.circle([lat, lng], {
                  radius: acc,
                  color: '#1d4ed8',
                  fillColor: '#3b82f6',
                  fillOpacity: 0.15,
                  weight: 1.5
                }).addTo(map);

                gpsMarker = L.marker([lat, lng])
                  .addTo(map)
                  .bindPopup('<b>Ubicación del usuario</b><br>Margen de precisión: ±' + Math.round(acc) + ' m')
                  .openPopup();
              },
              function(err) {
                gpsBtn.innerHTML = svgTarget;
                var msjs = {
                  1: 'Acceso denegado a la ubicación.',
                  2: 'Ubicación no disponible.',
                  3: 'Tiempo de espera agotado al consultar el GPS.'
                };
                alert(msjs[err.code] || 'Error al obtener la ubicación.');
              },
              { enableHighAccuracy: true, timeout: 10000, maximumAge: 0 }
            );
          });
        }

        var searchInput = el.querySelector('.custom-search-input');
        var searchBtn = el.querySelector('.custom-search-btn');

        function executeSearch() {
          var query = searchInput.value;
          if (!query) return;
          fetch('https://nominatim.openstreetmap.org/search?format=json&countrycodes=co&q=' + encodeURIComponent(query))
            .then(function(res) { return res.json(); })
            .then(function(data) {
              if (data && data.length > 0) {
                map.flyTo([parseFloat(data[0].lat), parseFloat(data[0].lon)], 14);
              } else {
                alert('Lugar no encontrado en Colombia.');
              }
            });
        }

        if (searchBtn && searchInput) {
          L.DomEvent.on(searchBtn, 'click', executeSearch);
          L.DomEvent.on(searchInput, 'keypress', function(e) {
            if (e.keyCode === 13) executeSearch();
          });
        }
      }
    ")
    
    coor <- c(min(estaciones$LONGITUD), min(estaciones$LATITUD), max(estaciones$LONGITUD), max(estaciones$LATITUD))
    fitBounds(map, coor[1] - 0.03, coor[2] - 0.03, coor[3] + 0.03, coor[4] + 0.03)
  })
  
  observeEvent(input$RioPerfil, {
    req(input$RioPerfil)
    
    proxy1perfil <- leafletProxy("mapa_perfil")
    
    Estacionf <- input$RioPerfil
    estaciones <- unique(Perfiles_Datos[, c("NOMBRE", "AbcisaNombre", "CODIGO", "NOMBRE_ESTACION", "Abscisakm", "LONGITUD", "LATITUD", "NOMBRE_COMPLETO")])
    estaciones <- estaciones[estaciones$NOMBRE == Estacionf, ]
    coor <- c(min(estaciones$LONGITUD), min(estaciones$LATITUD), max(estaciones$LONGITUD), max(estaciones$LATITUD))
    
    shp <- st_zm(Perfilesshp, drop = TRUE, what = "ZM")
    selected_polygon <- shp[shp$NOMBRE == Estacionf, ]
    
    proxy1perfil %>% fitBounds(coor[1] - 0.03, coor[2] - 0.03, coor[3] + 0.03, coor[4] + 0.03)
    proxy1perfil %>% clearGroup("highlighted_polygonPerfiles")
    proxy1perfil %>% addPolylines(data = selected_polygon, group = "highlighted_polygonPerfiles", weight = 10,
                                  color = "black", options = pathOptions(pane = "principallineas"))
  })
  
  observe({
    req(Perfiles_Parametros$Parametros)
    
    updateSelectInput(
      session,
      inputId = "parametro_perfil",
      choices = Perfiles_Parametros$Parametros,
      selected = Perfiles_Parametros$Parametros[1]
    )
  })
  
  datos_perfil_filtrados <- reactive({
    req(input$RioPerfil, input$parametro_perfil)
    
    df <- Perfiles_Datos[Perfiles_Datos$NOMBRE == input$RioPerfil, ]
    req(nrow(df) > 0)
    
    req(input$parametro_perfil %in% colnames(df))
    
    df <- df[order(df$Abscisakm), ]
    return(df)
  })
  
  output$grafico_perfil_render <- renderPlotly({
    req(input$RioPerfil, input$parametro_perfil)
    
    Estacionf <- input$RioPerfil
    col <- input$parametro_perfil
    ejey <- input$log_scale_perfil
    
    estaciones <- Perfiles_Datos[Perfiles_Datos$NOMBRE == Estacionf, ]
    
    if (col == "ICA-5 [IDEAM]") {
      estaciones_sub <- unique(estaciones[, c("CODIGO", "NOMBRE_ESTACION", "NOMBRE", "Abscisakm", "AbcisaNombre", "NOMBRE_COMPLETO")])
      
      ICA_datosSub <- ICA_datos1[, c("Estacion", "Codigo", "Fecha", "ICA-5 [IDEAM]")]
      estaciones_sub <- merge(estaciones_sub, ICA_datosSub, by.x = "CODIGO", by.y = "Codigo", all.x = TRUE)
      estaciones_sub <- estaciones_sub[!is.na(estaciones_sub[[col]]), ]
      
      req(nrow(estaciones_sub) > 0)
      
      d <- estaciones_sub
      d$Categoria <- "Sin informacion"
      Cat <- CategoriasICA
      for (i in 1:nrow(d)) {
        if (!is.na(d$`ICA-5 [IDEAM]`[i])) {
          d$Categoria[i] <- Cat[Cat$Variable == "ICA IDEAM" & Cat$Vmin <= d$`ICA-5 [IDEAM]`[i] & Cat$Vmax > d$`ICA-5 [IDEAM]`[i], "Categoria"]
        }
      }
      d <- d[order(d[,"Estacion"], d[,"Fecha"]), ]
      CategoriasSub <- CategoriasICA[CategoriasICA$Variable == "ICA IDEAM", ]
      d <- merge(d, CategoriasSub[, c("Categoria", "CodigoColor", "ordenOriginal")], by = "Categoria")
      
      estacionesmed <- aggregate(estaciones_sub[, col, drop = FALSE], list(AbcisaNombre = estaciones_sub$AbcisaNombre, Abscisakm = estaciones_sub$Abscisakm, NOMBRE_ESTACION = estaciones_sub$NOMBRE_ESTACION), mean, na.rm = TRUE)
      estacionesmax <- aggregate(estaciones_sub[, col, drop = FALSE], list(AbcisaNombre = estaciones_sub$AbcisaNombre, Abscisakm = estaciones_sub$Abscisakm, NOMBRE_ESTACION = estaciones_sub$NOMBRE_ESTACION), max, na.rm = TRUE)
      estacionesmin <- aggregate(estaciones_sub[, col, drop = FALSE], list(AbcisaNombre = estaciones_sub$AbcisaNombre, Abscisakm = estaciones_sub$Abscisakm, NOMBRE_ESTACION = estaciones_sub$NOMBRE_ESTACION), min, na.rm = TRUE)
      
      estacionesmed <- estacionesmed[order(estacionesmed$Abscisakm), ]
      estacionesmax <- estacionesmax[order(estacionesmax$Abscisakm), ]
      estacionesmin <- estacionesmin[order(estacionesmin$Abscisakm), ]
      
      estacionesmed[, col] <- round(estacionesmed[, col], 3)
      estacionesmax[, col] <- round(estacionesmax[, col], 3)
      estacionesmin[, col] <- round(estacionesmin[, col], 3)
      
      CategoriasSub$CodigoColor1 <- colorspace::lighten(CategoriasSub$CodigoColor, amount = 0.7, method = "relative")
      icas <- sort(unique(CategoriasSub$ordenOriginal), decreasing = TRUE)
      
      fig <- plot_ly() %>%
        add_trace(x = c(min(estacionesmax$Abscisakm), max(estacionesmax$Abscisakm)), y = c(1, 1), type = 'scatter',
                  mode = 'lines', line = list(color = CategoriasSub[CategoriasSub$ordenOriginal == max(CategoriasSub$ordenOriginal), "CodigoColor1"], width = 0.5),
                  showlegend = FALSE)
      
      for (i in icas) {
        fig <- fig %>%
          add_trace(x = c(min(estacionesmax$Abscisakm), max(estacionesmax$Abscisakm)), 
                    y = c(CategoriasSub[CategoriasSub$ordenOriginal == i, "Vmin"], CategoriasSub[CategoriasSub$ordenOriginal == i, "Vmin"]),
                    type = 'scatter', mode = 'lines', alpha = 0.3, opacity = 0.3, fill = 'tonexty', 
                    fillcolor = CategoriasSub[CategoriasSub$ordenOriginal == i, "CodigoColor1"], 
                    line = list(color = CategoriasSub[CategoriasSub$ordenOriginal == i, "CodigoColor1"]),
                    name = paste0("Categoria ICA: ", CategoriasSub[CategoriasSub$ordenOriginal == i, "Categoria"]))
      }
      
      fig <- fig %>%
        add_trace(data = estacionesmax, x = ~Abscisakm, y = estacionesmax[[col]], name = "Maximo", type = 'scatter', mode = 'lines', line = list(color = "#314d7a", width = 1.5, dash = 'dash')) %>%
        add_trace(data = estacionesmed, x = ~Abscisakm, y = estacionesmed[[col]], name = "Medio", type = 'scatter', mode = 'lines', line = list(color = "black", width = 2, dash = 'dots')) %>%
        add_trace(data = estacionesmin, x = ~Abscisakm, y = estacionesmin[[col]], name = "Minimo", type = 'scatter', mode = 'lines', line = list(color = "#70120f", width = 1.5, dash = 'longdash'))
      
      icas_d <- sort(unique(d$ordenOriginal))
      for (i in icas_d) {
        est_cat <- d[d$ordenOriginal == i, ]
        fig <- fig %>%
          add_trace(data = est_cat, x = ~Abscisakm, y = est_cat[[col]], name = as.character(unique(est_cat$Categoria)), type = 'scatter', mode = 'markers', 
                    marker = list(size = 4, color = unique(est_cat$CodigoColor)),
                    text = ~paste("Nombre: ", NOMBRE_COMPLETO, '<br>',
                                  "Estacion: ", NOMBRE_ESTACION, '<br>',
                                  "Categoria: ", Categoria, '<br>',
                                  "Fecha: ", Fecha, '<br>',
                                  "Abscisa: ", Abscisakm, '<br>',
                                  "Corriente: ", NOMBRE))
      }
      
      fig <- fig %>%
        layout(xaxis = list(tickvals = estacionesmin$Abscisakm, ticktext = estacionesmin$AbcisaNombre, title = "", tickangle = 90, tickfont = list(size = 10)),
               yaxis = list(title = col),
               autosize = TRUE, 
               margin = list(l = 100, r = 50, b = 100, t = 100, pad = 4),
               title = paste0("Corriente: ", Estacionf, "<br> Parametro: ", col),
               font = list(size = 11))
      
      return(fig)
    }
    
    
    estaciones <- estaciones[!is.na(estaciones[[col]]), ]
    req(nrow(estaciones) > 0)
    
    idx_p <- match(col, Perfiles_Parametros$Parametros)
    franja <- if (!is.na(idx_p)) Perfiles_Parametros$Valor_optimo[idx_p] else "Menor concentracion"
    if (identical(franja, "Mayor concentracion")) {
      franjac <- c("#ebf2ec", "#ffe0e2")
    } else {
      franjac <- c("#ffe0e2", "#ebf2ec")
    }
    
    m <- list(l = 100, r = 50, b = 100, t = 100, pad = 4)
    
    estacionesmed <- aggregate(estaciones[, col, drop = FALSE], list(AbcisaNombre = estaciones$AbcisaNombre, Abscisakm = estaciones$Abscisakm, NOMBRE_ESTACION = estaciones$NOMBRE_ESTACION), mean, na.rm = TRUE)
    estacionesmax <- aggregate(estaciones[, col, drop = FALSE], list(AbcisaNombre = estaciones$AbcisaNombre, Abscisakm = estaciones$Abscisakm, NOMBRE_ESTACION = estaciones$NOMBRE_ESTACION), max, na.rm = TRUE)
    estacionesmin <- aggregate(estaciones[, col, drop = FALSE], list(AbcisaNombre = estaciones$AbcisaNombre, Abscisakm = estaciones$Abscisakm, NOMBRE_ESTACION = estaciones$NOMBRE_ESTACION), min, na.rm = TRUE)
    
    estacionesmed <- estacionesmed[order(estacionesmed$Abscisakm), ]
    estacionesmax <- estacionesmax[order(estacionesmax$Abscisakm), ]
    estacionesmin <- estacionesmin[order(estacionesmin$Abscisakm), ]
    
    estacionesmed[, col] <- round(estacionesmed[, col], 3)
    estacionesmax[, col] <- round(estacionesmax[, col], 3)
    estacionesmin[, col] <- round(estacionesmin[, col], 3)
    
    y_axis_config <- if (isTRUE(ejey)) list(title = col, type = "log") else list(title = col)
    
    fig <- plot_ly() %>%
      add_trace(data = estacionesmax, x = ~Abscisakm, y = estacionesmax[[col]], name = "Maximo", type = 'scatter', mode = 'lines', line = list(color = "#ed8087", width = 1.5, dash = 'dash')) %>%
      add_trace(data = estacionesmed, x = ~Abscisakm, y = estacionesmed[[col]], type = 'scatter', mode = 'lines',
                opacity = 0.5, fill = 'tonexty', fillcolor = franjac[1], line = list(color = franjac[1]), name = 'Franja Med-Max') %>%
      add_trace(data = estacionesmed, x = ~Abscisakm, y = estacionesmed[[col]], name = "Medio", type = 'scatter', mode = 'lines', line = list(color = "black", width = 1.5, dash = 'dots')) %>%
      add_trace(data = estacionesmin, x = ~Abscisakm, y = estacionesmin[[col]], type = 'scatter', mode = 'lines',
                opacity = 0.5, fill = 'tonexty', fillcolor = franjac[2], line = list(color = franjac[2]), name = 'Franja Min-Med') %>%
      add_trace(data = estacionesmin, x = ~Abscisakm, y = estacionesmin[[col]], name = "Minimo", type = 'scatter', mode = 'lines', line = list(color = "#3b8a43", width = 1.5, dash = 'longdash')) %>%
      add_trace(data = estaciones, x = ~Abscisakm, y = estaciones[[col]], name = "Datos", type = 'scatter', mode = 'markers', marker = list(size = 4, color = "#b5b8ba"),
                text = ~paste("Nombre: ", NOMBRE_COMPLETO, '<br>',
                              "Codigo: ", NOMBRE_ESTACION, '<br>',
                              "Fecha: ", Fecha, '<br>',
                              "Abscisa: ", Abscisakm, '<br>',
                              "Corriente: ", NOMBRE)) %>%
      layout(xaxis = list(tickvals = estacionesmin$Abscisakm, ticktext = estacionesmin$AbcisaNombre, title = "", tickangle = 90, tickfont = list(size = 10)),
             yaxis = y_axis_config,
             autosize = TRUE, margin = m,
             title = paste0("Corriente: ", Estacionf, "<br> Parametro: ", col),
             font = list(size = 11))
    
    EstacionesRio <- Perfilesshp[Perfilesshp$NOMBRE == Estacionf, c("NOMBRE", "Abscisakm", "ESTWQ")] %>% st_drop_geometry()
    PerfilesMetas1 <- PerfilesMetas[is.element(PerfilesMetas$ESTWQ, EstacionesRio$ESTWQ), c("ESTWQ", "Meta", "Abscisakm", "Tramo", "Nombre Tramo", col)]
    PerfilesMetas1 <- as.data.frame(PerfilesMetas1)
    
    azules <- c("#1022e8", "#1080e8", "#10d6e8", "#10e8b9")
    metas <- unique(PerfilesMetas1$Meta)
    
    if (length(metas) > 0) {
      for (t in 1:length(metas)) {
        PerfilesMetasm <- PerfilesMetas1[PerfilesMetas1$Meta == metas[t], ]
        PerfilesMetasm <- PerfilesMetasm[order(PerfilesMetasm$Tramo, PerfilesMetasm$Abscisakm), ]
        PerfilesMetasm1 <- PerfilesMetasm[seq(1, nrow(PerfilesMetasm), by = 2), ]
        PerfilesMetasm1$NombreTramoWrap <- stringr::str_wrap(PerfilesMetasm1$`Nombre Tramo`, width = 12)
        
        if (is.element(col, colnames(PerfilesMetasm))) {
          fig <- fig %>%
            add_trace(data = PerfilesMetasm, x = ~Abscisakm, y = PerfilesMetasm[[col]], name = metas[t], type = 'scatter', mode = 'lines', line = list(color = azules[t], width = 3, dash = 'solid')) %>%
            add_text(data = PerfilesMetasm1, x = ~Abscisakm, y = PerfilesMetasm1[[col]], name = paste0("Tramos ", metas[t]), text = ~NombreTramoWrap, textposition = 'top right',
                     marker = list(size = 1, color = azules[min(t + 5, length(azules))]),
                     textfont = list(size = 4))
        }
      }
    }
    
    fig
  })
  
  output$Perfil0 <- renderPlotly({
    req(input$RioPerfil)
    
    Estacionf <- input$RioPerfil
    col <- "ICA-5 [IDEAM]"
    
    estaciones <- Perfiles_Datos[Perfiles_Datos$NOMBRE == Estacionf, ]
    estaciones_sub <- unique(estaciones[, c("CODIGO", "NOMBRE_ESTACION", "NOMBRE", "Abscisakm", "AbcisaNombre", "NOMBRE_COMPLETO")])
    
    ICA_datosSub <- ICA_datos1[, c("Estacion", "Codigo", "Fecha", "ICA-5 [IDEAM]")]
    estaciones_sub <- merge(estaciones_sub, ICA_datosSub, by.x = "CODIGO", by.y = "Codigo", all.x = TRUE)
    estaciones_sub <- estaciones_sub[!is.na(estaciones_sub[[col]]), ]
    
    req(nrow(estaciones_sub) > 0)
    
    d <- estaciones_sub
    d$Categoria <- "Sin informacion"
    Cat <- CategoriasICA
    for (i in 1:nrow(d)) {
      if (!is.na(d$`ICA-5 [IDEAM]`[i])) {
        d$Categoria[i] <- Cat[Cat$Variable == "ICA IDEAM" & Cat$Vmin <= d$`ICA-5 [IDEAM]`[i] & Cat$Vmax > d$`ICA-5 [IDEAM]`[i], "Categoria"]
      }
    }
    d <- d[order(d[, "Estacion"], d[, "Fecha"]), ]
    CategoriasSub <- CategoriasICA[CategoriasICA$Variable == "ICA IDEAM", ]
    d <- merge(d, CategoriasSub[, c("Categoria", "CodigoColor", "ordenOriginal")], by = "Categoria")
    
    estacionesmed <- aggregate(estaciones_sub[, col, drop = FALSE], list(AbcisaNombre = estaciones_sub$AbcisaNombre, Abscisakm = estaciones_sub$Abscisakm, NOMBRE_ESTACION = estaciones_sub$NOMBRE_ESTACION), mean, na.rm = TRUE)
    estacionesmax <- aggregate(estaciones_sub[, col, drop = FALSE], list(AbcisaNombre = estaciones_sub$AbcisaNombre, Abscisakm = estaciones_sub$Abscisakm, NOMBRE_ESTACION = estaciones_sub$NOMBRE_ESTACION), max, na.rm = TRUE)
    estacionesmin <- aggregate(estaciones_sub[, col, drop = FALSE], list(AbcisaNombre = estaciones_sub$AbcisaNombre, Abscisakm = estaciones_sub$Abscisakm, NOMBRE_ESTACION = estaciones_sub$NOMBRE_ESTACION), min, na.rm = TRUE)
    
    estacionesmed <- estacionesmed[order(estacionesmed$Abscisakm), ]
    estacionesmax <- estacionesmax[order(estacionesmax$Abscisakm), ]
    estacionesmin <- estacionesmin[order(estacionesmin$Abscisakm), ]
    
    estacionesmed[, col] <- round(estacionesmed[, col], 3)
    estacionesmax[, col] <- round(estacionesmax[, col], 3)
    estacionesmin[, col] <- round(estacionesmin[, col], 3)
    
    CategoriasSub$CodigoColor1 <- colorspace::lighten(CategoriasSub$CodigoColor, amount = 0.7, method = "relative")
    icas <- sort(unique(CategoriasSub$ordenOriginal), decreasing = TRUE)
    
    fig <- plot_ly() %>%
      add_trace(x = c(min(estacionesmax$Abscisakm), max(estacionesmax$Abscisakm)), y = c(1, 1), type = 'scatter',
                mode = 'lines', line = list(color = CategoriasSub[CategoriasSub$ordenOriginal == max(CategoriasSub$ordenOriginal), "CodigoColor1"], width = 0.5),
                showlegend = FALSE)
    
    for (i in icas) {
      fig <- fig %>%
        add_trace(x = c(min(estacionesmax$Abscisakm), max(estacionesmax$Abscisakm)), 
                  y = c(CategoriasSub[CategoriasSub$ordenOriginal == i, "Vmin"], CategoriasSub[CategoriasSub$ordenOriginal == i, "Vmin"]),
                  type = 'scatter', mode = 'lines', alpha = 0.3, opacity = 0.3, fill = 'tonexty', 
                  fillcolor = CategoriasSub[CategoriasSub$ordenOriginal == i, "CodigoColor1"], 
                  line = list(color = CategoriasSub[CategoriasSub$ordenOriginal == i, "CodigoColor1"]),
                  name = paste0("Categoria ICA: ", CategoriasSub[CategoriasSub$ordenOriginal == i, "Categoria"]))
    }
    
    fig <- fig %>%
      add_trace(data = estacionesmax, x = ~Abscisakm, y = estacionesmax[[col]], name = "Maximo", type = 'scatter', mode = 'lines', line = list(color = "#314d7a", width = 1.5, dash = 'dash')) %>%
      add_trace(data = estacionesmed, x = ~Abscisakm, y = estacionesmed[[col]], name = "Medio", type = 'scatter', mode = 'lines', line = list(color = "black", width = 2, dash = 'dots')) %>%
      add_trace(data = estacionesmin, x = ~Abscisakm, y = estacionesmin[[col]], name = "Minimo", type = 'scatter', mode = 'lines', line = list(color = "#70120f", width = 1.5, dash = 'longdash'))
    
    icas_d <- sort(unique(d$ordenOriginal))
    for (i in icas_d) {
      est_cat <- d[d$ordenOriginal == i, ]
      fig <- fig %>%
        add_trace(data = est_cat, x = ~Abscisakm, y = est_cat[[col]], name = as.character(unique(est_cat$Categoria)), type = 'scatter', mode = 'markers', 
                  marker = list(size = 4, color = unique(est_cat$CodigoColor)),
                  text = ~paste("Nombre: ", NOMBRE_COMPLETO, '<br>',
                                "Estacion: ", NOMBRE_ESTACION, '<br>',
                                "Categoria: ", Categoria, '<br>',
                                "Fecha: ", Fecha, '<br>',
                                "Abscisa: ", Abscisakm, '<br>',
                                "Corriente: ", NOMBRE))
    }
    
    fig %>%
      layout(xaxis = list(tickvals = estacionesmin$Abscisakm, ticktext = estacionesmin$AbcisaNombre, title = "", tickangle = 90, tickfont = list(size = 10)),
             yaxis = list(title = col),
             autosize = TRUE, 
             margin = list(l = 100, r = 50, b = 100, t = 100, pad = 4),
             title = paste0("Corriente: ", Estacionf, "<br> Perfil ICA-5 [IDEAM]"),
             font = list(size = 11))
  })
  
  
  # ==============================================================================
  # Server - Sección de Metas de Calidad (Anteriores vs Actuales)
  # ==============================================================================
  
  output$mapa_metas_agua <- renderLeaflet({
    
    leaflet() %>%
      addMapPane("principallineas_metas", zIndex = 440) %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri.WorldImagery") %>%
      addTiles(group = "OpenStreetMap") %>%
      addProviderTiles(providers$Esri.NatGeoWorld, group = "Esri.NatGeoWorld") %>%
      addProviderTiles(providers$Esri.WorldPhysical, group = "Esri.WorldPhysical") %>%
      setView(lng = -75.7, lat = 4.8, zoom = 11) %>%
      leaflet.extras::addDrawToolbar(
        position = "topleft",
        targetGroup = "Herramientas de dibujo",
        polylineOptions = leaflet.extras::drawPolylineOptions(showLength = TRUE, metric = TRUE),
        polygonOptions = leaflet.extras::drawPolygonOptions(showArea = TRUE, metric = TRUE),
        circleOptions = leaflet.extras::drawCircleOptions(showRadius = TRUE, metric = TRUE),
        rectangleOptions = leaflet.extras::drawRectangleOptions(showArea = TRUE, metric = TRUE),
        markerOptions = leaflet.extras::drawMarkerOptions(),
        circleMarkerOptions = FALSE,
        editOptions = leaflet.extras::editToolbarOptions(selectedPathOptions = leaflet.extras::selectedPathOptions())
      ) %>%
      addLayersControl(
        position = "topright",
        baseGroups = c("Esri.WorldImagery", "OpenStreetMap", "Esri.NatGeoWorld", "Esri.WorldPhysical"),
        overlayGroups = c("Tramos Metas Calidad", "Herramientas de dibujo"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      addMiniMap(toggleDisplay = TRUE, position = "topright") %>%
      htmlwidgets::onRender("
    function(el, x) {
      var map = this;
      var searchControl = L.control({position: 'bottomleft'});
      searchControl.onAdd = function (map) {
        var div = L.DomUtil.create('div', 'leaflet-bar leaflet-control');
        div.style.backgroundColor = 'white'; div.style.padding = '3px'; div.style.display = 'flex'; div.style.alignItems = 'center';
        div.innerHTML = '<input type=\"text\" class=\"custom-search-input-metas\" placeholder=\"Buscar lugar en Colombia...\" style=\"width: 170px; padding: 2px 5px; border: 1px solid #ccc; outline: none; font-size: 12px; height: 24px;\">' +
                        '<button class=\"custom-search-btn-metas\" title=\"Buscar\" style=\"background: #fff; border: 1px solid #ccc; border-left: none; cursor: pointer; padding: 2px 6px; height: 24px;\">🔍</button>';
        L.DomEvent.disableClickPropagation(div);
        return div;
      };
      searchControl.addTo(map);

      var gpsControl = L.control({position: 'bottomright'});
      var svgTarget = '<svg width=\"16\" height=\"16\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"#444\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\"><circle cx=\"12\" cy=\"12\" r=\"10\"/><line x1=\"12\" y1=\"2\" x2=\"12\" y2=\"6\"/><line x1=\"12\" y1=\"18\" x2=\"12\" y2=\"22\"/><line x1=\"2\" y1=\"12\" x2=\"6\" y2=\"12\"/><line x1=\"18\" y1=\"12\" x2=\"22\" y2=\"22\"/><circle cx=\"12\" cy=\"12\" r=\"3\" fill=\"#444\"/></svg>';
      var svgSpinner = '<svg width=\"16\" height=\"16\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"#2563eb\" stroke-width=\"2.5\" stroke-linecap=\"round\" style=\"animation: spin 1s linear infinite;\"><style>@keyframes spin { 100% { transform: rotate(360deg); } }</style><path d=\"M21 12a9 9 0 1 1-6.219-8.56\"/></svg>';

      gpsControl.onAdd = function (map) {
        var div = L.DomUtil.create('div', 'leaflet-bar leaflet-control');
        div.style.backgroundColor = 'white'; div.style.padding = '0px';
        div.innerHTML = '<button class=\"gps-location-btn-metas\" title=\"Obtener mi ubicación\" style=\"background: #fff; border: none; cursor: pointer; height: 30px; width: 30px; display: flex; align-items: center; justify-content: center; outline: none;\">' + svgTarget + '</button>';
        L.DomEvent.disableClickPropagation(div);
        return div;
      };
      gpsControl.addTo(map);

      var gpsMarker = null, gpsCircle = null;
      var gpsBtn = el.querySelector('.gps-location-btn-metas');
      if (gpsBtn) {
        L.DomEvent.on(gpsBtn, 'click', function() {
          if (!navigator.geolocation) { alert('Geolocalización no soportada'); return; }
          gpsBtn.innerHTML = svgSpinner;
          navigator.geolocation.getCurrentPosition(
            function(pos) {
              gpsBtn.innerHTML = svgTarget;
              var lat = pos.coords.latitude, lng = pos.coords.longitude, acc = pos.coords.accuracy;
              if (gpsMarker) map.removeLayer(gpsMarker);
              if (gpsCircle) map.removeLayer(gpsCircle);
              map.flyTo([lat, lng], 15);
              gpsCircle = L.circle([lat, lng], { radius: acc, color: '#1d4ed8', fillColor: '#3b82f6', fillOpacity: 0.15, weight: 1.5 }).addTo(map);
              gpsMarker = L.marker([lat, lng]).addTo(map).bindPopup('<b>Ubicación del usuario</b><br>Precisión: ±' + Math.round(acc) + ' m').openPopup();
            },
            function(err) { gpsBtn.innerHTML = svgTarget; alert('Error al obtener la ubicación.'); },
            { enableHighAccuracy: true, timeout: 10000, maximumAge: 0 }
          );
        });
      }

      var searchInput = el.querySelector('.custom-search-input-metas');
      var searchBtn = el.querySelector('.custom-search-btn-metas');
      function executeSearch() {
        var query = searchInput.value;
        if (!query) return;
        fetch('https://nominatim.openstreetmap.org/search?format=json&countrycodes=co&q=' + encodeURIComponent(query))
          .then(function(res) { return res.json(); })
          .then(function(data) {
            if (data && data.length > 0) { map.flyTo([parseFloat(data[0].lat), parseFloat(data[0].lon)], 14); }
            else { alert('Lugar no encontrado en Colombia.'); }
          });
      }
      if (searchBtn && searchInput) {
        L.DomEvent.on(searchBtn, 'click', executeSearch);
        L.DomEvent.on(searchInput, 'keypress', function(e) { if (e.keyCode === 13) executeSearch(); });
      }
    }
    ")
  })
  
  observe({
    req(exists("parametros_calidad"), length(parametros_calidad) > 0)
    updateSelectInput(session, "sel_parametro_manual_metas", choices = parametros_calidad, selected = parametros_calidad[1])
  })
  
  anim_state_metas <- reactiveValues(
    running = FALSE,
    index = 1
  )
  
  auto_invalidate_metas <- reactiveTimer(1500)
  
  observe({
    if (anim_state_metas$running) {
      auto_invalidate_metas()
      isolate({
        n_params <- length(parametros_calidad)
        if (n_params > 0) {
          anim_state_metas$index <- ifelse(anim_state_metas$index >= n_params, 1, anim_state_metas$index + 1)
          updateSelectInput(session, "sel_parametro_manual_metas", selected = parametros_calidad[anim_state_metas$index])
        }
      })
    }
  })
  
  observeEvent(input$btn_play_param_metas, {
    anim_state_metas$running <- !anim_state_metas$running
    if (anim_state_metas$running) {
      updateActionButton(session, "btn_play_param_metas", label = " Pause", icon = icon("pause"))
    } else {
      updateActionButton(session, "btn_play_param_metas", label = " Play", icon = icon("play"))
    }
  })
  
  observeEvent(input$btn_next_param_metas, {
    anim_state_metas$running <- FALSE
    updateActionButton(session, "btn_play_param_metas", label = " Play", icon = icon("play"))
    n_params <- length(parametros_calidad)
    if (n_params > 0) {
      anim_state_metas$index <- ifelse(anim_state_metas$index >= n_params, 1, anim_state_metas$index + 1)
      updateSelectInput(session, "sel_parametro_manual_metas", selected = parametros_calidad[anim_state_metas$index])
    }
  })
  
  observeEvent(input$btn_prev_param_metas, {
    anim_state_metas$running <- FALSE
    updateActionButton(session, "btn_play_param_metas", label = " Play", icon = icon("play"))
    n_params <- length(parametros_calidad)
    if (n_params > 0) {
      anim_state_metas$index <- ifelse(anim_state_metas$index <= 1, n_params, anim_state_metas$index - 1)
      updateSelectInput(session, "sel_parametro_manual_metas", selected = parametros_calidad[anim_state_metas$index])
    }
  })
  
  observeEvent(input$sel_parametro_manual_metas, {
    match_idx <- match(input$sel_parametro_manual_metas, parametros_calidad)
    if (!is.na(match_idx)) {
      anim_state_metas$index <- match_idx
    }
  }, ignoreInit = TRUE)
  
  output$txt_parametro_activo_metas <- renderText({
    req(input$sel_parametro_manual_metas)
    input$sel_parametro_manual_metas
  })
  
  mapa_metas_listo <- reactiveVal(FALSE)
  mapa_metas_encuadrado <- reactiveVal(FALSE)
  
  observeEvent(input$mapa_metas_agua_bounds, {
    mapa_metas_listo(TRUE)
  }, once = TRUE)
  
  fmt_meta <- function(x) {
    vapply(x, function(v) {
      if (is.na(v)) "Sin meta" else format(signif(v, 6), scientific = FALSE, trim = TRUE, drop0trailing = TRUE)
    }, character(1))
  }
  
  datos_meta_mapa <- reactive({
    req(input$sel_parametro_manual_metas, input$sel_meta_horizonte_metas)
    vals <- metas_comparacion_long %>%
      filter(Parametro == input$sel_parametro_manual_metas,
             Horizonte == input$sel_meta_horizonte_metas) %>%
      select(llave, meta_actual, meta_anterior, diferencia)
    
    tramos_meta_shp %>%
      left_join(vals, by = "llave") %>%
      left_join(info_tramos_meta, by = "llave")
  })
  
  observe({
    req(mapa_metas_listo(), input$sel_version_metas)
    d       <- datos_meta_mapa()
    param   <- input$sel_parametro_manual_metas
    hor     <- input$sel_meta_horizonte_metas
    version <- input$sel_version_metas
    
    val <- switch(version,
                  actual     = d$meta_actual,
                  anterior   = d$meta_anterior,
                  diferencia = d$diferencia)
    
    todos <- c(d$meta_actual, d$meta_anterior)
    todos <- todos[!is.na(todos)]
    
    if (version == "diferencia") {
      validos <- val[!is.na(val)]
      lim <- if (length(validos) > 0) max(abs(validos)) else 1
      if (lim == 0) lim <- 1
      dominio <- c(-lim, lim)
      pal <- colorNumeric(c("#2c7bb6", "#f7f7f7", "#d7191c"), domain = dominio, na.color = "#808080")
      titulo_leyenda <- "Diferencia<br>(actual - anterior)"
    } else {
      dominio <- if (length(todos) > 0) range(todos) else c(0, 1)
      if (dominio[1] == dominio[2]) dominio[2] <- dominio[1] + 1
      pal <- colorNumeric("YlGnBu", domain = dominio, na.color = "#808080")
      titulo_leyenda <- if (version == "actual") "Meta actual" else "Meta anterior"
    }
    
    hay_valores <- any(!is.na(val))
    colores <- if (hay_valores) pal(val) else rep("#808080", nrow(d))
    colores[is.na(colores)] <- "#808080"
    
    etiquetas <- paste0(d$rio_meta, " | Tramo ", d$tramo_meta, " | ", param, ": ", fmt_meta(val))
    
    popups <- paste0(
      "<div style='font-family: sans-serif; min-width: 210px;'>",
      "<h5 style='margin-bottom:5px; color:#2c3e50;'><b>", d$rio_meta, " - Tramo ", d$tramo_meta, "</b></h5>",
      "<b>Uso Propuesto (actual): </b>", d$uso_meta, "<br>",
      "<b>Escenario: </b>", hor, "<br>",
      "<b>Estación: </b>", d$est_ini, " &rarr; ", d$est_fin, "<br>",
      "<hr style='margin: 5px 0;'>",
      "<b>Parámetro: </b>", param, "<br>",
      "<b style='color:#1080e8;'>Meta actual: </b>", fmt_meta(d$meta_actual), "<br>",
      "<b style='color:#6c757d;'>Meta anterior: </b>", fmt_meta(d$meta_anterior), "<br>",
      "<b>Diferencia: </b>", ifelse(is.na(d$diferencia), "-", fmt_meta(d$diferencia)),
      "</div>"
    )
    
    proxy <- leafletProxy("mapa_metas_agua") %>%
      clearGroup("Tramos Metas Calidad") %>%
      removeControl(layerId = "leyenda_metas") %>%
      addPolylines(
        data = d,
        group = "Tramos Metas Calidad",
        weight = 6,
        color = colores,
        opacity = 0.95,
        options = pathOptions(pane = "principallineas_metas"),
        label = etiquetas,
        popup = popups
      )
    
    if (hay_valores) {
      proxy %>% addLegend(
        position = "bottomright", pal = pal, values = dominio,
        title = paste0(param, "<br><small>", titulo_leyenda, " - ", hor, "</small>"),
        layerId = "leyenda_metas", opacity = 0.9
      )
    } else {
      proxy %>% addLegend(
        position = "bottomright", colors = "#808080", labels = "Sin meta definida",
        title = paste0(param, "<br><small>", titulo_leyenda, " - ", hor, "</small>"),
        layerId = "leyenda_metas", opacity = 0.9
      )
    }
    
    if (!isolate(mapa_metas_encuadrado())) {
      bb <- sf::st_bbox(tramos_meta_shp)
      proxy %>% fitBounds(as.numeric(bb["xmin"]), as.numeric(bb["ymin"]),
                          as.numeric(bb["xmax"]), as.numeric(bb["ymax"]))
      mapa_metas_encuadrado(TRUE)
    }
  })
  
  output$grafico_metas_tramos <- renderPlotly({
    req(input$sel_parametro_manual_metas, input$sel_rio_grafico_metas)
    param      <- input$sel_parametro_manual_metas
    cod        <- input$sel_rio_grafico_metas
    hors       <- intersect(horizontes_metas, input$chk_horizontes_metas)
    series     <- input$chk_series_metas
    nombre_rio <- names(rios_meta)[match(cod, rios_meta)]
    mostrar_metas <- length(hors) > 0 && length(series) > 0
    
    vacio <- function(txt) {
      plotly_empty(type = "scatter", mode = "markers") %>%
        layout(title = list(text = txt, font = list(color = "gray", size = 12)))
    }
    
    tr <- tabla_tramos_meta %>% filter(cod_rio == cod) %>% arrange(abs_ini)
    req(nrow(tr) > 0)
    n <- nrow(tr)
    
    est <- NULL
    col_hist <- NA_character_
    if (isTRUE(input$chk_hist_metas)) {
      nm_hist  <- names(Perfiles_Datos)
      col_busq <- if (param == "pH min") "pH" else param
      col_hist <- if (col_busq %in% nm_hist) col_busq else nm_hist[match(norm_txt(col_busq), norm_txt(nm_hist))]
      
      if (!is.na(col_hist)) {
        idx <- which(codigo_rio_meta(as.character(Perfiles_Datos$NOMBRE)) %in% cod)
        est <- data.frame(
          Abscisakm       = Perfiles_Datos$Abscisakm[idx],
          AbcisaNombre    = as.character(Perfiles_Datos$AbcisaNombre[idx]),
          NOMBRE_ESTACION = as.character(Perfiles_Datos$NOMBRE_ESTACION[idx]),
          NOMBRE_COMPLETO = as.character(Perfiles_Datos$NOMBRE_COMPLETO[idx]),
          NOMBRE          = as.character(Perfiles_Datos$NOMBRE[idx]),
          Fecha           = as.character(Perfiles_Datos$Fecha[idx]),
          valor           = suppressWarnings(as.numeric(as.character(Perfiles_Datos[[col_hist]][idx]))),
          stringsAsFactors = FALSE
        )
        est <- est[!is.na(est$valor) & !is.na(est$Abscisakm), ]
        if (nrow(est) == 0) est <- NULL
      }
    }
    hist_ok <- !is.null(est)
    
    d <- tr %>%
      inner_join(metas_comparacion_long %>% filter(Parametro == param), by = "llave") %>%
      filter(Horizonte %in% hors) %>%
      mutate(hover = paste0(
        "<b>", nombre_rio, " - Tramo ", tramo_meta, "</b><br>",
        "Uso: ", uso_meta, "<br>",
        "Horizonte: ", Horizonte, "<br>",
        "Km ", round(abs_ini, 1), " a ", round(abs_fin, 1), "<br>",
        "Meta actual: ", fmt_meta(meta_actual), "<br>",
        "Meta anterior: ", fmt_meta(meta_anterior)
      ))
    
    hay_metas <- mostrar_metas && nrow(d) > 0 && any(!is.na(d$meta_actual) | !is.na(d$meta_anterior))
    if (!hist_ok && !hay_metas) {
      return(vacio(paste0("Sin metas ni datos históricos para ", param, " en ", nombre_rio)))
    }
    
    fig <- plot_ly()
    
    if (hist_ok) {
      agg <- function(f) {
        a <- aggregate(valor ~ Abscisakm + AbcisaNombre, data = est, FUN = f)
        a <- a[order(a$Abscisakm), ]
        a$valor <- round(a$valor, 3)
        a
      }
      emed <- agg(mean); emax <- agg(max); emin <- agg(min)
      
      idx_p  <- match(col_hist, Perfiles_Parametros$Parametros)
      franja <- if (!is.na(idx_p)) Perfiles_Parametros$Valor_optimo[idx_p] else "Menor concentracion"
      franjac <- if (identical(franja, "Mayor concentracion")) c("#ebf2ec", "#ffe0e2") else c("#ffe0e2", "#ebf2ec")
      
      fig <- fig %>%
        add_trace(data = emax, x = ~Abscisakm, y = ~valor, name = "Maximo (histórico)", type = "scatter", mode = "lines",
                  line = list(color = "#ed8087", width = 1.5, dash = "dash")) %>%
        add_trace(data = emed, x = ~Abscisakm, y = ~valor, type = "scatter", mode = "lines",
                  opacity = 0.5, fill = "tonexty", fillcolor = franjac[1], line = list(color = franjac[1]),
                  name = "Franja Med-Max") %>%
        add_trace(data = emed, x = ~Abscisakm, y = ~valor, name = "Medio (histórico)", type = "scatter", mode = "lines",
                  line = list(color = "black", width = 1.5, dash = "dots")) %>%
        add_trace(data = emin, x = ~Abscisakm, y = ~valor, type = "scatter", mode = "lines",
                  opacity = 0.5, fill = "tonexty", fillcolor = franjac[2], line = list(color = franjac[2]),
                  name = "Franja Min-Med") %>%
        add_trace(data = emin, x = ~Abscisakm, y = ~valor, name = "Minimo (histórico)", type = "scatter", mode = "lines",
                  line = list(color = "#3b8a43", width = 1.5, dash = "longdash")) %>%
        add_trace(data = est, x = ~Abscisakm, y = ~valor, name = "Datos", type = "scatter", mode = "markers",
                  marker = list(size = 4, color = "#b5b8ba"),
                  text = ~paste("Nombre: ", NOMBRE_COMPLETO, "<br>",
                                "Codigo: ", NOMBRE_ESTACION, "<br>",
                                "Fecha: ", Fecha, "<br>",
                                "Abscisa: ", round(Abscisakm, 2), "<br>",
                                "Corriente: ", NOMBRE),
                  hoverinfo = "text")
    }
    
    escalon <- function(df, col) {
      df <- df[order(df$abs_ini), ]
      list(x   = as.vector(rbind(df$abs_ini, df$abs_fin)),
           y   = as.vector(rbind(df[[col]],  df[[col]])),
           txt = as.vector(rbind(df$hover,   df$hover)))
    }
    
    if (hay_metas) {
      for (h in hors) {
        dh  <- d[d$Horizonte == h, ]
        col <- colores_horizontes_metas[[h]]
        if ("anterior" %in% series) {
          e <- escalon(dh, "meta_anterior")
          if (any(!is.na(e$y))) {
            fig <- fig %>% add_trace(x = e$x, y = e$y, text = e$txt, hoverinfo = "text",
                                     type = "scatter", mode = "lines+markers",
                                     name = paste0("Anterior - ", h), legendgroup = h,
                                     line = list(color = col, width = 3, dash = "dash"),
                                     marker = list(size = 5, color = col, symbol = "circle-open"))
          }
        }
        if ("actual" %in% series) {
          e <- escalon(dh, "meta_actual")
          if (any(!is.na(e$y))) {
            fig <- fig %>% add_trace(x = e$x, y = e$y, text = e$txt, hoverinfo = "text",
                                     type = "scatter", mode = "lines+markers",
                                     name = paste0("Actual - ", h), legendgroup = h,
                                     line = list(color = col, width = 3, dash = "solid"),
                                     marker = list(size = 5, color = col))
          }
        }
      }
    }
    
    ticks <- data.frame(
      x   = c(tr$abs_ini, tr$abs_fin[n]),
      txt = paste0("Km ", round(c(tr$abs_ini, tr$abs_fin[n]), 1),
                   ifelse(is.na(c(tr$est_ini_tramo, tr$est_fin_tramo[n])), "",
                          paste0(" - ", c(tr$est_ini_tramo, tr$est_fin_tramo[n])))),
      stringsAsFactors = FALSE
    )
    if (hist_ok) {
      st <- unique(emin[, c("Abscisakm", "AbcisaNombre")])
      keep  <- vapply(ticks$x, function(v) all(abs(v - st$Abscisakm) > 0.3), logical(1))
      ticks <- rbind(ticks[keep, ], data.frame(x = st$Abscisakm, txt = st$AbcisaNombre, stringsAsFactors = FALSE))
    }
    ticks <- ticks[order(ticks$x), ]
    
    bandas <- lapply(seq_len(n), function(i) list(
      type = "rect", xref = "x", yref = "paper", layer = "below",
      x0 = tr$abs_ini[i], x1 = tr$abs_fin[i], y0 = 0, y1 = 1,
      fillcolor = if (i %% 2 == 1) "rgba(150,150,150,0.12)" else "rgba(150,150,150,0)",
      line = list(width = 0)
    ))
    rotulos <- lapply(seq_len(n), function(i) list(
      x = (tr$abs_ini[i] + tr$abs_fin[i]) / 2, y = 1, xref = "x", yref = "paper",
      yanchor = "bottom", showarrow = FALSE, font = list(size = 10),
      text = paste0("Tramo ", tr$tramo_meta[i])
    ))
    
    subtitulo <- if (isTRUE(input$chk_hist_metas) && !hist_ok) {
      "<br><sup>Este parámetro no tiene datos históricos en ERA_Perfiles_WQ_Depurados</sup>"
    } else ""
    
    fig %>% layout(
      title = paste0("Corriente: ", nombre_rio, "<br>Parametro: ", param,
                     if (hist_ok) " (histórico + metas anteriores vs actuales)" else " (metas anteriores vs actuales)",
                     subtitulo),
      xaxis = list(tickvals = ticks$x, ticktext = ticks$txt, title = "",
                   tickangle = 90, tickfont = list(size = 10), zeroline = FALSE),
      yaxis = list(title = param,
                   type = if (isTRUE(input$chk_log_metas)) "log" else "linear",
                   rangemode = if (isTRUE(input$chk_log_metas)) "normal" else "tozero"),
      shapes = bandas,
      annotations = rotulos,
      hovermode = "closest",
      autosize = TRUE,
      margin = list(l = 100, r = 50, b = 130, t = 120, pad = 4),
      font = list(size = 11)
    )
  })
  
  
  # ==============================================================================
  # SERVER - MODELO DE CALIDAD: ANÁLISIS DE TRAMOS (ANTERIORES vs ACTUALES)
  # ==============================================================================
  mapa_tramos_listo <- reactiveVal(FALSE)
  observeEvent(input$mapa_analisis_tramos_bounds, { mapa_tramos_listo(TRUE) }, once = TRUE)
  
  colores_tramo <- c("1" = "#8e1a6b", "2" = "#8fd0f5", "3" = "#5df58f", "4" = "#d4f55d", "5" = "#e8140f")
  
  output$mapa_analisis_tramos <- renderLeaflet({
    col_ant <- unname(colores_tramo[as.character(tramos_ant_shp$tramo_n)]); col_ant[is.na(col_ant)] <- "#808080"
    col_act <- unname(colores_tramo[as.character(tramos_act_shp$tramo_shp)]); col_act[is.na(col_act)] <- "#808080"
    
    ta <- tramos_ant_shp
    ta$popup <- paste0(
      "<div style='min-width:240px'><b>", lbl_rio(ta$cod_rio), " - Tramo ", ta$tramo_n, " (anterior)</b><br>",
      "CodRio: ", ta$codrio, " | Longitud: ", sprintf("%.2f", ta$long_km), " km<br>",
      "<b>Uso:</b> ", html_esc(ta$uso_ant),
      "<hr style='margin:5px 0'><b>Estaci\u00f3n (ESTWQ):</b><br>", fmt_est(ta$est_ant), "</div>")
    
    tc <- tramos_act_shp
    tc$popup <- paste0(
      "<div style='min-width:260px'><b>", lbl_rio(tc$cod_rio), " - Tramo ", tc$tramo_shp, " (actual)</b><br>",
      html_esc(tc$desc_seg), "<br>",
      "CodRio: ", tc$codrio, " | Longitud: ", sprintf("%.2f", tc$long_km2), " km",
      ifelse(tc$cambio_tramo,
             paste0("<div style='background:#fff3cd;padding:3px 6px;margin-top:4px'>\u26a0 Cambi\u00f3 del <b>Tramo ",
                    tc$tramo_ant, "</b> al <b>Tramo ", tc$tramo_shp, "</b></div>"), ""),
      "<hr style='margin:5px 0'><b>Estaci\u00f3n inicio:</b><br>", fmt_est(tc$est_ini),
      "<br><b>Estaci\u00f3n final:</b><br>", fmt_est(tc$est_fin),
      "<br><b>Estaci\u00f3n QW:</b><br>", fmt_est(tc$est_qw), "</div>")
    
    bb <- sf::st_bbox(c(sf::st_geometry(ta), sf::st_geometry(tc)))
    
    m <- leaflet() %>%
      addMapPane("pane_cambios_tramos", zIndex = 405) %>%
      addMapPane("pane_tramos_ant", zIndex = 410) %>%
      addMapPane("pane_tramos_act", zIndex = 411) %>%
      addMapPane("pane_puntos_tramos", zIndex = 450) %>%
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Esri.WorldTopoMap") %>%
      addTiles(group = "OpenStreetMap") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri.WorldImagery") %>%
      fitBounds(as.numeric(bb["xmin"]), as.numeric(bb["ymin"]), as.numeric(bb["xmax"]), as.numeric(bb["ymax"])) %>%
      addPolylines(data = ta, group = "Tramos anteriores", color = col_ant, weight = 6, opacity = 0.95,
                   options = pathOptions(pane = "pane_tramos_ant"),
                   label = paste0(lbl_rio(ta$cod_rio), " - Tramo ", ta$tramo_n, " (anterior)"), popup = ta$popup) %>%
      addPolylines(data = tc, group = "Tramos actuales", color = col_act, weight = 6, opacity = 0.95,
                   options = pathOptions(pane = "pane_tramos_act"),
                   label = paste0(lbl_rio(tc$cod_rio), " - Tramo ", tc$tramo_shp, " (actual)"), popup = tc$popup)
    
    seg_c <- tc[tc$cambio_tramo, ]
    if (nrow(seg_c) > 0) {
      m <- m %>% addPolylines(data = seg_c, group = "segmentos_cambio", color = "#ffd400", weight = 16, opacity = 0.6,
                              options = pathOptions(pane = "pane_cambios_tramos"), popup = seg_c$popup)
    }
    pts_o <- puntos_est[!puntos_est$cambio, ]
    pts_c <- puntos_est[puntos_est$cambio, ]
    if (nrow(pts_o) > 0) {
      m <- m %>% addCircleMarkers(data = pts_o, group = "puntos_otros", radius = 5, weight = 1.5, color = "#333333",
                                  fillColor = "#9e9e9e", fillOpacity = 0.95,
                                  options = pathOptions(pane = "pane_puntos_tramos"),
                                  label = lapply(pts_o$etiqueta_mapa, htmltools::HTML), popup = ~popup)
    }
    if (nrow(pts_c) > 0) {
      m <- m %>% addCircleMarkers(data = pts_c, group = "puntos_cambio", radius = 9, weight = 2.5, color = "#000000",
                                  fillColor = "#00e5ff", fillOpacity = 1,
                                  options = pathOptions(pane = "pane_puntos_tramos"),
                                  label = lapply(pts_c$etiqueta_mapa, htmltools::HTML),
                                  labelOptions = labelOptions(noHide = TRUE, direction = "right", textsize = "11px",
                                                              style = list("white-space" = "normal", "width" = "max-content",
                                                                           "max-width" = "230px")),
                                  popup = ~popup)
    }
    
    m %>%
      addLayersControl(position = "topright",
                       baseGroups = c("Esri.WorldTopoMap", "OpenStreetMap", "Esri.WorldImagery"),
                       options = layersControlOptions(collapsed = TRUE)) %>%
      addLegend(position = "bottomright", title = "Tramo", opacity = 1,
                colors = unname(colores_tramo), labels = paste("Tramo", names(colores_tramo))) %>%
      addLegend(position = "bottomleft", title = "Estaciones y cambios", opacity = 1,
                colors = c("#00e5ff", "#9e9e9e", "#ffd400"),
                labels = c("Estaci\u00f3n con cambio identificado", "Otras estaciones", "Segmento con cambio de tramo")) %>%
      addScaleBar(position = "bottomleft") %>%
      htmlwidgets::onRender("
        function(el, x) {
          var map = this;
          var pAnt = map.getPane('pane_tramos_ant');
          var pAct = map.getPane('pane_tramos_act');
          if (!pAnt || !pAct) { return; }
          var container = map.getContainer();
          var frac = 0.5;

          var divider = document.createElement('div');
          divider.style.cssText = 'position:absolute;top:0;bottom:0;width:4px;margin-left:-2px;background:#ffffff;box-shadow:0 0 4px rgba(0,0,0,0.6);z-index:900;pointer-events:none;';
          var handle = document.createElement('div');
          handle.innerHTML = '&#8596;';
          handle.style.cssText = 'position:absolute;top:50%;left:50%;width:34px;height:34px;margin:-17px 0 0 -17px;border-radius:50%;background:#ffffff;box-shadow:0 0 6px rgba(0,0,0,0.7);text-align:center;line-height:34px;font-size:20px;cursor:ew-resize;pointer-events:auto;';
          var lblA = document.createElement('div');
          lblA.innerHTML = '&#9664; Anterior';
          lblA.style.cssText = 'position:absolute;top:10px;right:14px;white-space:nowrap;background:rgba(255,255,255,0.9);padding:2px 8px;border-radius:4px;font:bold 12px sans-serif;box-shadow:0 0 3px rgba(0,0,0,0.5);';
          var lblB = document.createElement('div');
          lblB.innerHTML = 'Actual &#9654;';
          lblB.style.cssText = 'position:absolute;top:10px;left:14px;white-space:nowrap;background:rgba(255,255,255,0.9);padding:2px 8px;border-radius:4px;font:bold 12px sans-serif;box-shadow:0 0 3px rgba(0,0,0,0.5);';
          divider.appendChild(handle); divider.appendChild(lblA); divider.appendChild(lblB);
          container.appendChild(divider);
          L.DomEvent.disableClickPropagation(divider);

          function update() {
            var size = map.getSize();
            var nw = map.containerPointToLayerPoint([0, 0]);
            var se = map.containerPointToLayerPoint(size);
            var clipX = nw.x + (se.x - nw.x) * frac;
            pAnt.style.clip = 'rect(' + nw.y + 'px,' + clipX + 'px,' + se.y + 'px,' + nw.x + 'px)';
            pAct.style.clip = 'rect(' + nw.y + 'px,' + se.x + 'px,' + se.y + 'px,' + clipX + 'px)';
            divider.style.left = (size.x * frac) + 'px';
          }
          map.on('move', update); map.on('zoom', update); map.on('resize', update); map.on('viewreset', update);
          update();

          var dragging = false;
          function pos(e) { return (e.touches && e.touches.length) ? e.touches[0].clientX : e.clientX; }
          function start(e) { dragging = true; map.dragging.disable(); e.stopPropagation(); e.preventDefault(); }
          function move(e) {
            if (!dragging) { return; }
            var r = container.getBoundingClientRect();
            frac = Math.min(0.98, Math.max(0.02, (pos(e) - r.left) / r.width));
            update();
          }
          function stop() { if (dragging) { dragging = false; map.dragging.enable(); } }
          handle.addEventListener('mousedown', start);
          handle.addEventListener('touchstart', start);
          document.addEventListener('mousemove', move);
          document.addEventListener('touchmove', move);
          document.addEventListener('mouseup', stop);
          document.addEventListener('touchend', stop);
        }
      ")
  })
  
  # Mostrar / ocultar capas
  observe({
    req(mapa_tramos_listo())
    proxy <- leafletProxy("mapa_analisis_tramos")
    if (isTRUE(input$chk_puntos_tramos)) {
      proxy %>% showGroup("puntos_cambio")
      if (isTRUE(input$chk_solo_cambio_tramos)) proxy %>% hideGroup("puntos_otros") else proxy %>% showGroup("puntos_otros")
    } else {
      proxy %>% hideGroup("puntos_cambio") %>% hideGroup("puntos_otros")
    }
    if (isTRUE(input$chk_resaltar_tramos)) proxy %>% showGroup("segmentos_cambio") else proxy %>% hideGroup("segmentos_cambio")
  })
  
  # Encuadre por corriente
  observeEvent(input$sel_rio_tramos, {
    req(mapa_tramos_listo(), input$sel_rio_tramos)
    if (input$sel_rio_tramos == "ALL") {
      g <- c(sf::st_geometry(tramos_ant_shp), sf::st_geometry(tramos_act_shp))
    } else {
      g <- c(sf::st_geometry(tramos_ant_shp)[which(tramos_ant_shp$cod_rio == input$sel_rio_tramos)],
             sf::st_geometry(tramos_act_shp)[which(tramos_act_shp$cod_rio == input$sel_rio_tramos)])
    }
    req(length(g) > 0)
    bb <- sf::st_bbox(g)
    leafletProxy("mapa_analisis_tramos") %>%
      flyToBounds(as.numeric(bb["xmin"]), as.numeric(bb["ymin"]), as.numeric(bb["xmax"]), as.numeric(bb["ymax"]))
  }, ignoreInit = TRUE)
  
  # Gráfico: longitud de cada tramo, anterior vs actual
  output$grafico_tramos_distancia <- renderPlotly({
    d <- comp_tramos
    fig <- plot_ly(source = "graf_tramos_dist") %>%
      add_bars(x = d$etiqueta, y = d$km_ant, name = "Tramos anteriores",
               marker = list(color = "#8c9aa8"),
               text = sprintf("%.2f", d$km_ant), textposition = "outside",
               hovertext = d$hover_ant, hoverinfo = "text") %>%
      add_bars(x = d$etiqueta, y = d$km_act, name = "Tramos actuales",
               marker = list(color = "#1080e8"),
               text = sprintf("%.2f (%+.2f)", d$km_act, d$dif_km), textposition = "outside",
               hovertext = d$hover_act, hoverinfo = "text") %>%
      layout(
        barmode = "group",
        title = "Longitud de los tramos (km): anteriores vs actuales",
        xaxis = list(title = "", categoryorder = "array", categoryarray = d$etiqueta),
        yaxis = list(title = "Longitud (km)", rangemode = "tozero"),
        legend = list(orientation = "h", x = 0, y = -0.15),
        hoverlabel = list(align = "left"),
        margin = list(t = 60, b = 80)
      )
    event_register(fig, "plotly_click")
  })
  
  # Clic en una barra: ubica el tramo en el mapa
  observeEvent(event_data("plotly_click", source = "graf_tramos_dist"), {
    ev <- event_data("plotly_click", source = "graf_tramos_dist")
    req(!is.null(ev), mapa_tramos_listo())
    k <- match(as.character(ev$x[1]), comp_tramos$etiqueta)
    req(!is.na(k))
    cr <- comp_tramos$cod_rio[k]; tr <- comp_tramos$tramo[k]
    seg_n <- tramos_act_shp[which(tramos_act_shp$cod_rio == cr & tramos_act_shp$tramo_shp == tr), ]
    seg_a <- tramos_ant_shp[which(tramos_ant_shp$cod_rio == cr & tramos_ant_shp$tramo_n == tr), ]
    g <- c(sf::st_geometry(seg_n), sf::st_geometry(seg_a))
    req(length(g) > 0)
    bb <- sf::st_bbox(g)
    proxy <- leafletProxy("mapa_analisis_tramos") %>% clearGroup("resaltado_tramo")
    if (nrow(seg_n) > 0) {
      proxy <- proxy %>% addPolylines(data = seg_n, group = "resaltado_tramo", color = "#000000", weight = 12, opacity = 0.35,
                                      options = pathOptions(pane = "pane_cambios_tramos", interactive = FALSE))
    }
    proxy %>% flyToBounds(as.numeric(bb["xmin"]), as.numeric(bb["ymin"]), as.numeric(bb["xmax"]), as.numeric(bb["ymax"]))
  })
  
  output$tabla_cambios_tramos <- DT::renderDT({
    DT::datatable(tabla_cambios_tramos, rownames = FALSE,
                  options = list(pageLength = 8, scrollX = TRUE, dom = "tip"))
  })
  

  # ==============================================================================
  # SERVER - MODELO DE CALIDAD: MAPA DE CALIDAD DEL AGUA (METAS vs QUAL2K)
  # ==============================================================================
  mapa_calidad_listo <- reactiveVal(FALSE)
  observeEvent(input$mapa_modelo_calidad_agua_bounds, { mapa_calidad_listo(TRUE) }, once = TRUE)
  
  etiquetas_estado <- function(sentido, tol_pct) {
    tol_txt <- paste0(" (\u00b1", tol_pct, "%)")
    e <- switch(sentido,
      max   = c(Incumple = "Incumple: simulado > meta",
                Igual    = paste0("Igual a la meta", tol_txt),
                Cumple   = "Cumple: simulado < meta"),
      min   = c(Incumple = "Incumple: simulado < meta",
                Igual    = paste0("Igual a la meta", tol_txt),
                Cumple   = "Cumple: simulado > meta"),
      rango = c(Incumple = "Incumple: fuera del rango",
                Igual    = paste0("En el l\u00edmite del rango", tol_txt),
                Cumple   = "Cumple: dentro del rango"))
    c(e, "Sin meta" = "Sin meta definida")
  }
  
  # ==============================================================================
  # Server Objetivos de Calidad: Identificación de usos del agua (Power BI embebido)
  # ==============================================================================
  url_powerbi_usos <- "https://app.powerbi.com/view?r=eyJrIjoiZThhNTI3NDktN2NkNS00YzhiLWEyZmMtZTRmNjJlOGNmYWFjIiwidCI6IjNkZmUxMjVkLWE1ODktNDRhZi05ZmM3LTExM2YzYmZmNTg3MyIsImMiOjh9"
  
  output$powerbi_usos <- renderUI({
    tagList(
      tags$iframe(
        title = "Identificación de usos del agua por tramo - Power BI",
        src = url_powerbi_usos,
        width = "100%", frameborder = "0", allowfullscreen = "true",
        style = "width:100%; height:78vh; min-height:600px; border:0; display:block;"
      ),
      tags$div(
        class = "small text-secondary p-2",
        "Si el tablero no se visualiza, ",
        tags$a(href = url_powerbi_usos, target = "_blank", rel = "noopener", "ábralo en una pestaña nueva"), "."
      )
    )
  })
  
  # ==============================================================================
  # Server Modelo de Calidad: Mapa de tramos con vertimientos y abstracciones
  # ==============================================================================
  pts_elem <- puntos_est
  pts_elem$cod_rio     <- ifelse(pts_elem$corriente == "Río Otún", "RO", "Q2Q")
  pts_elem$campana_etq <- ifelse(is.na(pts_elem$campana) | !nzchar(trimws(pts_elem$campana)), "Sin campaña", pts_elem$campana)
  forma_de_tipo <- function(tipo_etq) {
    k <- match(tipo_etq, simbologia_puntos$etiqueta)
    ifelse(is.na(k), "circulo", simbologia_puntos$forma[k])
  }
  paleta_tramos_elem <- function(n) {
    base <- c("#8e44ad", "#2b8cbe", "#2ca25f", "#f1c40f", "#d7301f")
    if (n <= length(base)) base[seq_len(n)] else grDevices::colorRampPalette(base)(n)
  }
  col_rio_elem <- c(RO = "#1f78b4", Q2Q = "#e6550d")
  
  caja_leyenda <- function(titulo, filas) {
    paste0("<div style='background:rgba(255,255,255,0.92);padding:6px 10px;border-radius:4px;box-shadow:0 0 6px rgba(0,0,0,0.4);",
           "font:12px sans-serif;line-height:22px'><b>", titulo, "</b>", paste(filas, collapse = ""), "</div>")
  }
  fila_linea <- function(col, txt) {
    paste0("<div><span style='display:inline-block;width:24px;height:5px;background:", col,
           ";margin-right:6px;vertical-align:middle;border-radius:2px'></span>", txt, "</div>")
  }
  fila_icono <- function(url, txt) {
    paste0("<div><img src=\"", url, "\" width='16' height='16' style='vertical-align:middle;margin-right:6px'>", txt, "</div>")
  }
  
  output$mapa_tramos_elementos <- renderLeaflet({
    bb <- sf::st_bbox(tramos_act_shp)
    leaflet() %>%
      addMapPane("pane_tramos_elem", zIndex = 410) %>%
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Esri.WorldTopoMap") %>%
      addTiles(group = "OpenStreetMap") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri.WorldImagery") %>%
      fitBounds(as.numeric(bb["xmin"]), as.numeric(bb["ymin"]), as.numeric(bb["xmax"]), as.numeric(bb["ymax"])) %>%
      agregar_quebradas("pane_quebradas_elem") %>%
      addLayersControl(position = "topright",
                       baseGroups = c("Esri.WorldTopoMap", "OpenStreetMap", "Esri.WorldImagery"),
                       overlayGroups = c("Tramos", "Estaciones y elementos", grupos_quebradas),
                       options = layersControlOptions(collapsed = TRUE)) %>%
      addScaleBar(position = "bottomleft")
  })
  
  mapa_elem_listo <- reactiveVal(FALSE)
  observeEvent(input$mapa_tramos_elementos_bounds, { mapa_elem_listo(TRUE) }, once = TRUE)
  
  tramos_elem_sel <- reactive({
    corr <- input$sel_corr_elem
    segs <- tramos_act_shp
    if (!is.null(corr) && corr != "ALL") segs <- segs[!is.na(segs$cod_rio) & segs$cod_rio == corr, ]
    segs
  })
  
  # Tramos: simbología según la columna elegida
  observe({
    req(mapa_elem_listo(), input$sel_simb_tramos)
    segs <- tramos_elem_sel()
    modo <- input$sel_simb_tramos
    prox <- leafletProxy("mapa_tramos_elementos") %>%
      clearGroup("Tramos") %>% removeControl(layerId = "leyenda_tramos_elem")
    if (nrow(segs) == 0) return(invisible(NULL))
    
    if (modo == "tramo") {
      clase  <- as.character(segs$tramo_shp)
      niv    <- as.character(sort(unique(segs$tramo_shp)))
      etq    <- paste("Tramo", niv)
      pal    <- paleta_tramos_elem(length(niv))
      titulo <- "Tramo"
    } else if (modo == "rio") {
      clase  <- segs$cod_rio
      niv    <- intersect(c("RO", "Q2Q"), unique(clase))
      etq    <- lbl_rio(niv)
      pal    <- unname(col_rio_elem[niv])
      titulo <- "Río"
    } else {
      clase  <- rep("uno", nrow(segs))
      niv    <- "uno"
      etq    <- "Tramos del modelo"
      pal    <- "#37474f"
      titulo <- "Tramos"
    }
    col <- unname(stats::setNames(pal, niv)[clase])
    col[is.na(col)] <- "#9e9e9e"
    km <- vapply(niv, function(v) sum(segs$long_km2[which(clase == v)], na.rm = TRUE), numeric(1))
    filas <- mapply(function(cl, e, k) fila_linea(cl, paste0(e, " (", sprintf("%.1f", k), " km)")), pal, etq, km)
    
    pop <- paste0(
      "<div style='min-width:260px'><b>", lbl_rio(segs$cod_rio), " - Tramo ", segs$tramo_shp, "</b><br>",
      "<b>Segmento:</b> ", html_esc(segs$desc_seg), "<br>",
      "<b>Inicio:</b> ", fmt_est(segs$est_ini), "<br>",
      "<b>Fin:</b> ", fmt_est(segs$est_fin), "<br>",
      "<b>Longitud:</b> ", sprintf("%.2f", segs$long_km2), " km</div>")
    lbl <- paste0(lbl_rio(segs$cod_rio), " - Tramo ", segs$tramo_shp)
    
    prox %>%
      addPolylines(data = segs, group = "Tramos", color = col, weight = 5, opacity = 1,
                   label = lbl, popup = pop,
                   highlightOptions = highlightOptions(weight = 8, bringToFront = FALSE),
                   options = pathOptions(pane = "pane_tramos_elem")) %>%
      addControl(html = caja_leyenda(titulo, filas), position = "bottomright", layerId = "leyenda_tramos_elem")
  })
  
  # Estaciones y elementos: simbología según la columna elegida
  observe({
    req(mapa_elem_listo(), input$sel_simb_puntos)
    modo <- input$sel_simb_puntos
    p <- pts_elem
    corr <- input$sel_corr_elem
    if (!is.null(corr) && corr != "ALL") p <- p[p$cod_rio == corr, ]
    p <- p[p$tipo_etq %in% input$chk_tipos_elem, ]
    prox <- leafletProxy("mapa_tramos_elementos") %>%
      clearGroup("Estaciones y elementos") %>% removeControl(layerId = "leyenda_puntos_elem")
    if (nrow(p) == 0) return(invisible(NULL))
    
    if (modo == "campana") {
      niv <- sort(unique(p$campana_etq))
      pal <- c("#1f78b4", "#e6550d", "#33a02c", "#6a3d9a", "#b15928", "#999999")[seq_along(niv)]
      colp <- stats::setNames(pal, niv)[p$campana_etq]
      p$icono_mapa <- vapply(seq_len(nrow(p)), function(i) icono_svg(forma_de_tipo(p$tipo_etq[i]), unname(colp[i])), character(1))
      filas <- c(
        mapply(function(cl, e) fila_icono(icono_svg("circulo", cl, 16), paste0(e, " (", sum(p$campana_etq == e), ")")), pal, niv),
        "<div style='margin-top:4px'><i>Forma según tipo:</i></div>",
        vapply(unique(p$tipo_etq), function(t) fila_icono(icono_svg(forma_de_tipo(t), "#9e9e9e", 16), t), character(1)))
      titulo <- "Campaña"
    } else {
      p$icono_mapa <- p$icono
      tipos <- unique(p$tipo_etq)
      tipos <- c(simbologia_puntos$etiqueta[simbologia_puntos$etiqueta %in% tipos], setdiff(tipos, simbologia_puntos$etiqueta))
      filas <- vapply(tipos, function(t) {
        k <- match(t, simbologia_puntos$etiqueta)
        url <- if (is.na(k)) icono_svg("circulo", "#9e9e9e", 16) else icono_svg(simbologia_puntos$forma[k], simbologia_puntos$color[k], 16)
        fila_icono(url, paste0(t, " (", sum(p$tipo_etq == t), ")"))
      }, character(1))
      titulo <- "Estaciones y elementos"
    }
    
    prox %>%
      addMarkers(data = p, group = "Estaciones y elementos",
                 icon = icons(iconUrl = p$icono_mapa, iconWidth = 20, iconHeight = 20, iconAnchorX = 10, iconAnchorY = 10),
                 label = lapply(p$etiqueta_nombre, htmltools::HTML),
                 labelOptions = opciones_lbl_estacion, popup = p$popup_simple) %>%
      addControl(html = caja_leyenda(titulo, filas), position = "bottomleft", layerId = "leyenda_puntos_elem")
  })
  
  # Al cambiar de corriente, el mapa se ajusta a los tramos seleccionados
  observeEvent(input$sel_corr_elem, {
    req(mapa_elem_listo())
    segs <- tramos_elem_sel()
    req(nrow(segs) > 0)
    bb <- sf::st_bbox(segs)
    leafletProxy("mapa_tramos_elementos") %>%
      fitBounds(as.numeric(bb["xmin"]), as.numeric(bb["ymin"]), as.numeric(bb["xmax"]), as.numeric(bb["ymax"]))
  }, ignoreInit = TRUE)
  
  # ==============================================================================
  # Server Modelo de Calidad: Diagrama y topología del modelo (HTML embebido)
  # ==============================================================================
  output$diagrama_topologia <- renderUI({
    if (is.null(html_topologia)) {
      return(div(class = "alert alert-warning",
                 "No se encontró el archivo 'Top_calidad_ROtun.html' en la carpeta de la aplicación."))
    }
    tags$iframe(
      title = "Diagrama y topología del modelo de calidad del agua",
      srcdoc = html_topologia,
      width = "100%", frameborder = "0", allowfullscreen = "true",
      style = "width:100%; height:85vh; min-height:700px; border:0; display:block; background:white;"
    )
  })

  # ==============================================================================
  # Server Situación Actual: caudales mensuales por tramo (promedio vs seca)
  # ==============================================================================
  output$msg_hidro <- renderUI({
    if (!hid_ok) {
      return(div(class = "alert alert-warning",
                 "No se pudieron cargar los caudales: verifique que 'analisis_hidrologico.xlsx' (hojas Promedio y Seca) ",
                 "esté en la carpeta de la aplicación y que sus códigos SUP existan en PUNTOS_OTUN / PUNTOS_Q2Q."))
    }
    if (length(hid_sin_ubicar)) {
      div(class = "small text-secondary mb-2",
          "Estaciones del archivo sin ubicación en PUNTOS_OTUN / PUNTOS_Q2Q (no se muestran): ",
          paste(hid_sin_ubicar, collapse = ", "), ".")
    }
  })

  observeEvent(input$sel_rio_hid, {
    req(hid_ok)
    e  <- est_hid[!is.na(est_hid$tramo) & est_hid$cod_rio == input$sel_rio_hid, ]
    tr <- sort(unique(e$tramo))
    ch <- stats::setNames(as.character(tr), paste0("Tramo ", tr, " (", vapply(tr, function(t) sum(e$tramo == t), numeric(1)), ifelse(vapply(tr, function(t) sum(e$tramo == t), numeric(1)) == 1, " estación)", " estaciones)")))
    updateSelectInput(session, "sel_tramo_hid", choices = ch, selected = if (length(ch)) ch[[1]] else NULL)
  })

  anim_hid <- reactiveValues(running = FALSE, index = 1)
  auto_hid <- reactiveTimer(1500)

  observe({
    if (anim_hid$running) {
      auto_hid()
      isolate({
        anim_hid$index <- if (anim_hid$index >= 12) 1 else anim_hid$index + 1
        updateSelectInput(session, "sel_mes_hid", selected = meses_hid[anim_hid$index])
      })
    }
  })
  observeEvent(input$btn_play_mes_hid, {
    anim_hid$running <- !anim_hid$running
    if (anim_hid$running) updateActionButton(session, "btn_play_mes_hid", label = " Pause", icon = icon("pause"))
    else updateActionButton(session, "btn_play_mes_hid", label = " Play", icon = icon("play"))
  })
  observeEvent(input$btn_next_mes_hid, {
    anim_hid$running <- FALSE
    updateActionButton(session, "btn_play_mes_hid", label = " Play", icon = icon("play"))
    anim_hid$index <- if (anim_hid$index >= 12) 1 else anim_hid$index + 1
    updateSelectInput(session, "sel_mes_hid", selected = meses_hid[anim_hid$index])
  })
  observeEvent(input$btn_prev_mes_hid, {
    anim_hid$running <- FALSE
    updateActionButton(session, "btn_play_mes_hid", label = " Play", icon = icon("play"))
    anim_hid$index <- if (anim_hid$index <= 1) 12 else anim_hid$index - 1
    updateSelectInput(session, "sel_mes_hid", selected = meses_hid[anim_hid$index])
  })
  observeEvent(input$sel_mes_hid, {
    i <- match(input$sel_mes_hid, meses_hid)
    if (!is.na(i)) anim_hid$index <- i
  }, ignoreInit = TRUE)

  mes_hid_idx <- reactive({
    i <- match(input$sel_mes_hid, meses_hid)
    if (is.na(i)) 1L else i
  })

  # Estaciones del tramo seleccionado (índices dentro de est_hid)
  hid_sel <- reactive({
    req(hid_ok, input$sel_rio_hid, input$sel_tramo_hid)
    tr <- suppressWarnings(as.integer(input$sel_tramo_hid))
    k  <- which(est_hid$cod_rio == input$sel_rio_hid & !is.na(est_hid$tramo) & est_hid$tramo == tr)
    validate(need(length(k) > 0, "El tramo seleccionado no tiene estaciones con datos."))
    k
  })

  hid_segs <- reactive({
    tr <- suppressWarnings(as.integer(input$sel_tramo_hid))
    list(
      rio   = tramos_act_shp[!is.na(tramos_act_shp$cod_rio) & tramos_act_shp$cod_rio == input$sel_rio_hid, ],
      tramo = tramos_act_shp[!is.na(tramos_act_shp$cod_rio) & tramos_act_shp$cod_rio == input$sel_rio_hid &
                               !is.na(tramos_act_shp$tramo_shp) & tramos_act_shp$tramo_shp == tr, ]
    )
  })

  sync_js <- function(otro) paste0("
    function(el, x) {
      var map = this;
      map.on('move', function() {
        if (window.__syncHid) return;
        window.__syncHid = true;
        try {
          var o = HTMLWidgets.find('#", otro, "');
          if (o && o.getMap) o.getMap().setView(map.getCenter(), map.getZoom(), {animate: false});
        } catch (e) {}
        window.__syncHid = false;
      });
    }")

  mapa_base_hid <- function(otro) {
    bb <- sf::st_bbox(tramos_act_shp)
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Esri.WorldTopoMap") %>%
      addTiles(group = "OpenStreetMap") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri.WorldImagery") %>%
      fitBounds(as.numeric(bb["xmin"]), as.numeric(bb["ymin"]), as.numeric(bb["xmax"]), as.numeric(bb["ymax"])) %>%
      addLayersControl(position = "topright",
                       baseGroups = c("Esri.WorldTopoMap", "OpenStreetMap", "Esri.WorldImagery"),
                       options = layersControlOptions(collapsed = TRUE)) %>%
      addScaleBar(position = "bottomleft") %>%
      htmlwidgets::onRender(sync_js(otro))
  }
  mapa_base_hid2 <- function(otro) {
    mapa_base_hid(otro) %>%
      addMapPane("pane_drenajes_hid", zIndex = 410) %>%
      addMapPane("pane_est_hid", zIndex = 450)
  }
  output$mapa_hid_promedio <- renderLeaflet(mapa_base_hid2("mapa_hid_seca"))
  output$mapa_hid_seca     <- renderLeaflet(mapa_base_hid2("mapa_hid_promedio"))

  mapa_hid_prom_listo <- reactiveVal(FALSE)
  mapa_hid_seca_listo <- reactiveVal(FALSE)
  observeEvent(input$mapa_hid_promedio_bounds, { mapa_hid_prom_listo(TRUE) }, once = TRUE)
  observeEvent(input$mapa_hid_seca_bounds,     { mapa_hid_seca_listo(TRUE) }, once = TRUE)

  dibujar_hid <- function(mapa_id, mat, titulo) {
    k <- hid_sel(); m <- mes_hid_idx()
    e <- est_hid[k, ]
    v  <- mat[k, m]
    vp <- est_prom[k, m]; vs <- est_seca[k, m]
    rng <- suppressWarnings(range(c(est_prom[k, ], est_seca[k, ]), na.rm = TRUE))
    if (!all(is.finite(rng))) rng <- c(0, 1)
    if (rng[2] <= rng[1]) rng <- c(0, max(rng[2], 1e-6))
    pal <- leaflet::colorNumeric("YlGnBu", domain = rng)
    rad <- 7 + 20 * sqrt(pmax(v, 0) / rng[2]); rad[is.na(rad)] <- 6
    col <- ifelse(is.na(v), "#bdbdbd", pal(v))
    fm  <- function(x) ifelse(is.na(x), "-", formatC(x, format = "f", digits = 3))
    pct <- ifelse(!is.na(vp) & !is.na(vs) & vp > 0, paste0(" (", sprintf("%.0f", 100 * vs / vp), "% del promedio)"), "")
    dren <- ifelse(is.na(e$drenaje), "-", paste0(html_esc(e$drenaje), " <i>(a ", sprintf("%.0f", e$drenaje_m), " m)</i>"))
    pop <- paste0("<div style='min-width:240px'><b>", html_esc(e$desc), "</b> (", html_esc(e$codigo), ")<br>",
                  "<b>Quebrada:</b> ", dren, "<br>",
                  lbl_rio(e$cod_rio), " - Tramo ", e$tramo, " | Km ", html_esc(e$km), "<hr style='margin:5px 0'>",
                  "<b>", meses_hid[m], "</b><br>",
                  "Caudal medio (Promedio): ", fm(vp), " m³/s<br>",
                  "Caudal bajo (Seca): ", fm(vs), " m³/s", pct, "</div>")
    lab <- paste0(ifelse(is.na(e$drenaje), e$desc, e$drenaje), ": ", fm(v), " m³/s")
    prox <- leafletProxy(mapa_id) %>%
      clearGroup("Drenajes") %>% clearGroup("Estaciones") %>% clearControls()
    if (!is.null(drenajes_shp) && isTRUE(input$chk_drenajes_hid)) {
      prox <- prox %>% addPolylines(data = drenajes_shp, group = "Drenajes", color = "#4a90c2", weight = 1.5, opacity = 0.75,
                                    label = ifelse(is.na(drenajes_shp$nombre_geo), "Quebrada sin nombre", drenajes_shp$nombre_geo),
                                    highlightOptions = highlightOptions(weight = 4, color = "#0b5394", bringToFront = FALSE),
                                    options = pathOptions(pane = "pane_drenajes_hid"))
    }
    prox %>%
      addCircleMarkers(lng = e$lon, lat = e$lat, group = "Estaciones", radius = rad,
                       fillColor = col, fillOpacity = 0.9, color = "#222222", weight = 1.5,
                       label = lab, popup = pop, options = pathOptions(pane = "pane_est_hid")) %>%
      addLegend(position = "bottomright", pal = pal, values = rng, title = "Caudal (m³/s)", opacity = 1) %>%
      addControl(html = paste0("<div style='background:rgba(255,255,255,0.93);padding:5px 10px;border-radius:4px;box-shadow:0 0 6px rgba(0,0,0,0.4);",
                               "font:13px sans-serif;line-height:20px'><b>", titulo, "</b><br>", meses_hid[m], "</div>"),
                 position = "topleft", layerId = "titulo_hid")
  }
  observe({
    req(hid_ok, mapa_hid_prom_listo())
    dibujar_hid("mapa_hid_promedio", est_prom, "Caudales medios (Promedio)")
  })
  observe({
    req(hid_ok, mapa_hid_seca_listo())
    dibujar_hid("mapa_hid_seca", est_seca, "Caudales bajos (Seca)")
  })

  # Al cambiar de corriente o tramo, ambos mapas se encuadran en el tramo
  observeEvent(list(input$sel_rio_hid, input$sel_tramo_hid), {
    req(hid_ok, mapa_hid_prom_listo(), mapa_hid_seca_listo())
    k <- hid_sel(); sg <- hid_segs()
    bb <- if (nrow(sg$tramo)) sf::st_bbox(sg$tramo) else sf::st_bbox(sg$rio)
    x <- c(as.numeric(bb["xmin"]), as.numeric(bb["xmax"]), est_hid$lon[k])
    y <- c(as.numeric(bb["ymin"]), as.numeric(bb["ymax"]), est_hid$lat[k])
    x <- x[is.finite(x)]; y <- y[is.finite(y)]
    for (id in c("mapa_hid_promedio", "mapa_hid_seca")) {
      leafletProxy(id) %>% fitBounds(min(x), min(y), max(x), max(y), options = list(padding = c(30, 30)))
    }
  }, ignoreInit = TRUE)

  output$resumen_hid <- renderUI({
    req(hid_ok)
    k <- hid_sel(); m <- mes_hid_idx()
    tp <- sum(est_prom[k, m], na.rm = TRUE); ts <- sum(est_seca[k, m], na.rm = TRUE)
    caja <- function(titulo, valor, extra = NULL) div(class = "col-md-3 col-6",
      div(class = "border rounded p-2 text-center bg-light",
          div(class = "small text-secondary", titulo), div(class = "fs-5 fw-bold", valor), extra))
    div(class = "row g-2 mb-3",
        caja("Estaciones del tramo", length(k)),
        caja(paste0("Aporte total - Promedio (", meses_hid[m], ")"), paste0(formatC(tp, format = "f", digits = 3), " m³/s")),
        caja(paste0("Aporte total - Seca (", meses_hid[m], ")"), paste0(formatC(ts, format = "f", digits = 3), " m³/s")),
        caja("Seca respecto al promedio", if (tp > 0) paste0(sprintf("%.0f", 100 * ts / tp), " %") else "-"))
  })

  output$grafico_hid_mensual <- renderPlotly({
    req(hid_ok)
    k <- hid_sel(); m <- mes_hid_idx()
    tp <- colSums(est_prom[k, , drop = FALSE], na.rm = TRUE)
    ts <- colSums(est_seca[k, , drop = FALSE], na.rm = TRUE)
    cols <- grDevices::hcl.colors(max(length(k), 3), "Dark 3")
    fig <- plot_ly()
    for (i in seq_along(k)) {
      j <- k[i]; nm <- est_hid$desc[j]
      fig <- fig %>%
        add_trace(x = meses_hid, y = est_prom[j, ], type = "scatter", mode = "lines", name = nm, legendgroup = nm,
                  line = list(color = cols[i], width = 1.5), opacity = 0.75,
                  hovertemplate = paste0(nm, " (Promedio): %{y:.3f} m³/s<extra></extra>")) %>%
        add_trace(x = meses_hid, y = est_seca[j, ], type = "scatter", mode = "lines", name = nm, legendgroup = nm,
                  showlegend = FALSE, line = list(color = cols[i], width = 1.5, dash = "dot"), opacity = 0.75,
                  hovertemplate = paste0(nm, " (Seca): %{y:.3f} m³/s<extra></extra>"))
    }
    fig %>%
      add_trace(x = meses_hid, y = tp, type = "scatter", mode = "lines+markers", name = "Total tributarios - Promedio",
                line = list(color = "#000000", width = 3.5), marker = list(color = "#000000", size = 6),
                hovertemplate = "Total (Promedio): %{y:.3f} m³/s<extra></extra>") %>%
      add_trace(x = meses_hid, y = ts, type = "scatter", mode = "lines+markers", name = "Total tributarios - Seca",
                line = list(color = "#000000", width = 3.5, dash = "dot"), marker = list(color = "#000000", size = 6, symbol = "diamond"),
                hovertemplate = "Total (Seca): %{y:.3f} m³/s<extra></extra>") %>%
      layout(
        title = paste0(lbl_rio(input$sel_rio_hid), " - Tramo ", input$sel_tramo_hid, ": caudales mensuales de las estaciones de aporte"),
        xaxis = list(title = "", categoryorder = "array", categoryarray = meses_hid),
        yaxis = list(title = "Caudal (m³/s)", rangemode = "tozero"),
        shapes = list(list(type = "line", xref = "x", yref = "paper", x0 = m - 1, x1 = m - 1, y0 = 0, y1 = 1,
                           line = list(color = "#d73027", width = 2, dash = "dash"))),
        hovermode = "x unified", legend = list(orientation = "h", x = 0, y = -0.2),
        margin = list(l = 70, r = 30, b = 90, t = 60), font = list(size = 11))
  })

  # ==============================================================================
  # Server Situación Actual: perfil comparativo de metas por escenario (clima medio vs seco)
  # ==============================================================================
  output$msg_esc <- renderUI({
    if (!esc_ok) {
      div(class = "alert alert-warning",
          "No se encontraron los dos escenarios Qual2k en la carpeta de la aplicación: ",
          "'Escenario_actual-Clima_medio_promedio.xlsx' y 'Escenario_actual-_Clima_seco_bajo.xlsx'.")
    }
  })

  datos_perfil_esc <- reactive({
    validate(need(esc_ok, "Faltan los archivos de los escenarios Qual2k."))
    req(input$sel_var_esc, input$sel_hor_esc)
    mp  <- mapa_esc[mapa_esc$etiqueta == input$sel_var_esc, ][1, ]
    tol_pct <- input$num_tol_esc
    if (is.null(tol_pct) || is.na(tol_pct)) tol_pct <- 5
    tol <- max(tol_pct, 0) / 100
    d <- q2k_seg[, c("id_porh", "llave", "cod_rio", "tramo_shp", "est_ini", "est_fin", "abscisa_ini", "abscisa_fin")]
    d$sim_m <- valor_esc(esc_q2k$medio, ix_esc$medio, mp$etiqueta, mp$col_esc)
    d$sim_s <- valor_esc(esc_q2k$seco,  ix_esc$seco,  mp$etiqueta, mp$col_esc)
    hi <- rep(NA_real_, nrow(d)); lo <- rep(NA_real_, nrow(d))
    if (!is.na(mp$meta)) {
      meta_de <- function(par) {
        mm <- metas_comparacion_long[metas_comparacion_long$Parametro == par & metas_comparacion_long$Horizonte == input$sel_hor_esc,
                                     c("llave", "meta_actual")]
        mm[!duplicated(mm$llave), ]
      }
      m1 <- meta_de(mp$meta); v <- m1$meta_actual[match(d$llave, m1$llave)]
      if (mp$sentido == "max") hi <- v
      if (mp$sentido == "min") lo <- v
      if (mp$sentido == "rango") { hi <- v; m2 <- meta_de("pH min"); lo <- m2$meta_actual[match(d$llave, m2$llave)] }
    }
    d$hi <- hi; d$lo <- lo
    d$ref <- if (mp$sentido == "min") lo else hi
    d$est_m <- estado_vec(d$sim_m, lo, hi, tol)
    d$est_s <- estado_vec(d$sim_s, lo, hi, tol)
    list(d = d, mp = mp, hor = input$sel_hor_esc, tol_pct = tol_pct)
  })

  output$grafico_perfil_escenarios <- renderPlotly({
    req(input$sel_rio_esc)
    r <- datos_perfil_esc(); mp <- r$mp; p <- mp$etiqueta; d0 <- r$d
    cod <- input$sel_rio_esc; nombre_rio <- lbl_rio(cod)
    con_meta <- !identical(mp$sentido, "ninguno")
    rango <- identical(mp$sentido, "rango")
    lab <- if (con_meta) etiquetas_estado(mp$sentido, r$tol_pct) else NULL

    da <- d0[which(d0$cod_rio == cod), ]
    da <- da[order(da$abscisa_ini), ]
    req(nrow(da) > 0)

    tr <- tabla_tramos_meta %>% filter(cod_rio == cod) %>% arrange(abs_ini)
    tm <- d0[!duplicated(d0$llave), c("llave", "hi", "lo")]
    tr$hi <- tm$hi[match(tr$llave, tm$llave)]
    tr$lo <- tm$lo[match(tr$llave, tm$llave)]
    n <- nrow(tr)

    escalon <- function(x0, x1, y) list(x = as.vector(rbind(x0, x1, NA)), y = as.vector(rbind(y, y, NA)))
    fig <- plot_ly()
    if (con_meta && any(!is.na(tr$hi))) {
      e <- escalon(tr$abs_ini, tr$abs_fin, tr$hi)
      fig <- fig %>% add_trace(x = e$x, y = e$y, type = "scatter", mode = "lines", hoverinfo = "skip",
                               name = if (rango) "Meta actual - máximo" else paste0("Meta actual (", r$hor, ")"),
                               line = list(color = "#000000", width = 3))
    }
    if (con_meta && any(!is.na(tr$lo))) {
      e <- escalon(tr$abs_ini, tr$abs_fin, tr$lo)
      fig <- fig %>% add_trace(x = e$x, y = e$y, type = "scatter", mode = "lines", hoverinfo = "skip",
                               name = if (rango) "Meta actual - mínimo" else paste0("Meta actual (", r$hor, ")"),
                               line = list(color = "#000000", width = 3, dash = if (rango) "dash" else "solid"))
    }

    meta_txt <- if (!con_meta) "-" else if (rango) paste0(fmt_meta(da$lo), " a ", fmt_meta(da$hi)) else fmt_meta(da$ref)
    base_hov <- paste0("<b>", nombre_rio, " - Tramo ", da$tramo_shp, "</b> (ID-PORH ", da$id_porh, ")<br>",
                       da$est_ini, " &rarr; ", da$est_fin, "<br>",
                       "Km ", round(da$abscisa_ini, 2), " a ", round(da$abscisa_fin, 2), "<br>",
                       "Clima medio: ", fmt_meta(da$sim_m), if (con_meta) paste0(" (", da$est_m, ")") else "", "<br>",
                       "Clima seco: ",  fmt_meta(da$sim_s), if (con_meta) paste0(" (", da$est_s, ")") else "", "<br>",
                       "Meta: ", meta_txt)
    series <- list(
      list(nm = "Clima medio (promedio)", v = da$sim_m, est = da$est_m, col = "#1f78b4", sym = "circle"),
      list(nm = "Clima seco (bajo)",      v = da$sim_s, est = da$est_s, col = "#e6550d", sym = "diamond"))
    for (s in series) {
      ok <- which(!is.na(s$v))
      if (!length(ok)) next
      x0 <- da$abscisa_ini[ok]; x1 <- da$abscisa_fin[ok]; y <- s$v[ok]
      fig <- fig %>% add_trace(x = as.vector(rbind(x0, x1, NA)), y = as.vector(rbind(y, y, NA)),
                               type = "scatter", mode = "lines", hoverinfo = "skip", name = s$nm, legendgroup = s$nm,
                               line = list(color = s$col, width = 4))
      mcol <- if (con_meta) unname(col_estado[s$est[ok]]) else s$col
      fig <- fig %>% add_trace(x = (x0 + x1) / 2, y = y, type = "scatter", mode = "markers",
                               text = base_hov[ok], hoverinfo = "text", legendgroup = s$nm, showlegend = FALSE,
                               marker = list(size = 11, symbol = s$sym, color = mcol, line = list(color = s$col, width = 2.5)))
    }
    if (con_meta) {
      for (est in c("Cumple", "Igual", "Incumple")) {
        fig <- fig %>% add_trace(x = NA_real_, y = NA_real_, type = "scatter", mode = "markers", name = lab[[est]],
                                 hoverinfo = "skip", marker = list(size = 11, color = col_estado[[est]], line = list(color = "#555555", width = 1)))
      }
    }

    tx <- c(da$abscisa_ini, max(da$abscisa_fin))
    tl <- c(da$est_ini, da$est_fin[which.max(da$abscisa_fin)])
    kp <- logical(length(tx)); ultimo <- -Inf
    for (i in seq_along(tx)) if (tx[i] - ultimo > 0.3) { kp[i] <- TRUE; ultimo <- tx[i] }
    bandas <- lapply(seq_len(n), function(i) list(
      type = "rect", xref = "x", yref = "paper", layer = "below",
      x0 = tr$abs_ini[i], x1 = tr$abs_fin[i], y0 = 0, y1 = 1,
      fillcolor = if (i %% 2 == 1) "rgba(150,150,150,0.12)" else "rgba(150,150,150,0)", line = list(width = 0)))
    rotulos <- lapply(seq_len(n), function(i) list(
      x = (tr$abs_ini[i] + tr$abs_fin[i]) / 2, y = 1, xref = "x", yref = "paper",
      yanchor = "bottom", showarrow = FALSE, font = list(size = 10), text = paste0("Tramo ", tr$tramo_meta[i])))

    fig %>% layout(
      title = paste0("Corriente: ", nombre_rio, "<br>Parámetro: ", p,
                     if (con_meta) paste0(" - metas actuales (", r$hor, ") vs escenarios Qual2k") else " - escenarios Qual2k"),
      xaxis = list(tickvals = tx[kp], ticktext = paste0("Km ", round(tx[kp], 1), " - ", tl[kp]), title = "",
                   tickangle = 90, tickfont = list(size = 10), zeroline = FALSE),
      yaxis = list(title = p, rangemode = if (identical(p, "pH")) "normal" else "tozero"),
      shapes = bandas, annotations = rotulos, hovermode = "closest", autosize = TRUE,
      legend = list(orientation = "h", x = 0, y = -0.5),
      margin = list(l = 90, r = 40, b = 160, t = 110, pad = 4), font = list(size = 11))
  })

  output$tabla_perfil_escenarios <- DT::renderDT({
    r <- datos_perfil_esc(); d <- r$d; mp <- r$mp
    d <- d[order(match(d$cod_rio, c("RO", "Q2Q")), d$abscisa_ini), ]
    con_meta <- !identical(mp$sentido, "ninguno")
    meta_col <- if (!con_meta) rep("-", nrow(d)) else if (identical(mp$sentido, "rango")) {
      ifelse(is.na(d$hi), "-", ifelse(is.na(d$lo), as.character(round(d$hi, 2)), paste0(round(d$lo, 2), " a ", round(d$hi, 2))))
    } else round(d$ref, 3)
    t <- data.frame(
      `Corriente` = lbl_rio(d$cod_rio), `Tramo` = d$tramo_shp, `ID-PORH` = d$id_porh,
      `Estación inicio` = d$est_ini, `Estación final` = d$est_fin,
      `Clima medio` = round(d$sim_m, 3), `Clima seco` = round(d$sim_s, 3),
      `Diferencia (seco - medio)` = round(d$sim_s - d$sim_m, 3),
      `Meta Actual` = meta_col,
      `Estado clima medio` = d$est_m, `Estado clima seco` = d$est_s,
      check.names = FALSE, stringsAsFactors = FALSE)
    if (!con_meta) t <- t[, !(names(t) %in% c("Estado clima medio", "Estado clima seco", "Meta Actual")), drop = FALSE]
    dt <- DT::datatable(t, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE, dom = "tip"))
    if (con_meta) {
      for (cn in c("Estado clima medio", "Estado clima seco"))
        dt <- DT::formatStyle(dt, cn, backgroundColor = DT::styleEqual(names(col_estado), unname(col_estado)), fontWeight = "bold")
    }
    dt
  })

  # ==============================================================================
  # Server Simulación de Escenarios (Power BI embebido)
  # ==============================================================================
  url_powerbi_simulacion <- "https://app.powerbi.com/view?r=eyJrIjoiZTRhM2M1ZTEtOGY3NS00NDQwLTliMjctNGNjNzkzYTAwMWU0IiwidCI6IjNkZmUxMjVkLWE1ODktNDRhZi05ZmM3LTExM2YzYmZmNTg3MyIsImMiOjh9&pageName=31233a6a86dbd2097275"

  output$powerbi_simulacion <- renderUI({
    tagList(
      tags$iframe(
        title = "Simulación de escenarios - Power BI",
        src = url_powerbi_simulacion,
        width = "100%", frameborder = "0", allowfullscreen = "true",
        style = "width:100%; height:78vh; min-height:600px; border:0; display:block;"
      ),
      tags$div(
        class = "small text-secondary p-2",
        "Si el tablero no se visualiza, ",
        tags$a(href = url_powerbi_simulacion, target = "_blank", rel = "noopener", "ábralo en una pestaña nueva"), "."
      )
    )
  })

  # ==============================================================================
  # Server Situación Actual: mapas de cumplimiento de metas por escenario Qual2k
  # ==============================================================================
  mapa_base_esc <- function(otro) {
    mapa_base_hid(otro) %>%
      addMapPane("pane_estado_esc", zIndex = 420) %>%
      addMapPane("pane_estado_esc2", zIndex = 421) %>%
      addMapPane("pane_est_esc", zIndex = 450) %>%
      addMarkers(data = puntos_est, group = "Estaciones",
                 icon = icons(iconUrl = puntos_est$icono, iconWidth = 20, iconHeight = 20, iconAnchorX = 10, iconAnchorY = 10),
                 label = ~paste0(codigo, " - ", tipo_etq), popup = ~popup,
                 options = markerOptions(pane = "pane_est_esc")) %>%
      addLayersControl(position = "topright",
                       baseGroups = c("Esri.WorldTopoMap", "OpenStreetMap", "Esri.WorldImagery"),
                       overlayGroups = c("Subtramos", "Estaciones"),
                       options = layersControlOptions(collapsed = TRUE))
  }
  output$mapa_esc_medio <- renderLeaflet(mapa_base_esc("mapa_esc_seco"))
  output$mapa_esc_seco  <- renderLeaflet(mapa_base_esc("mapa_esc_medio"))

  mapa_esc_medio_listo <- reactiveVal(FALSE)
  mapa_esc_seco_listo  <- reactiveVal(FALSE)
  observeEvent(input$mapa_esc_medio_bounds, { mapa_esc_medio_listo(TRUE) }, once = TRUE)
  observeEvent(input$mapa_esc_seco_bounds,  { mapa_esc_seco_listo(TRUE) }, once = TRUE)

  dibujar_esc <- function(mapa_id, cual) {
    r <- datos_perfil_esc(); d <- r$d; mp <- r$mp
    sim <- if (cual == "m") d$sim_m else d$sim_s
    est <- if (cual == "m") d$est_m else d$est_s
    otro_sim <- if (cual == "m") d$sim_s else d$sim_m
    titulo <- if (cual == "m") "Clima medio (promedio)" else "Clima seco (bajo)"
    con_meta <- !identical(mp$sentido, "ninguno")
    rango <- identical(mp$sentido, "rango")
    segs <- tramos_q2k_shp

    meta_txt <- if (!con_meta) "-" else if (rango) paste0(fmt_meta(d$lo), " a ", fmt_meta(d$hi)) else fmt_meta(d$ref)
    pop <- paste0(
      "<div style='min-width:260px'><b>", lbl_rio(d$cod_rio), " - Tramo ", d$tramo_shp, "</b> (ID-PORH ", d$id_porh, ")<br>",
      "<b>Subtramo:</b><br>", fmt_est(d$est_ini), "<br>&darr;<br>", fmt_est(d$est_fin),
      "<hr style='margin:5px 0'>",
      "<b>Parámetro:</b> ", html_esc(mp$etiqueta), "<br>",
      "<b>", titulo, ":</b> ", fmt_meta(sim), if (con_meta) paste0(" (", est, ")") else "", "<br>",
      "<b>", if (cual == "m") "Clima seco" else "Clima medio", ":</b> ", fmt_meta(otro_sim), "<br>",
      "<b>Meta actual (", r$hor, "):</b> ", meta_txt, "</div>")
    lbls <- paste0(lbl_rio(d$cod_rio), " - Tramo ", d$tramo_shp, " | ", mp$etiqueta, ": ", fmt_meta(sim),
                   if (con_meta) paste0(" (", est, ")") else "")

    prox <- leafletProxy(mapa_id) %>% clearGroup("Subtramos") %>% clearControls()
    if (con_meta) {
      cols <- unname(col_estado[est])
      lab  <- etiquetas_estado(mp$sentido, r$tol_pct)
      leyenda <- function(p) addLegend(p, position = "bottomright", colors = unname(col_estado[names(lab)]),
                                       labels = unname(lab), opacity = 1, title = mp$etiqueta)
    } else {
      rng <- suppressWarnings(range(c(d$sim_m, d$sim_s), na.rm = TRUE))
      if (!all(is.finite(rng)) || rng[2] <= rng[1]) rng <- c(0, max(rng[2], 1))
      pal  <- leaflet::colorNumeric("YlGnBu", domain = rng)
      cols <- ifelse(is.na(sim), "#9e9e9e", pal(sim))
      leyenda <- function(p) addLegend(p, position = "bottomright", pal = pal, values = rng, title = mp$etiqueta, opacity = 1)
    }
    prox %>%
      addPolylines(data = segs, group = "Subtramos", color = "#000000", weight = 10, opacity = 0.55,
                   options = pathOptions(pane = "pane_estado_esc", interactive = FALSE)) %>%
      addPolylines(data = segs, group = "Subtramos", color = cols, weight = 6, opacity = 1,
                   label = lbls, popup = pop,
                   highlightOptions = highlightOptions(weight = 9, bringToFront = FALSE),
                   options = pathOptions(pane = "pane_estado_esc2")) %>%
      leyenda() %>%
      addControl(html = paste0("<div style='background:rgba(255,255,255,0.93);padding:5px 10px;border-radius:4px;box-shadow:0 0 6px rgba(0,0,0,0.4);",
                               "font:13px sans-serif;line-height:20px'><b>", titulo, "</b><br>", html_esc(mp$etiqueta),
                               if (con_meta) paste0(" | meta: ", html_esc(r$hor)) else "", "</div>"),
                 position = "topleft", layerId = "titulo_esc")
  }
  observe({
    req(esc_ok, mapa_esc_medio_listo())
    dibujar_esc("mapa_esc_medio", "m")
  })
  observe({
    req(esc_ok, mapa_esc_seco_listo())
    dibujar_esc("mapa_esc_seco", "s")
  })

  # Comparación subtramo a subtramo: resultado Qual2k vs meta actual del tramo
  datos_calidad <- reactive({
    req(input$sel_param_q2k, input$sel_hor_q2k)
    p   <- input$sel_param_q2k
    hor <- input$sel_hor_q2k
    mp  <- mapa_q2k_ok[mapa_q2k_ok$parametro == p, ][1, ]
    tol_pct <- input$num_tol_q2k
    if (is.null(tol_pct) || is.na(tol_pct)) tol_pct <- 5
    tol <- max(tol_pct, 0) / 100
    
    meta_de <- function(par) {
      m <- metas_comparacion_long[metas_comparacion_long$Parametro == par & metas_comparacion_long$Horizonte == hor,
                                  c("llave", "meta_actual")]
      m[!duplicated(m$llave), ]
    }
    d <- q2k_seg
    d$sim <- d[[mp$col_real]] * mp$factor
    m1 <- meta_de(mp$meta)
    meta <- m1$meta_actual[match(d$llave, m1$llave)]
    meta_lo_ph <- rep(NA_real_, nrow(d))
    if (mp$sentido == "rango") {
      m2 <- meta_de("pH min")
      meta_lo_ph <- m2$meta_actual[match(d$llave, m2$llave)]
    }
    hi <- rep(NA_real_, nrow(d)); lo <- rep(NA_real_, nrow(d))
    if (mp$sentido == "max") hi <- meta
    if (mp$sentido == "min") lo <- meta
    if (mp$sentido == "rango") { hi <- meta; lo <- meta_lo_ph }
    d$hi <- hi; d$lo <- lo
    
    below <- !is.na(lo) & d$sim < lo - tol * abs(lo)
    above <- !is.na(hi) & d$sim > hi + tol * abs(hi)
    edge  <- (!is.na(lo) & abs(d$sim - lo) <= tol * abs(lo)) | (!is.na(hi) & abs(d$sim - hi) <= tol * abs(hi))
    tiene <- !is.na(d$sim) & (!is.na(lo) | !is.na(hi))
    d$estado <- ifelse(!tiene, "Sin meta", ifelse(below | above, "Incumple", ifelse(edge, "Igual", "Cumple")))
    
    d$ref     <- if (mp$sentido == "min") lo else hi
    d$dif     <- d$sim - d$ref
    d$dif_pct <- ifelse(!is.na(d$ref) & d$ref != 0, 100 * d$dif / d$ref, NA_real_)
    list(d = d, mp = mp, hor = hor, tol_pct = tol_pct, param = p)
  })
  
  output$mapa_modelo_calidad_agua <- renderLeaflet({
    bb <- sf::st_bbox(tramos_q2k_shp)
    leaflet() %>%
      addMapPane("pane_estado_q2k", zIndex = 420) %>%
      addMapPane("pane_estado_q2k2", zIndex = 421) %>%
      addMapPane("pane_est_q2k", zIndex = 450) %>%
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Esri.WorldTopoMap") %>%
      addTiles(group = "OpenStreetMap") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri.WorldImagery") %>%
      fitBounds(as.numeric(bb["xmin"]), as.numeric(bb["ymin"]), as.numeric(bb["xmax"]), as.numeric(bb["ymax"])) %>%
      agregar_quebradas("pane_quebradas_q2k") %>%
      addMarkers(data = puntos_est, group = "Estaciones",
                 icon = icons(iconUrl = puntos_est$icono, iconWidth = 20, iconHeight = 20,
                              iconAnchorX = 10, iconAnchorY = 10),
                 label = lapply(puntos_est$etiqueta_nombre, htmltools::HTML),
                 labelOptions = opciones_lbl_estacion, popup = ~popup_simple) %>%
      addControl(html = leyenda_puntos_html, position = "bottomleft", layerId = "leyenda_puntos") %>%
      addLayersControl(position = "topright",
                       baseGroups = c("Esri.WorldTopoMap", "OpenStreetMap", "Esri.WorldImagery"),
                       overlayGroups = c("Subtramos Q2K", "Estaciones", grupos_quebradas),
                       options = layersControlOptions(collapsed = TRUE)) %>%
      addScaleBar(position = "bottomleft")
  })
  
  observe({
    req(mapa_calidad_listo())
    r <- datos_calidad(); d <- r$d; mp <- r$mp
    lab  <- etiquetas_estado(mp$sentido, r$tol_pct)
    cols <- unname(col_estado[d$estado])
    segs <- tramos_q2k_shp[d$idx, ]
    
    meta_txt <- if (mp$sentido == "rango") paste0(fmt_meta(d$lo), " a ", fmt_meta(d$hi)) else fmt_meta(d$ref)
    dif_txt  <- ifelse(is.na(d$dif), "-",
                       paste0(sprintf("%+.3g", d$dif), ifelse(is.na(d$dif_pct), "", paste0(" (", sprintf("%+.1f", d$dif_pct), "%)"))))
    pop <- paste0(
      "<div style='min-width:260px'><b>", lbl_rio(d$cod_rio), " - Tramo ", d$tramo_shp, "</b> (ID-PORH ", d$id_porh, ")<br>",
      "<b>Subtramo:</b><br>", fmt_est(d$est_ini), "<br>&darr;<br>", fmt_est(d$est_fin),
      "<hr style='margin:5px 0'>",
      "<b>Par\u00e1metro:</b> ", html_esc(r$param), " | ", html_esc(r$hor), "<br>",
      "<b>Qual2k (simulado):</b> ", fmt_meta(d$sim), "<br>",
      "<b>Meta actual:</b> ", meta_txt, "<br>",
      "<b>Diferencia:</b> ", dif_txt, "<br>",
      "<span style='background:", cols, ";padding:2px 8px;border-radius:3px;font-weight:bold'>", lab[d$estado], "</span></div>"
    )
    lbls <- paste0(lbl_rio(d$cod_rio), " - Tramo ", d$tramo_shp, " | ", r$param, ": ", fmt_meta(d$sim), " (", d$estado, ")")
    
    leafletProxy("mapa_modelo_calidad_agua") %>%
      clearGroup("Subtramos Q2K") %>%
      removeControl(layerId = "leyenda_estado_q2k") %>%
      addPolylines(data = segs, group = "Subtramos Q2K", color = "#000000", weight = 10, opacity = 0.55,
                   options = pathOptions(pane = "pane_estado_q2k", interactive = FALSE)) %>%
      addPolylines(data = segs, group = "Subtramos Q2K", color = cols, weight = 6, opacity = 1,
                   options = pathOptions(pane = "pane_estado_q2k2"), label = lbls, popup = pop) %>%
      addLegend(position = "bottomright", colors = unname(col_estado[names(lab)]), labels = unname(lab),
                title = paste0(r$param, "<br><small>Metas actuales (", r$hor, ") vs Qual2k</small>"),
                layerId = "leyenda_estado_q2k", opacity = 1)
  })
  
  observe({
    req(mapa_calidad_listo())
    proxy <- leafletProxy("mapa_modelo_calidad_agua")
    if (isTRUE(input$chk_est_q2k)) proxy %>% showGroup("Estaciones") else proxy %>% hideGroup("Estaciones")
  })
  
  # Gráfico dinámico: metas actuales vs simulación Qual2k (misma simbología que el mapa)
  output$grafico_calidad_q2k <- renderPlotly({
    req(input$sel_param_q2k, input$sel_rio_graf_q2k)
    r <- datos_calidad(); mp <- r$mp; p <- r$param
    cod <- input$sel_rio_graf_q2k; nombre_rio <- lbl_rio(cod)
    lab <- etiquetas_estado(mp$sentido, r$tol_pct)
    rango <- identical(mp$sentido, "rango")
    
    da <- r$d[which(r$d$cod_rio == cod), ]
    da <- da[order(da$abscisa_ini), ]
    req(nrow(da) > 0)
    d <- da[!is.na(da$sim), ]
    
    tr <- tabla_tramos_meta %>% filter(cod_rio == cod) %>% arrange(abs_ini)
    tm <- r$d[!duplicated(r$d$llave), c("llave", "hi", "lo")]
    tr$hi <- tm$hi[match(tr$llave, tm$llave)]
    tr$lo <- tm$lo[match(tr$llave, tm$llave)]
    n <- nrow(tr)
    
    escalon <- function(x0, x1, y) list(x = as.vector(rbind(x0, x1, NA)), y = as.vector(rbind(y, y, NA)))
    fig <- plot_ly()
    
    if (any(!is.na(tr$hi))) {
      e <- escalon(tr$abs_ini, tr$abs_fin, tr$hi)
      fig <- fig %>% add_trace(x = e$x, y = e$y, type = "scatter", mode = "lines", hoverinfo = "skip",
                               name = if (rango) "Meta actual - m\u00e1ximo" else paste0("Meta actual (", r$hor, ")"),
                               line = list(color = "#000000", width = 3))
    }
    if (any(!is.na(tr$lo))) {
      e <- escalon(tr$abs_ini, tr$abs_fin, tr$lo)
      fig <- fig %>% add_trace(x = e$x, y = e$y, type = "scatter", mode = "lines", hoverinfo = "skip",
                               name = if (rango) "Meta actual - m\u00ednimo" else paste0("Meta actual (", r$hor, ")"),
                               line = list(color = "#000000", width = 3, dash = if (rango) "dash" else "solid"))
    }
    if (nrow(d) > 0) {
      fig <- fig %>% add_trace(x = (d$abscisa_ini + d$abscisa_fin) / 2, y = d$sim, type = "scatter", mode = "lines",
                               hoverinfo = "skip", name = "Perfil Qual2k",
                               line = list(color = "#6c757d", width = 1.5, dash = "dot"))
      meta_txt <- if (rango) paste0(fmt_meta(d$lo), " a ", fmt_meta(d$hi)) else fmt_meta(d$ref)
      hov <- paste0("<b>", nombre_rio, " - Tramo ", d$tramo_shp, "</b> (ID-PORH ", d$id_porh, ")<br>",
                    d$est_ini, " &rarr; ", d$est_fin, "<br>",
                    "Km ", round(d$abscisa_ini, 2), " a ", round(d$abscisa_fin, 2), "<br>",
                    "Qual2k: ", fmt_meta(d$sim), "<br>Meta: ", meta_txt, "<br>", lab[d$estado])
      for (est in names(col_estado)) {
        k <- which(d$estado == est)
        if (length(k) == 0) next
        fig <- fig %>% add_trace(
          x = as.vector(rbind(d$abscisa_ini[k], d$abscisa_fin[k], NA)),
          y = as.vector(rbind(d$sim[k], d$sim[k], NA)),
          text = as.vector(rbind(hov[k], hov[k], NA)), hoverinfo = "text",
          type = "scatter", mode = "lines+markers", name = lab[[est]],
          line = list(color = col_estado[[est]], width = 7),
          marker = list(color = col_estado[[est]], size = 6))
      }
    }
    
    # Eje X: estaciones de inicio/fin de los subtramos (se omiten las muy cercanas)
    tx <- c(da$abscisa_ini, max(da$abscisa_fin))
    tl <- c(da$est_ini, da$est_fin[which.max(da$abscisa_fin)])
    kp <- logical(length(tx)); ultimo <- -Inf
    for (i in seq_along(tx)) if (tx[i] - ultimo > 0.3) { kp[i] <- TRUE; ultimo <- tx[i] }
    
    bandas <- lapply(seq_len(n), function(i) list(
      type = "rect", xref = "x", yref = "paper", layer = "below",
      x0 = tr$abs_ini[i], x1 = tr$abs_fin[i], y0 = 0, y1 = 1,
      fillcolor = if (i %% 2 == 1) "rgba(150,150,150,0.12)" else "rgba(150,150,150,0)",
      line = list(width = 0)))
    rotulos <- lapply(seq_len(n), function(i) list(
      x = (tr$abs_ini[i] + tr$abs_fin[i]) / 2, y = 1, xref = "x", yref = "paper",
      yanchor = "bottom", showarrow = FALSE, font = list(size = 10), text = paste0("Tramo ", tr$tramo_meta[i])))
    
    fig %>% layout(
      title = paste0("Corriente: ", nombre_rio, "<br>Parametro: ", p, " - metas actuales (", r$hor, ") vs simulaci\u00f3n Qual2k"),
      xaxis = list(tickvals = tx[kp], ticktext = paste0("Km ", round(tx[kp], 1), " - ", tl[kp]), title = "",
                   tickangle = 90, tickfont = list(size = 10), zeroline = FALSE),
      yaxis = list(title = p, rangemode = if (identical(p, "pH")) "normal" else "tozero"),
      shapes = bandas, annotations = rotulos, hovermode = "closest", autosize = TRUE,
      legend = list(orientation = "h", x = 0, y = -0.45),
      margin = list(l = 90, r = 40, b = 150, t = 110, pad = 4),
      font = list(size = 11))
  })
  
  output$tabla_calidad_q2k <- DT::renderDT({
    r <- datos_calidad(); d <- r$d
    d <- d[order(match(d$cod_rio, c("RO", "Q2Q")), d$abscisa_ini), ]
    meta_col <- if (identical(r$mp$sentido, "rango")) {
      ifelse(is.na(d$hi), "-", ifelse(is.na(d$lo), as.character(round(d$hi, 2)), paste0(round(d$lo, 2), " a ", round(d$hi, 2))))
    } else round(d$ref, 3)
    t <- data.frame(
      `Corriente` = lbl_rio(d$cod_rio), `Tramo` = d$tramo_shp, `ID-PORH` = d$id_porh,
      `Estación inicio` = d$est_ini, `Estación final` = d$est_fin,
      `Qual2k` = round(d$sim, 3),
      `Meta Actual` = meta_col,
      `Diferencia` = round(d$dif, 3), `Diferencia (%)` = round(d$dif_pct, 1),
      `Estado` = d$estado, check.names = FALSE, stringsAsFactors = FALSE)
    DT::datatable(t, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE, dom = "tip")) %>%
      DT::formatStyle("Estado", backgroundColor = DT::styleEqual(names(col_estado), unname(col_estado)),
                      fontWeight = "bold")
  })
  

  # ==============================================================================
  # SECCIÓN: ESCENARIOS DE EVALUACIÓN - TABLERO POWER BI EMBEBIDO
  # ==============================================================================
  url_powerbi_escenarios <- "https://app.powerbi.com/view?r=eyJrIjoiZThhNTI3NDktN2NkNS00YzhiLWEyZmMtZTRmNjJlOGNmYWFjIiwidCI6IjNkZmUxMjVkLWE1ODktNDRhZi05ZmM3LTExM2YzYmZmNTg3MyIsImMiOjh9"
  
  output$powerbi_escenarios <- renderUI({
    tagList(
      tags$iframe(
        title = "Escenarios de evaluaci\u00f3n - Power BI",
        src = url_powerbi_escenarios,
        width = "100%", frameborder = "0", allowfullscreen = "true",
        style = "width:100%; height:78vh; min-height:600px; border:0; display:block;"
      ),
      tags$div(
        class = "small text-secondary p-2",
        "Si el tablero no se visualiza, ",
        tags$a(href = url_powerbi_escenarios, target = "_blank", rel = "noopener", "\u00e1bralo en una pesta\u00f1a nueva"), "."
      )
    )
  })
  
  # --- C\u00f3digo anterior de Escenarios (mapa Leaflet): ya no tiene UI asociada, por lo que no se ejecuta ---
  # ==============================================================================
  # SECCIÓN: ESCENARIOS DE EVALUACIÓN (SERVER)
  # ==============================================================================
  
  output$mapa_escenarios_agua <- renderLeaflet({
    Estacionf <- if (is.null(input$RioPerfil)) "General" else input$RioPerfil
    Radiof    <- if (is.null(input$radioPerfil)) 300 else as.numeric(input$radioPerfil)
    
    df_escenarios <- NULL
    if (exists("PerfilesEscenarios") && !is.null(PerfilesEscenarios)) {
      df_escenarios <- PerfilesEscenarios[!is.na(PerfilesEscenarios$LATITUD) & !is.na(PerfilesEscenarios$LONGITUD), ]
      if (!is.null(input$RioPerfil) && input$RioPerfil != "" && "Rio" %in% colnames(df_escenarios)) {
        df_escenarios_filtrado <- df_escenarios[df_escenarios$Rio == input$RioPerfil, ]
        if (nrow(df_escenarios_filtrado) > 0) df_escenarios <- df_escenarios_filtrado
      }
    }
    
    map <- leaflet() %>%
      addMapPane("principallineas", zIndex = 440) %>%
      addMapPane("principalpuntos", zIndex = 450) %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri.WorldImagery") %>%
      addTiles(group = "OpenStreetMap") %>%
      addProviderTiles(providers$Esri.NatGeoWorld, group = "Esri.NatGeoWorld") %>%
      addProviderTiles(providers$Esri.WorldPhysical, group = "Esri.WorldPhysical") %>%
      setView(lng = -75.7, lat = 4.8, zoom = 10)
    
    if (exists("Perfilesshp") && !is.null(Perfilesshp)) {
      tryCatch({
        shp <- sf::st_zm(Perfilesshp, drop = TRUE, what = "ZM")
        if (!is.na(sf::st_crs(shp)) && sf::st_crs(shp)$epsg != 4326) {
          shp <- sf::st_transform(shp, crs = 4326)
        }
        map <- map %>% addPolylines(
          data = shp, group = "Tramos Hídricos", weight = 3, color = "blue",
          options = pathOptions(pane = "principallineas"),
          popup = ~paste0("<b>Corriente: </b>", NOMBRE, "<br><b>Tramo: </b>", ESTWQ)
        )
      }, error = function(e) {
        warning("No se pudo cargar la capa espacial Perfilesshp: ", e$message)
      })
    }
    
    if (!is.null(df_escenarios) && nrow(df_escenarios) > 0) {
      map <- map %>% addCircles(
        data = df_escenarios, lng = ~LONGITUD, lat = ~LATITUD, group = "Puntos Escenarios",
        radius = Radiof, weight = 1.5, color = "#000000", fillColor = "#28a745", fillOpacity = 0.85,
        options = pathOptions(pane = "principalpuntos"),
        popup = ~paste0(
          "<b>Estación: </b>", AbcisaNombre, "<br>",
          "<b>Río: </b>", Rio, "<br>",
          "<b>Escenario: </b>", Escenario, "<br>",
          "<b>Kilometraje: </b>", round(Abscisakm, 2), " km"
        )
      )
      coor <- c(min(df_escenarios$LONGITUD), min(df_escenarios$LATITUD), max(df_escenarios$LONGITUD), max(df_escenarios$LATITUD))
      map <- fitBounds(map, coor[1] - 0.03, coor[2] - 0.03, coor[3] + 0.03, coor[4] + 0.03)
    }
    
    map %>%
      leaflet.extras::addDrawToolbar(
        position = "topleft",
        targetGroup = "Herramientas de dibujo",
        polylineOptions = leaflet.extras::drawPolylineOptions(showLength = TRUE, metric = TRUE),
        polygonOptions = leaflet.extras::drawPolygonOptions(showArea = TRUE, metric = TRUE),
        circleOptions = leaflet.extras::drawCircleOptions(showRadius = TRUE, metric = TRUE),
        rectangleOptions = leaflet.extras::drawRectangleOptions(showArea = TRUE, metric = TRUE),
        markerOptions = leaflet.extras::drawMarkerOptions(),
        circleMarkerOptions = FALSE,
        editOptions = leaflet.extras::editToolbarOptions(selectedPathOptions = leaflet.extras::selectedPathOptions())
      ) %>%
      addLayersControl(
        position = "topright",
        baseGroups = c("Esri.WorldImagery", "OpenStreetMap", "Esri.NatGeoWorld", "Esri.WorldPhysical"),
        overlayGroups = c("Puntos Escenarios", "Tramos Hídricos", "Tramos Escenarios Evaluacion", "Herramientas de dibujo"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      addMiniMap(toggleDisplay = TRUE, position = "topright") %>%
      htmlwidgets::onRender("
      function(el, x) {
        var map = this;
        var searchControl = L.control({position: 'bottomleft'});
        searchControl.onAdd = function (map) {
          var div = L.DomUtil.create('div', 'leaflet-bar leaflet-control');
          div.style.backgroundColor = 'white'; div.style.padding = '3px'; div.style.display = 'flex'; div.style.alignItems = 'center';
          div.innerHTML = '<input type=\"text\" class=\"custom-search-input\" placeholder=\"Buscar lugar en Colombia...\" style=\"width: 170px; padding: 2px 5px; border: 1px solid #ccc; outline: none; font-size: 12px; height: 24px;\">' +
                          '<button class=\"custom-search-btn\" title=\"Buscar\" style=\"background: #fff; border: 1px solid #ccc; border-left: none; cursor: pointer; padding: 2px 6px; height: 24px;\">🔍</button>';
          L.DomEvent.disableClickPropagation(div);
          return div;
        };
        searchControl.addTo(map);

        var gpsControl = L.control({position: 'bottomright'});
        var svgTarget = '<svg width=\"16\" height=\"16\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"#444\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\"><circle cx=\"12\" cy=\"12\" r=\"10\"/><line x1=\"12\" y1=\"2\" x2=\"12\" y2=\"6\"/><line x1=\"12\" y1=\"18\" x2=\"12\" y2=\"22\"/><line x1=\"2\" y1=\"12\" x2=\"6\" y2=\"12\"/><line x1=\"18\" y1=\"12\" x2=\"22\" y2=\"12\"/><circle cx=\"12\" cy=\"12\" r=\"3\" fill=\"#444\"/></svg>';
        var svgSpinner = '<svg width=\"16\" height=\"16\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"#2563eb\" stroke-width=\"2.5\" stroke-linecap=\"round\" style=\"animation: spin 1s linear infinite;\"><style>@keyframes spin { 100% { transform: rotate(360deg); } }</style><path d=\"M21 12a9 9 0 1 1-6.219-8.56\"/></svg>';

        gpsControl.onAdd = function (map) {
          var div = L.DomUtil.create('div', 'leaflet-bar leaflet-control');
          div.style.backgroundColor = 'white'; div.style.padding = '0px';
          div.innerHTML = '<button class=\"gps-location-btn\" title=\"Obtener mi ubicación\" style=\"background: #fff; border: none; cursor: pointer; height: 30px; width: 30px; display: flex; align-items: center; justify-content: center; outline: none;\">' + svgTarget + '</button>';
          L.DomEvent.disableClickPropagation(div);
          return div;
        };
        gpsControl.addTo(map);

        var gpsMarker = null, gpsCircle = null;
        var gpsBtn = el.querySelector('.gps-location-btn');
        if (gpsBtn) {
          L.DomEvent.on(gpsBtn, 'click', function() {
            if (!navigator.geolocation) { alert('Geolocalización no soportada'); return; }
            gpsBtn.innerHTML = svgSpinner;
            navigator.geolocation.getCurrentPosition(
              function(pos) {
                gpsBtn.innerHTML = svgTarget;
                var lat = pos.coords.latitude, lng = pos.coords.longitude, acc = pos.coords.accuracy;
                if (gpsMarker) map.removeLayer(gpsMarker);
                if (gpsCircle) map.removeLayer(gpsCircle);
                map.flyTo([lat, lng], 15);
                gpsCircle = L.circle([lat, lng], { radius: acc, color: '#1d4ed8', fillColor: '#3b82f6', fillOpacity: 0.15, weight: 1.5 }).addTo(map);
                gpsMarker = L.marker([lat, lng]).addTo(map).bindPopup('<b>Ubicación del usuario</b><br>Precisión: ±' + Math.round(acc) + ' m').openPopup();
              },
              function(err) { gpsBtn.innerHTML = svgTarget; alert('Error al obtener la ubicación.'); },
              { enableHighAccuracy: true, timeout: 10000, maximumAge: 0 }
            );
          });
        }

        var searchInput = el.querySelector('.custom-search-input');
        var searchBtn = el.querySelector('.custom-search-btn');
        function executeSearch() {
          var query = searchInput.value;
          if (!query) return;
          fetch('https://nominatim.openstreetmap.org/search?format=json&countrycodes=co&q=' + encodeURIComponent(query))
            .then(function(res) { return res.json(); })
            .then(function(data) {
              if (data && data.length > 0) { map.flyTo([parseFloat(data[0].lat), parseFloat(data[0].lon)], 14); }
              else { alert('Lugar no encontrado en Colombia.'); }
            });
        }
        if (searchBtn && searchInput) {
          L.DomEvent.on(searchBtn, 'click', executeSearch);
          L.DomEvent.on(searchInput, 'keypress', function(e) { if (e.keyCode === 13) executeSearch(); });
        }
      }
    ")
  })
  
  meses_list <- c("ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "sep", "oct", "nov", "dic")
  meses_nombres <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
                     "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
  
  anim_state_escenarios <- reactiveValues(
    running = FALSE,
    index = 1
  )
  
  auto_invalidate_escenarios <- reactiveTimer(1200)
  
  observe({
    if (anim_state_escenarios$running) {
      auto_invalidate_escenarios()
      isolate({
        anim_state_escenarios$index <- ifelse(anim_state_escenarios$index >= 12, 1, anim_state_escenarios$index + 1)
        updateSelectInput(session, "sel_mes_manual_escenarios", selected = meses_list[anim_state_escenarios$index])
      })
    }
  })
  
  observeEvent(input$btn_play_mes_escenarios, {
    anim_state_escenarios$running <- !anim_state_escenarios$running
    if (anim_state_escenarios$running) {
      updateActionButton(session, "btn_play_mes_escenarios", label = " Pause", icon = icon("pause"))
    } else {
      updateActionButton(session, "btn_play_mes_escenarios", label = " Play", icon = icon("play"))
    }
  })
  
  observeEvent(input$btn_next_mes_escenarios, {
    anim_state_escenarios$running <- FALSE
    updateActionButton(session, "btn_play_mes_escenarios", label = " Play", icon = icon("play"))
    anim_state_escenarios$index <- ifelse(anim_state_escenarios$index >= 12, 1, anim_state_escenarios$index + 1)
    updateSelectInput(session, "sel_mes_manual_escenarios", selected = meses_list[anim_state_escenarios$index])
  })
  
  observeEvent(input$btn_prev_mes_escenarios, {
    anim_state_escenarios$running <- FALSE
    updateActionButton(session, "btn_play_mes_escenarios", label = " Play", icon = icon("play"))
    anim_state_escenarios$index <- ifelse(anim_state_escenarios$index <= 1, 12, anim_state_escenarios$index - 1)
    updateSelectInput(session, "sel_mes_manual_escenarios", selected = meses_list[anim_state_escenarios$index])
  })
  
  observeEvent(input$sel_mes_manual_escenarios, {
    match_idx <- match(input$sel_mes_manual_escenarios, meses_list)
    if (!is.na(match_idx)) {
      anim_state_escenarios$index <- match_idx
    }
  }, ignoreInit = TRUE)
  
  output$txt_mes_activo_escenarios <- renderText({
    meses_nombres[anim_state_escenarios$index]
  })
  
  observe({
    req(exists("tramos_escenarios"), input$sel_mes_manual_escenarios)
    
    mes_col <- input$sel_mes_manual_escenarios
    mes_nombre_txt <- meses_nombres[anim_state_escenarios$index]
    
    datos_shp <- tramos_escenarios
    df_attr <- sf::st_drop_geometry(datos_shp)
    
    for (m in meses_list) {
      if (m %in% names(df_attr)) {
        df_attr[[m]] <- suppressWarnings(as.numeric(as.character(df_attr[[m]])))
      }
    }
    
    matriz_valores <- as.matrix(df_attr[, intersect(meses_list, names(df_attr))])
    todos_los_valores <- suppressWarnings(as.numeric(matriz_valores))
    todos_los_valores <- todos_los_valores[!is.na(todos_los_valores)]
    
    min_g <- if (length(todos_los_valores) > 0) min(todos_los_valores) else 0
    max_g <- if (length(todos_los_valores) > 0) max(todos_los_valores) else 10
    
    pal <- colorNumeric(
      palette = rev("RdYlGn"), 
      domain = c(min_g, max_g), 
      na.color = "#808080"
    )
    
    val_mes <- suppressWarnings(as.numeric(as.character(df_attr[[mes_col]])))
    colores_vect <- pal(val_mes)
    colores_vect[is.na(colores_vect)] <- "#808080"
    labels_vect <- paste0("Tramo: ", datos_shp$TRAMO, " | Río: ", datos_shp$RIO, " | Valor (", mes_nombre_txt, "): ", ifelse(is.na(val_mes), "Sin Dato", val_mes))
    popups_vect <- paste0(
      "<div style='font-family: sans-serif;'>",
      "<h5 style='margin-bottom:5px; color:#2c3e50;'><b>", datos_shp$TRAMO, "</b></h5>",
      "<b>Río: </b>", datos_shp$RIO, "<br>",
      "<b>Uso: </b>", datos_shp$USO, "<br>",
      "<b>Descripción: </b>", datos_shp$Descripci, "<br>",
      "<hr style='margin: 5px 0;'>",
      "<b style='color:#0d6efd;'>Valor ", mes_nombre_txt, ": </b>",
      "<span style='font-size:14px;' class='badge bg-primary'>", ifelse(is.na(val_mes), "Sin Dato", val_mes), "</span>",
      "</div>"
    )
    
    leafletProxy("mapa_escenarios_agua") %>%
      clearGroup("Tramos Escenarios Evaluacion") %>%
      removeControl(layerId = "leyenda_escenarios") %>%
      addPolylines(
        data = datos_shp,
        group = "Tramos Escenarios Evaluacion",
        weight = 5,
        color = colores_vect,
        opacity = 0.9,
        options = pathOptions(pane = "principallineas"),
        label = labels_vect,
        popup = popups_vect
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = c(min_g, max_g),
        title = paste0("Valor Escenario (", mes_nombre_txt, ")"),
        layerId = "leyenda_escenarios",
        opacity = 0.8
      )
  })
  
  observe({
    req(exists("tramos_escenarios_long"))
    tramos_unicos <- sort(unique(tramos_escenarios_long$etiqueta_tramo))
    
    updateSelectizeInput(
      session,
      "sel_tramos_grafico_escenarios",
      choices = tramos_unicos,
      selected = tramos_unicos,
      server = TRUE
    )
  })
  
  # Botón: Activar todas
  observeEvent(input$btn_seleccionar_todos_escenarios, {
    req(exists("tramos_escenarios_long"))
    tramos_unicos <- sort(unique(tramos_escenarios_long$etiqueta_tramo))
    updateSelectizeInput(session, "sel_tramos_grafico_escenarios", selected = tramos_unicos)
  })
  
  # Botón: Desactivar todas
  observeEvent(input$btn_deseleccionar_todos_escenarios, {
    updateSelectizeInput(session, "sel_tramos_grafico_escenarios", selected = character(0))
  })
  
  # Dataset reactivo filtrado
  df_grafico_escenarios_filtrado <- reactive({
    req(exists("tramos_escenarios_long"))
    
    if (is.null(input$sel_tramos_grafico_escenarios) || length(input$sel_tramos_grafico_escenarios) == 0) {
      return(tramos_escenarios_long[0, ]) # Retorna dataframe vacío
    }
    
    tramos_escenarios_long %>%
      dplyr::filter(etiqueta_tramo %in% input$sel_tramos_grafico_escenarios)
  })
  
  
  output$grafico_escenarios_tramos <- renderPlotly({
    df <- df_grafico_escenarios_filtrado()
    
    if (nrow(df) == 0) {
      return(
        plotly_empty(type = "scatter", mode = "markers") %>%
          layout(title = list(text = "Seleccione al menos un tramo para visualizar", font = list(color = "gray", size = 12)))
      )
    }
    
    # 1. Asegurar tipo numérico limpio
    df$escenario_valor_num <- suppressWarnings(as.numeric(as.character(df$escenario_valor)))
    df_valido <- df[!is.na(df$escenario_valor_num), ]
    
    if (nrow(df_valido) == 0) {
      return(
        plotly_empty(type = "scatter", mode = "markers") %>%
          layout(title = list(text = "No hay datos numéricos válidos", font = list(color = "gray", size = 12)))
      )
    }
    
    # 2. Definir rango global unificado
    if (exists("tramos_escenarios")) {
      df_attr <- sf::st_drop_geometry(tramos_escenarios)
      matriz_v <- as.matrix(df_attr[, intersect(meses_list, names(df_attr))])
      todos_v <- suppressWarnings(as.numeric(matriz_v))
      todos_v <- todos_v[!is.na(todos_v)]
      min_g <- if (length(todos_v) > 0) min(todos_v) else 0
      max_g <- if (length(todos_v) > 0) max(todos_v) else 16
    } else {
      min_g <- min(df_valido$escenario_valor_num)
      max_g <- max(df_valido$escenario_valor_num)
    }
    
    # 3. Paleta estándar "RdYlGn"
    pal_grafico <- leaflet::colorNumeric(
      palette = "RdYlGn",
      domain = c(min_g, max_g),
      na.color = "#808080"
    )
    
    # 4. Asignación de color
    df_valido$color_hex <- pal_grafico(df_valido$escenario_valor_num)
    
    # 5. Renderizado en Plotly
    p <- plot_ly()
    
    # Líneas base grises por tramo
    tramos_presentes <- unique(df_valido$etiqueta_tramo)
    for (tr in tramos_presentes) {
      sub_df <- df_valido[df_valido$etiqueta_tramo == tr, ]
      p <- p %>% add_trace(
        data = sub_df,
        x = ~mes_nombre,
        y = ~escenario_valor_num,
        type = "scatter",
        mode = "lines",
        line = list(width = 1.2, color = "#b0b0b0"),
        showlegend = FALSE,
        hoverinfo = "none"
      )
    }
    
    # Marcadores con color
    p <- p %>% add_trace(
      data = df_valido,
      x = ~mes_nombre,
      y = ~escenario_valor_num,
      type = "scatter",
      mode = "markers",
      marker = list(
        size = 8,
        color = ~color_hex,
        opacity = 1
      ),
      text = ~paste0(
        "<b>Río: </b>", RIO, "<br>",
        "<b>Tramo: </b>", TRAMO, "<br>",
        "<b>Uso: </b>", USO, "<br>",
        "<b>Descripción: </b>", Descripci, "<br>",
        "<b>Mes: </b>", mes_nombre, "<br>",
        "<b>Valor Escenario: </b>", escenario_valor_num
      ),
      hoverinfo = "text",
      showlegend = FALSE
    )
    
    # Configuración de ejes
    p %>% layout(
      showlegend = FALSE,
      xaxis = list(
        title = list(text = "Mes del Año", standoff = 15),
        zeroline = FALSE
      ),
      yaxis = list(
        title = "Valor Escenario de Evaluación",
        range = c(min_g - 0.5, max_g + 0.5),
        zeroline = FALSE
      ),
      hovermode = "closest",
      margin = list(t = 20, b = 50, r = 30, l = 50)
    )
  })
  
  observe({
    req(input$sel_mes_manual_escenarios, exists("tramos_escenarios_long"))
    
    mes_actual_txt <- meses_nombres[anim_state_escenarios$index]
    
    mes_label <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", 
                   "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")[anim_state_escenarios$index]
    
    if (is.factor(tramos_escenarios_long$mes_nombre)) {
      mes_label <- levels(tramos_escenarios_long$mes_nombre)[anim_state_escenarios$index]
    }
    
    plotlyProxy("grafico_escenarios_tramos", session = session) %>%
      plotlyProxyInvoke(
        "relayout",
        list(
          shapes = list(
            list(
              type = "line",
              x0 = mes_label,
              x1 = mes_label,
              y0 = 0,
              y1 = 1,
              yref = "paper",
              line = list(color = "#0000FF", width = 2, dash = "dash")
            )
          ),
          annotations = list(
            list(
              x = mes_label,
              y = 1,
              yref = "paper",
              text = paste0("Mes Activo: ", mes_actual_txt),
              showarrow = FALSE,
              xanchor = "center",
              yanchor = "bottom",
              font = list(color = "#0000FF", size = 11, family = "sans-serif")
            )
          )
        )
      )
  })
  
  observeEvent(input$mapa_escenarios_agua_shape_click, {
    click_data <- input$mapa_escenarios_agua_shape_click
    
    if (!is.null(click_data$group) && click_data$group == "Tramos Escenarios Evaluacion") {
      # Lógica de resaltado interactivo
    }
  })  
  
  
  
}



shinyApp(ui = ui, server = server)