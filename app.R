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


# ==============================================================================
# IMPORTACIÓN Y PREPARACIÓN DE DATOS: ESCENARIOS DE EVALUACIÓN
# ==============================================================================

# 1. Importación y transformación espacial del Shapefile de Escenarios
tramos_escenarios_shp <- read_sf(dsn = "TRAMOS_PORH.shp")
tramos_escenarios     <- st_zm(tramos_escenarios_shp, drop = TRUE, what = "ZM")
tramos_escenarios     <- st_transform(tramos_escenarios, crs = 4326)

# FORZAR COLUMNA DE DISTANCIA Y MESES A NÚMERO
if ("distancia" %in% names(tramos_escenarios)) {
  tramos_escenarios$distancia <- as.numeric(as.character(tramos_escenarios$distancia))
}

meses_cols <- c("ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "sep", "oct", "nov", "dic")
for (m in meses_cols) {
  if (m %in% names(tramos_escenarios)) {
    tramos_escenarios[[m]] <- as.numeric(as.character(tramos_escenarios[[m]]))
  }
}

# 2. Creación de la tabla en formato largo para el gráfico de Plotly (Escenarios)
tramos_escenarios_long <- tramos_escenarios %>%
  st_drop_geometry() %>%
  mutate(
    # Conservar y asegurar el tipo de distancia en el data frame
    distancia = as.numeric(distancia),
    # Etiqueta única combinando Río, Tramo y Descripción
    etiqueta_tramo = paste0(RIO, " | ", TRAMO, " (", Descripci, ")")
  ) %>%
  pivot_longer(
    cols = all_of(meses_cols),
    names_to = "mes_id",
    values_to = "escenario_valor"  # Renombrado de 'meta_valor' a 'escenario_valor'
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


# 1. Carga del Shapefile de Tramos (Otún y Dosquebradas)
tramos_meta_shp <- read_sf("Tramos_D2k_Otun.shp")
tramos_meta_shp <- st_zm(tramos_meta_shp, drop = TRUE, what = "ZM")
tramos_meta_shp <- st_transform(tramos_meta_shp, crs = 4326)

# Asegurar tipo numérico para la distancia
if ("distancia" %in% names(tramos_meta_shp)) {
  tramos_meta_shp$distancia <- as.numeric(as.character(tramos_meta_shp$distancia))
}

# 2. Carga del Excel de Parámetros y Metas de Calidad
perfiles_metas_excel <- read_excel("ERA_Perfiles_Metas_Nuevas_Final.xlsx")

# Limpieza de nombres de columna (elimina columnas vacías como 'Unnamed: 22' si existen)
perfiles_metas_excel <- perfiles_metas_excel %>% 
  select(-contains("Unnamed"))

# Identificación automática de columnas de parámetros de calidad
cols_metadatos <- c("Identificador", "Meta", "Rio", "Tramo", "Uso")
parametros_calidad <- setdiff(names(perfiles_metas_excel), cols_metadatos)

# Convertir todas las columnas de parámetros a tipo numérico
perfiles_metas_excel <- perfiles_metas_excel %>%
  mutate(across(all_of(parametros_calidad), ~ as.numeric(as.character(.))))

# 3. Creación de tabla larga (Pivot) para gráfico y análisis dinámico
tramos_meta_calidad_long <- perfiles_metas_excel %>%
  pivot_longer(
    cols = all_of(parametros_calidad),
    names_to = "Parametro",
    values_to = "meta_valor"
  ) %>%
  mutate(
    etiqueta_tramo = paste0(Rio, " | ", Tramo)
  )



# --- INTERFAZ DE USUARIO (UI) ---
ui <- page_sidebar(
  theme = bs_theme(version = 5, bootswatch = "darkly"),
  
  tags$head(
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
    class = "d-flex justify-content-between align-items-center w-100 pe-1",
    style = "font-family: 'Georgia', serif; font-weight: bold; font-size: 20px;",
    span("PORH-Tool"),
    
    div(
      class = "d-flex align-items-center gap-3 fs-6 font-sans-serif",
      style = "font-family: system-ui, -apple-system, sans-serif; font-weight: normal;",
      
      tags$div(
        class = "dropdown",
        tags$a(
          class = "text-white text-decoration-none position-relative cursor-pointer",
          `data-bs-toggle` = "dropdown",
          href = "#",
          icon("newspaper"),
          span(class = "position-absolute top-0 start-100 translate-middle badge rounded-pill bg-info", "1")
        ),
        tags$ul(
          class = "dropdown-menu dropdown-menu-end shadow-sm",
          tags$li(h6(class = "dropdown-header", "Noticias")),
          tags$li(a(class = "dropdown-item small", href = "#", strong("Noticia 1: "), "Información 1."))
        )
      ),
      
      tags$div(
        class = "dropdown ms-2",
        tags$a(
          class = "text-white text-decoration-none position-relative cursor-pointer",
          `data-bs-toggle` = "dropdown",
          href = "#",
          icon("triangle-exclamation"),
          span(class = "position-absolute top-0 start-100 translate-middle badge rounded-pill bg-warning", "1")
        ),
        tags$ul(
          class = "dropdown-menu dropdown-menu-end shadow-sm",
          tags$li(h6(class = "dropdown-header", "Notificaciones")),
          tags$li(a(class = "dropdown-item small", href = "#", icon("warning"), "Notificación: 1"))
        )
      ),
      
      tags$div(
        class = "dropdown ms-2 me-3",
        tags$a(
          class = "text-white text-decoration-none position-relative cursor-pointer",
          `data-bs-toggle` = "dropdown",
          href = "#",
          icon("list-check"),
          span(class = "position-absolute top-0 start-100 translate-middle badge rounded-pill bg-success", "1")
        ),
        tags$ul(
          class = "dropdown-menu dropdown-menu-end shadow-sm",
          style = "width: 250px;",
          tags$li(h6(class = "dropdown-header", "Versión 1.0")),
          tags$li(div(class = "dropdown-item small", "Desarrollo (40%)", div(class = "progress", style = "height: 5px;", div(class = "progress-bar bg-success", style = "width: 40%;")))),
          tags$li(div(class = "dropdown-item small", "Diseño (40%)", div(class = "progress", style = "height: 5px;", div(class = "progress-bar bg-info", style = "width: 40%;")))),
          tags$li(div(class = "dropdown-item small", "Validación (55%)", div(class = "progress", style = "height: 5px;", div(class = "progress-bar bg-warning", style = "width: 55%;")))),
          tags$li(div(class = "dropdown-item small", "Socialización(10%)", div(class = "progress", style = "height: 5px;", div(class = "progress-bar bg-danger", style = "width: 10%;"))))
        )
      ),
      
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
  
  sidebar = sidebar(
    title = "Panel de Navegación",
    width = 350,
    div(
      class = "d-flex flex-column gap-1 mb-3",
      actionButton("btn_nav_estaciones", label = tagList(icon("gauge"), " Información de las estaciones"), class = "nav-btn-link text-start"),
      actionButton("btn_nav_perfiles", label = tagList(icon("chart-line"), " Perfiles de calidad"), class = "nav-btn-link text-start"),
      actionButton("btn_nav_metas", label = tagList(icon("bullseye"), " Metas de calidad de agua"), class = "nav-btn-link text-start"),
      actionButton("btn_nav_topologia", label = tagList(icon("diagram-project"), " Modelo topología"), class = "nav-btn-link text-start"),
      actionButton("btn_nav_escenarios", label = tagList(icon("sliders"), " Escenarios de evaluación"), class = "nav-btn-link text-start")
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
        
        selectInput(
          inputId = "TipoICA",
          label = "Seleccione el Índice ICA:",
          choices = c("Ambos", "ICA-5 [IDEAM]", "ICA-6 [CARDER]"),
          selected = "Ambos"
        ),
        
        sliderInput(
          inputId = "radioICA",
          label = "Fije el radio de los puntos ICA en el mapa",
          min = 0,
          max = 2000,
          value = 100,
          step = 50
        ),
        
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
    ),
  
    navset_hidden(
      id = "secciones_main",
      
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
              div(icon("map-location-dot"), "Información de las estaciones (title)"),
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
                    title = "ICA-5 [IDEAM]",
                    leafletOutput("mapa_ica5_ideam", height = "520px")
                  ),
                  nav_panel(
                    title = "ICA-6 [CARDER]",
                    leafletOutput("mapa_ica6_carder", height = "520px")
                  ),
                  nav_panel(
                    title = "IMA",
                    leafletOutput("mapa_ima", height = "520px")
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
                fluidRow(
                  column(12, plotlyOutput("ICAt1", height = "380px")),
                  column(12, class = "mt-3", plotlyOutput("ICAt2", height = "380px"))
                )
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
      ),
     
      # ==============================================================================
      # SECCIÓN: METAS DE CALIDAD DE AGUA (UI)
      # ==============================================================================
      nav_panel_hidden(
        value = "sec_metas",
        div(
          h3("Metas de Calidad de Agua"),
          hr(),
          br(),
          
          # CSS GLOBAL PARA CONTROLAR LA ALTURA Y SCROLL DE LOS SELECTINPUT
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
          
          # 1. GEOVISOR DE METAS DE CALIDAD
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
              
              # BARRA SUPERIOR DE SELECCIÓN DE HORIZONTE DE META
              div(
                class = "bg-light border-bottom py-2 px-3 d-flex align-items-center justify-content-between",
                div(class = "d-flex align-items-center gap-2")
              ),
              
              card_body(
                padding = 0,
                leafletOutput("mapa_metas_agua", height = "600px")
              ),
              
              # BOTONERA Y CONTROLES UNIFICADOS EN UN SOLO DIV
              card_footer(
                class = "bg-light pt-2 pb-5 px-3",
                div(
                  class = "d-flex align-items-center justify-content-between flex-wrap gap-2",
                  
                  # TODO EN UN MISMO CONTENEDOR FLEX
                  div(
                    class = "d-flex align-items-center gap-2 w-100 justify-content-between",
                    
                    # 1. Indicador del parámetro activo
                    div(
                      class = "d-flex align-items-center gap-2",
                      span(class = "text-black", "Parámetro:"),
                      span(
                        id = "lbl_parametro_activo_metas",
                        class = "text-black fs-8 px-2 py-2",
                        textOutput("txt_parametro_activo_metas", inline = FALSE)
                      )
                    ),
                    
                    # 2. Botones de reproducción
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
                    
                    # 3. Selector manual de parámetro
                    selectInput(
                      inputId = "sel_parametro_manual_metas",
                      label = NULL,
                      choices = NULL,
                      width = "280px"
                    ),
                    
                    # 4. Selector de horizonte de meta
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
          
          # 2. GRÁFICO LONGITUDINAL POR DISTANCIA (METAS)
          card(
            full_screen = TRUE,
            class = "shadow-sm border-0 mb-4",
            card_header(
              class = "d-flex align-items-center justify-content-between",
              div(icon("chart-line"), " Perfil Longitudinal de Calidad de Agua (Distancia vs Meta)"),
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
                # BARRA DE CONTROLES DEL GRÁFICO
                div(
                  class = "row g-2 align-items-center mb-2",
                  div(
                    class = "col-auto",
                    actionButton("btn_seleccionar_todos_metas", " Activar todas", icon = icon("check-double"), class = "btn-sm btn-outline-primary")
                  ),
                  div(
                    class = "col-auto",
                    actionButton("btn_deseleccionar_todos_metas", " Desactivar todas", icon = icon("ban"), class = "btn-sm btn-outline-secondary")
                  ),
                  div(
                    class = "col",
                    selectizeInput(
                      inputId = "sel_tramos_grafico_metas",
                      label = NULL,
                      choices = NULL,
                      multiple = TRUE,
                      options = list(
                        placeholder = "Seleccione o busque tramos...",
                        plugins = list("remove_button"),
                        maxOptions = 100
                      ),
                      width = "100%"
                    )
                  )
                ),
                # GRÁFICO LONGITUDINAL DE PLOTLY
                plotlyOutput("grafico_metas_tramos", height = "420px")
              )
            )
          )
        )
      ),
      
      # ==============================================================================
      # SECCIÓN: MODELO DE TOPOLOGÍA HÍDRICA (UI - VACÍA)
      # ==============================================================================
      nav_panel_hidden(
        value = "sec_topologia",
        div(
          h3("Modelo de Topología Hídrica"),
          hr(),
          br()
        )
      ),
      
      nav_panel_hidden(
        value = "sec_escenarios",
        div(
          h3("Escenarios de evaluación"),
          hr(),
          br(),
          
          # --- CARD 1: GEOVISOR DE ESCENARIOS ---
          card(
            full_screen = TRUE,
            class = "shadow-sm border-0 mb-4",
            card_header(
              class = "d-flex align-items-center justify-content-between",
              div(icon("map-location-dot"), " Geovisor de escenarios de evaluación"),
              tags$button(
                class = "btn btn-sm p-0 text-reset border-0 opacity-75 opacity-100-hover me-4",
                type = "button",
                `data-bs-toggle` = "collapse",
                `data-bs-target` = "#collapse_mapa_escenarios_body",
                `aria-expanded` = "true",
                icon("minus")
              )
            ),
            div(
              id = "collapse_mapa_escenarios_body",
              class = "collapse show",
              card_body(
                padding = 0,
                leafletOutput("mapa_escenarios_agua", height = "600px")
              ),
              
              # --- CONTROLES DE ANIMACIÓN MENSUAL DE ESCENARIOS ---
              card_footer(
                class = "bg-light py-2 px-3",
                div(
                  class = "d-flex align-items-center justify-content-between flex-wrap gap-2",
                  
                  # 1. Indicador del mes activo
                  div(
                    class = "d-flex align-items-center gap-2",
                    span(class = "fw-bold text-secondary", "Mes en visualización:"),
                    span(
                      id = "lbl_mes_activo_escenarios",
                      class = "badge bg-primary fs-6 px-3 py-2",
                      textOutput("txt_mes_activo_escenarios", inline = TRUE)
                    )
                  ),
                  
                  # 2. Controles de Reproducción
                  div(
                    class = "btn-group shadow-sm",
                    role = "group",
                    `aria-label` = "Controles de animación mensual escenarios",
                    
                    actionButton(
                      inputId = "btn_prev_mes_escenarios",
                      label = NULL,
                      icon = icon("step-backward"),
                      class = "btn btn-outline-secondary btn-sm",
                      title = "Mes anterior"
                    ),
                    actionButton(
                      inputId = "btn_play_mes_escenarios",
                      label = " Play",
                      icon = icon("play"),
                      class = "btn btn-success btn-sm fw-bold",
                      title = "Iniciar/Pausar animación automática"
                    ),
                    actionButton(
                      inputId = "btn_next_mes_escenarios",
                      label = NULL,
                      icon = icon("step-forward"),
                      class = "btn btn-outline-secondary btn-sm",
                      title = "Mes siguiente"
                    )
                  ),
                  
                  # 3. Selector manual directo de mes
                  div(
                    class = "d-flex align-items-center gap-2",
                    selectInput(
                      inputId = "sel_mes_manual_escenarios",
                      label = NULL,
                      choices = c(
                        "Enero" = "ene", "Febrero" = "feb", "Marzo" = "mar", 
                        "Abril" = "abr", "Mayo" = "may", "Junio" = "jun", 
                        "Julio" = "jul", "Agosto" = "ago", "Septiembre" = "sep", 
                        "Octubre" = "oct", "Noviembre" = "nov", "Diciembre" = "dic"
                      ),
                      selected = "ene",
                      width = "140px"
                    )
                  )
                )
              )
            )
          ),
          
          # --- CARD 2: GRÁFICO DE VARIACIÓN ESPACIO-TEMPORAL ---
          card(
            full_screen = TRUE,
            class = "shadow-sm border-0 mb-4",
            card_header(
              class = "d-flex align-items-center justify-content-between",
              div(icon("chart-line"), " Variación espacial-temporal de escenarios de evaluación"),
              tags$button(
                class = "btn btn-sm p-0 text-reset border-0 opacity-75 opacity-100-hover me-4",
                type = "button",
                `data-bs-toggle` = "collapse",
                `data-bs-target` = "#collapse_grafico_escenarios_body",
                `aria-expanded` = "true",
                icon("minus")
              )
            ),
            div(
              id = "collapse_grafico_escenarios_body",
              class = "collapse show",
              card_body(
                # ESTILOS COMPACTOS SELECTIZE
                tags$head(
                  tags$style(HTML("
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
                padding: 3px 8px !important;
              }
            "))
                ),
                
                # BARRA DE CONTROLES
                div(
                  class = "row g-2 align-items-center mb-2",
                  div(
                    class = "col-auto",
                    actionButton("btn_seleccionar_todos_escenarios", " Activar todas", icon = icon("check-double"), class = "btn-sm btn-outline-primary")
                  ),
                  div(
                    class = "col-auto",
                    actionButton("btn_deseleccionar_todos_escenarios", " Desactivar todas", icon = icon("ban"), class = "btn-sm btn-outline-secondary")
                  ),
                  div(
                    class = "col",
                    selectizeInput(
                      inputId = "sel_tramos_grafico_escenarios",
                      label = NULL,
                      choices = NULL,
                      multiple = TRUE,
                      options = list(
                        placeholder = "Seleccione o busque tramos...",
                        plugins = list("remove_button"),
                        maxOptions = 100
                      ),
                      width = "100%"
                    )
                  )
                ),
                
                # GRÁFICO PLOTLY
                plotlyOutput("grafico_escenarios_tramos", height = "420px")
              )
            )
          )
        )
      )
      
      
    )
  )
)



# --- LÓGICA DEL SERVIDOR ---
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
  
  
  observeEvent(input$btn_nav_metas, {
    nav_select("secciones_main", selected = "sec_metas")
  })
  
  observeEvent(input$btn_nav_topologia, {
    nav_select("secciones_main", selected = "sec_topologia")
  })
  
  observeEvent(input$btn_nav_escenarios, {
    nav_select("secciones_main", selected = "sec_escenarios")
  })
  
  
  observeEvent(input$btn_nav_intro, {
    mostrar_seccion_fullscreen(
      "Introducción al Sistema",
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
  
  ERA_ICA <- reactive({
    estacion_sel <- if (is.null(input$EstacionICA) || input$EstacionICA == "") "Todas" else input$EstacionICA
    
    if (estacion_sel == "Todas") {
      return(ICA_datos1)
    } else {
      return(ICA_datos1[ICA_datos1$Estacion == estacion_sel, ])
    }
  })
  
  output$contenedor_graficos_dinamico <- renderUI({
    tipo <- if (is.null(input$TipoICA)) "Ambos" else input$TipoICA
    
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
    ICA_datosSub <- ICA_datosSub[, c("Estacion", "N_Datos", ICAs, ICAsc, "LATITUD", "LONGITUD", "CORRIENTE")]
    
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
      paste("datos_ICA_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(VALORES[["ICAtablamapa"]], file, row.names = FALSE)
    }
  )
  
 
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
  # SECCIÓN: METAS DE CALIDAD DE AGUA (SERVER COMPLETO Y ACTUALIZADO)
  # ==============================================================================
  
  # 1. RENDERIZADO INICIAL DEL GEOVISOR DE METAS
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
  
  # 2. INICIALIZACIÓN Y CONTROLES DE ANIMACIÓN POR PARÁMETROS
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
  
  # 3. ACTUALIZACIÓN DINÁMICA DE TRAMOS EN EL MAPA (LEAFLET PROXY METAS)
  observe({
    req(exists("tramos_meta_shp"), exists("perfiles_metas_excel"), 
        input$sel_parametro_manual_metas, input$sel_meta_horizonte_metas)
    
    param_activo <- input$sel_parametro_manual_metas
    meta_activa  <- input$sel_meta_horizonte_metas
    
    # Estandarización sencilla (mayúsculas y sin espacios al inicio/final)
    df_excel_sub <- perfiles_metas_excel %>%
      filter(toupper(trimws(Meta)) == toupper(trimws(meta_activa))) %>%
      mutate(
        Rio_clean = toupper(trimws(Rio)),
        Tramo_clean = toupper(trimws(Tramo))
      )
    
    tramos_shp_clean <- tramos_meta_shp %>%
      mutate(
        RIO_clean = toupper(trimws(RIO)),
        TRAMO_clean = toupper(trimws(TRAMO))
      )
    
    # Cruce directo de atributos
    datos_mapa <- tramos_shp_clean %>%
      inner_join(df_excel_sub, by = c("RIO_clean" = "Rio_clean", "TRAMO_clean" = "Tramo_clean"))
    
    req(nrow(datos_mapa) > 0, param_activo %in% names(datos_mapa))
    
    val_param <- suppressWarnings(as.numeric(as.character(datos_mapa[[param_activo]])))
    
    # Paleta de colores dinámica
    vals_validos <- val_param[!is.na(val_param)]
    min_g <- if (length(vals_validos) > 0) min(vals_validos) else 0
    max_g <- if (length(vals_validos) > 0) max(vals_validos) else 10
    if (min_g == max_g) max_g <- min_g + 1
    
    pal <- colorNumeric(
      palette = rev("RdYlGn"), 
      domain = c(min_g, max_g), 
      na.color = "#808080"
    )
    
    colores_vect <- pal(val_param)
    colores_vect[is.na(colores_vect)] <- "#808080"
    
    labels_vect <- paste0("Tramo: ", datos_mapa$TRAMO, " | Río: ", datos_mapa$RIO, 
                          " | ", param_activo, ": ", ifelse(is.na(val_param), "Sin Dato", val_param))
    
    popups_vect <- paste0(
      "<div style='font-family: sans-serif;'>",
      "<h5 style='margin-bottom:5px; color:#2c3e50;'><b>", datos_mapa$TRAMO, "</b></h5>",
      "<b>Río: </b>", datos_mapa$RIO, "<br>",
      "<b>Uso: </b>", datos_mapa$USO, "<br>",
      "<b>Escenario: </b>", datos_mapa$Meta, "<br>",
      "<b>Descripción: </b>", datos_mapa$Descripci, "<br>",
      "<hr style='margin: 5px 0;'>",
      "<b style='color:#0d6efd;'>Meta ", param_activo, ": </b>",
      "<span style='font-size:14px;' class='badge bg-primary'>", ifelse(is.na(val_param), "Sin Dato", val_param), "</span>",
      "</div>"
    )
    
    # Encuadre automático del mapa a todos los tramos (Otún + Quebrada Dos Quebradas)
    bbox <- sf::st_bbox(datos_mapa)
    
    leafletProxy("mapa_metas_agua") %>%
      clearGroup("Tramos Metas Calidad") %>%
      removeControl(layerId = "leyenda_metas") %>%
      addPolylines(
        data = datos_mapa,
        group = "Tramos Metas Calidad",
        weight = 5,
        color = colores_vect,
        opacity = 0.9,
        options = pathOptions(pane = "principallineas_metas"),
        label = labels_vect,
        popup = popups_vect
      ) %>%
      fitBounds(
        lng1 = as.numeric(bbox["xmin"]), lat1 = as.numeric(bbox["ymin"]),
        lng2 = as.numeric(bbox["xmax"]), lat2 = as.numeric(bbox["ymax"])
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = c(min_g, max_g),
        title = paste0(param_activo, "<br><small>", meta_activa, "</small>"),
        layerId = "leyenda_metas",
        opacity = 0.8
      )
  })
  
  # 4. DATASET REACTIVO PARA PERFIL LONGITUDINAL
  dataset_metas_distancia <- reactive({
    req(exists("tramos_meta_shp"), exists("tramos_meta_calidad_long"))
    
    df_distancias <- tramos_meta_shp %>%
      st_drop_geometry() %>%
      mutate(
        RIO_clean = toupper(trimws(RIO)),
        TRAMO_clean = toupper(trimws(TRAMO))
      ) %>%
      select(RIO_clean, TRAMO_clean, distancia) %>%
      distinct()
    
    tramos_meta_calidad_long %>%
      mutate(
        RIO_clean = toupper(trimws(Rio)),
        TRAMO_clean = toupper(trimws(Tramo))
      ) %>%
      inner_join(df_distancias, by = c("RIO_clean", "TRAMO_clean"))
  })
  
  # Actualización dinámica del selector de tramos en el gráfico
  observe({
    df <- dataset_metas_distancia()
    req(df, "etiqueta_tramo" %in% names(df))
    
    tramos_unicos <- sort(unique(df$etiqueta_tramo))
    updateSelectizeInput(
      session,
      "sel_tramos_grafico_metas",
      choices = tramos_unicos,
      selected = tramos_unicos,
      server = TRUE
    )
  })
  
  observeEvent(input$btn_seleccionar_todos_metas, {
    df <- dataset_metas_distancia()
    req(df, "etiqueta_tramo" %in% names(df))
    tramos_unicos <- sort(unique(df$etiqueta_tramo))
    updateSelectizeInput(session, "sel_tramos_grafico_metas", selected = tramos_unicos)
  })
  
  observeEvent(input$btn_deseleccionar_todos_metas, {
    updateSelectizeInput(session, "sel_tramos_grafico_metas", selected = character(0))
  })
  
  # Renderizado del gráfico perfil longitudinal (Distancia vs Meta Valor)
  output$grafico_metas_tramos <- renderPlotly({
    df <- dataset_metas_distancia()
    
    req(input$sel_parametro_manual_metas, input$sel_meta_horizonte_metas)
    
    param_actual <- input$sel_parametro_manual_metas
    meta_actual  <- input$sel_meta_horizonte_metas
    
    df_sub <- df %>%
      filter(toupper(trimws(Parametro)) == toupper(trimws(param_actual)),
             toupper(trimws(Meta)) == toupper(trimws(meta_actual)))
    
    if (!is.null(input$sel_tramos_grafico_metas) && length(input$sel_tramos_grafico_metas) > 0) {
      df_sub <- df_sub[df_sub$etiqueta_tramo %in% input$sel_tramos_grafico_metas, ]
    }
    
    if (nrow(df_sub) == 0) {
      return(
        plotly_empty(type = "scatter", mode = "markers") %>%
          layout(title = list(text = "Seleccione al menos un tramo para visualizar", font = list(color = "gray", size = 12)))
      )
    }
    
    df_valido <- df_sub %>%
      filter(!is.na(distancia), !is.na(meta_valor)) %>%
      arrange(distancia)
    
    if (nrow(df_valido) == 0) {
      return(
        plotly_empty(type = "scatter", mode = "markers") %>%
          layout(title = list(text = paste0("Sin valores de metas para ", param_actual), font = list(color = "gray", size = 12)))
      )
    }
    
    min_g <- min(df_valido$meta_valor)
    max_g <- max(df_valido$meta_valor)
    if (min_g == max_g) max_g <- min_g + 1
    
    pal_grafico <- leaflet::colorNumeric(palette = "RdYlGn", domain = c(min_g, max_g), na.color = "#808080")
    df_valido$color_hex <- pal_grafico(df_valido$meta_valor)
    
    plot_ly(data = df_valido) %>%
      add_trace(
        x = ~distancia,
        y = ~meta_valor,
        type = "scatter",
        mode = "lines+markers",
        line = list(color = "#2563eb", width = 2),
        marker = list(size = 9, color = ~color_hex, line = list(color = "#333333", width = 1)),
        text = ~paste0(
          "<b>Tramo: </b>", etiqueta_tramo, "<br>",
          "<b>Distancia: </b>", round(distancia, 2), " km<br>",
          "<b>Horizonte: </b>", Meta, "<br>",
          "<b>", param_actual, ": </b>", meta_valor
        ),
        hoverinfo = "text",
        showlegend = FALSE
      ) %>%
      layout(
        title = list(text = paste0("Perfil Longitudinal: ", param_actual, " (", meta_actual, ")"), font = list(size = 14)),
        xaxis = list(
          title = list(text = "Distancia (km)", standoff = 15),
          zeroline = FALSE,
          showgrid = TRUE
        ),
        yaxis = list(
          title = paste0(param_actual),
          zeroline = FALSE,
          showgrid = TRUE
        ),
        hovermode = "closest",
        margin = list(t = 40, b = 50, r = 30, l = 50)
      )
  })
  
  
  
  
  # ==============================================================================
  # SECCIÓN: ESCENARIOS DE EVALUACIÓN (SERVER)
  # ==============================================================================
  
  # 1. MAPA BASE DE ESCENARIOS
  output$mapa_escenarios_agua <- renderLeaflet({
    
    # Captura de entradas interactivas
    Estacionf <- if (is.null(input$RioPerfil)) "General" else input$RioPerfil
    Radiof    <- if (is.null(input$radioPerfil)) 300 else as.numeric(input$radioPerfil)
    
    # Filtrado del Dataframe de Escenarios
    df_escenarios <- NULL
    if (exists("PerfilesEscenarios") && !is.null(PerfilesEscenarios)) {
      df_escenarios <- PerfilesEscenarios[!is.na(PerfilesEscenarios$LATITUD) & !is.na(PerfilesEscenarios$LONGITUD), ]
      if (!is.null(input$RioPerfil) && input$RioPerfil != "" && "Rio" %in% colnames(df_escenarios)) {
        df_escenarios_filtrado <- df_escenarios[df_escenarios$Rio == input$RioPerfil, ]
        if (nrow(df_escenarios_filtrado) > 0) df_escenarios <- df_escenarios_filtrado
      }
    }
    
    # Inicialización del Mapa y Panes
    map <- leaflet() %>%
      addMapPane("principallineas", zIndex = 440) %>%
      addMapPane("principalpuntos", zIndex = 450) %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri.WorldImagery") %>%
      addTiles(group = "OpenStreetMap") %>%
      addProviderTiles(providers$Esri.NatGeoWorld, group = "Esri.NatGeoWorld") %>%
      addProviderTiles(providers$Esri.WorldPhysical, group = "Esri.WorldPhysical") %>%
      setView(lng = -75.7, lat = 4.8, zoom = 10)
    
    # Capa fija de Tramos Hídricos Generales
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
    
    # Capa fija de Puntos de Escenarios de Evaluación
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
    
    # Herramientas y Controles del Mapa
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
  
  
  # 2. CONTROLES Y ANIMACIÓN DE MESES (ESCENARIOS)
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
  
  
  # 3. ACTUALIZACIÓN DINÁMICA DEL MAPA MEDIANTE LEAFLET PROXY
  observe({
    req(exists("tramos_escenarios"), input$sel_mes_manual_escenarios)
    
    mes_col <- input$sel_mes_manual_escenarios
    mes_nombre_txt <- meses_nombres[anim_state_escenarios$index]
    
    datos_shp <- tramos_escenarios
    df_attr <- sf::st_drop_geometry(datos_shp)
    
    # Asegurar tipo numérico limpio
    for (m in meses_list) {
      if (m %in% names(df_attr)) {
        df_attr[[m]] <- suppressWarnings(as.numeric(as.character(df_attr[[m]])))
      }
    }
    
    # Calcular rango global unificado de todo el año
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
    
    # Preparación directa de vectores
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
    
    # Renderizado liviano sobre el mapa activo
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
  
  
  # ==============================================================================
  # 4. POBLADO DINÁMICO Y CONTROLES DE SELECCIÓN DE TRAMOS
  # ==============================================================================
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
  
  
  # ==============================================================================
  # 5. RENDERIZADO DEL GRÁFICO DE ESCENARIOS
  # ==============================================================================
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
  
  
  # ==============================================================================
  # 6. SINCRONIZACIÓN DE LÍNEA VERTICAL DEL MES ACTIVO EN GRÁFICO
  # ==============================================================================
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
  
  
  # ==============================================================================
  # 7. INTERACTIVIDAD EN MAPA
  # ==============================================================================
  observeEvent(input$mapa_escenarios_agua_shape_click, {
    click_data <- input$mapa_escenarios_agua_shape_click
    
    if (!is.null(click_data$group) && click_data$group == "Tramos Escenarios Evaluacion") {
      # Lógica de resaltado interactivo
    }
  })  
  

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  }



shinyApp(ui = ui, server = server)