# Carga e instalación de librerías según tu estructura
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

library(shiny)
library(bslib)
library(leaflet)
library(tidyverse)
library(DT)
library(sf)

# --- FUNCIÓN AUXILIAR PARA CREAR INFOBOXES ESTILO B EN CÓDIGO A ---
custom_info_box <- function(title, value, icon_name, color = "primary", fill = FALSE) {
  if (fill) {
    card(
      class = paste0("bg-", color, " text-white p-0 overflow-hidden"),
      div(
        class = "d-flex align-items-center",
        div(
          class = "p-3 d-flex align-items-center justify-content-center",
          style = "width: 70px; background-color: rgba(0,0,0,0.15);",
          icon(icon_name, class = "fa-2x")
        ),
        div(
          class = "p-3",
          div(style = "font-size: 11px; font-weight: bold; text-transform: uppercase; opacity: 0.9;", title),
          h4(class = "m-0 font-weight-bold", value)
        )
      )
    )
  } else {
    card(
      class = "p-0 overflow-hidden border-0 shadow-sm",
      div(
        class = "d-flex align-items-center",
        div(
          class = paste0("bg-", color, " text-white p-3 d-flex align-items-center justify-content-center"),
          style = "width: 70px; min-height: 75px;",
          icon(icon_name, class = "fa-2x")
        ),
        div(
          class = "p-3",
          div(class = "text-muted", style = "font-size: 11px; font-weight: bold; text-transform: uppercase;", title),
          h4(class = "m-0 font-weight-bold text-dark", value)
        )
      )
    )
  }
}

# --- INTERFAZ DE USUARIO (UI) ---
ui <- page_sidebar(
  theme = bs_theme(version = 5, bootswatch = "default"),
  
  # --- HEADER SUPERIOR CUSTOMIZADO ---
  title = div(
    class = "d-flex justify-content-between align-items-center w-100 pe-3",
    style = "font-family: 'Georgia', serif; font-weight: bold; font-size: 20px;",
    span("Mi Dashboard"),
    
    # Barra de herramientas superior (Mensajes, Notificaciones, Tareas y Tema)
    div(
      class = "d-flex align-items-center gap-3 fs-6 font-sans-serif",
      style = "font-family: system-ui, -apple-system, sans-serif; font-weight: normal;",
      
      # 1. Menú de Mensajes
      tags$div(
        class = "dropdown",
        tags$a(
          class = "text-white text-decoration-none position-relative cursor-pointer",
          `data-bs-toggle` = "dropdown",
          href = "#",
          icon("envelope"),
          span(class = "position-absolute top-0 start-100 translate-middle badge rounded-pill bg-info", "3")
        ),
        tags$ul(
          class = "dropdown-menu dropdown-menu-end shadow-sm",
          tags$li(h6(class = "dropdown-header", "Mensajes")),
          tags$li(a(class = "dropdown-item small", href = "#", strong("Ventas: "), "Las ventas se mantienen estables este mes.")),
          tags$li(a(class = "dropdown-item small", href = "#", strong("Nuevo Usuario: "), "¿Cómo me registro?")),
          tags$li(a(class = "dropdown-item small", href = "#", strong("Soporte: "), "El nuevo servidor está listo."))
        )
      ),
      
      # 2. Menú de Notificaciones
      tags$div(
        class = "dropdown ms-2",
        tags$a(
          class = "text-white text-decoration-none position-relative cursor-pointer",
          `data-bs-toggle` = "dropdown",
          href = "#",
          icon("triangle-exclamation"),
          span(class = "position-absolute top-0 start-100 translate-middle badge rounded-pill bg-warning", "3")
        ),
        tags$ul(
          class = "dropdown-menu dropdown-menu-end shadow-sm",
          tags$li(h6(class = "dropdown-header", "Notificaciones")),
          tags$li(a(class = "dropdown-item small", href = "#", icon("users"), " 5 usuarios nuevos hoy")),
          tags$li(a(class = "dropdown-item small", href = "#", icon("truck"), " 12 envíos entregados")),
          tags$li(a(class = "dropdown-item small", href = "#", icon("triangle-exclamation"), " Carga del servidor al 86%"))
        )
      ),
      
      # 3. Menú de Tareas
      tags$div(
        class = "dropdown ms-2 me-3",
        tags$a(
          class = "text-white text-decoration-none position-relative cursor-pointer",
          `data-bs-toggle` = "dropdown",
          href = "#",
          icon("list-check"),
          span(class = "position-absolute top-0 start-100 translate-middle badge rounded-pill bg-success", "4")
        ),
        tags$ul(
          class = "dropdown-menu dropdown-menu-end shadow-sm",
          style = "width: 250px;",
          tags$li(h6(class = "dropdown-header", "Tareas")),
          tags$li(div(class = "dropdown-item small", "Documentación (90%)", div(class = "progress", style = "height: 5px;", div(class = "progress-bar bg-success", style = "width: 90%;")))),
          tags$li(div(class = "dropdown-item small", "Proyecto X (17%)", div(class = "progress", style = "height: 5px;", div(class = "progress-bar bg-info", style = "width: 17%;")))),
          tags$li(div(class = "dropdown-item small", "Despliegue servidor (75%)", div(class = "progress", style = "height: 5px;", div(class = "progress-bar bg-warning", style = "width: 75%;")))),
          tags$li(div(class = "dropdown-item small", "Proyecto General (80%)", div(class = "progress", style = "height: 5px;", div(class = "progress-bar bg-danger", style = "width: 80%;"))))
        )
      ),
      
      # Selector de Temas Dinámico
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
  
  # --- BARRA LATERAL (Sidebar) ---
  sidebar = sidebar(
    title = "Navegación",
    width = 250,
    p(icon("gauge"), " Dashboard")
  ),
  
  # --- CONTENIDO PRINCIPAL ---
  div(
    h3("Indicadores Principales"),
    hr(),
    
    # --- SECCIÓN 1: VALUE BOXES ---
    layout_columns(
      fill = FALSE,
      value_box(
        title = "Nuevas Órdenes",
        value = "100",
        showcase = icon("credit-card"),
        p("Tasa de crecimiento mensual"),
        theme = "primary"
      ),
      value_box(
        title = "Tasa de Aprobación",
        value = "80%",
        showcase = icon("thumbs-up"),
        p("Basado en el último reporte"),
        theme = "warning"
      ),
      value_box(
        title = "Progreso del Proyecto",
        value = "45%",
        showcase = icon("list-check"),
        p("Avance de la fase actual"),
        theme = "info"
      )
    ),
    
    br(),
    
    # --- SECCIÓN 2: INFO BOXES (Sin relleno) ---
    layout_columns(
      fill = FALSE,
      custom_info_box("Nuevas Órdenes", "10*2", "credit-card", color = "primary", fill = FALSE),
      custom_info_box("Progreso", "25%", "list", color = "info", fill = FALSE),
      custom_info_box("Aprobación", "80%", "thumbs-up", color = "warning", fill = FALSE)
    ),
    
    br(),
    
    # --- SECCIÓN 3: INFO BOXES (Con relleno) ---
    layout_columns(
      fill = FALSE,
      custom_info_box("Nuevas Órdenes", "10*2", "credit-card", color = "primary", fill = TRUE),
      custom_info_box("Progreso", "25%", "list", color = "info", fill = TRUE),
      custom_info_box("Aprobación", "80%", "thumbs-up", color = "warning", fill = TRUE)
    )
  )
)

# --- LÓGICA DEL SERVIDOR ---
server <- function(input, output, session) {
  
  observeEvent(input$theme_choice, {
    session$setCurrentTheme(
      bs_theme(version = 5, bootswatch = input$theme_choice)
    )
  })
  
}

# --- EJECUCIÓN DE LA APLICACIÓN ---
shinyApp(ui = ui, server = server)