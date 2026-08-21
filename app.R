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

library(dplyr)
library(shiny)
library(bslib)
library(leaflet)
library(leaflet.extras)
library(tidyverse)
library(DT)
library(sf)

# --- FUNCIÓN AUXILIAR PARA INFOBOXES CON SOPORTE DE PANTALLA COMPLETA Y COLAPSO ---
custom_info_box <- function(title, value, icon_name, color = "primary", fill = FALSE, id = NULL) {
  box_id <- if (is.null(id)) paste0("widget_", sample(10000:99999, 1)) else id
  
  bg_class <- if (fill) paste0("bg-", color, " text-white") else "bg-body text-dark"
  header_bg <- if (fill) paste0("bg-", color, " text-white border-bottom-0") else ""
  icon_bg <- if (fill) "background-color: rgba(0,0,0,0.15);" else paste0("background-color: var(--bs-", color, "); color: white;")
  
  card(
    full_screen = TRUE,
    class = paste0("p-0 shadow-sm border-0 ", bg_class),
    
    card_header(
      class = paste0("d-flex justify-content-between align-items-center py-2 px-3 ", header_bg),
      span(style = "font-size: 11px; font-weight: bold; text-transform: uppercase; opacity: 0.9;", title),
      tags$button(
        class = "btn btn-sm p-0 text-reset border-0 opacity-75 opacity-100-hover",
        type = "button",
        `data-bs-toggle` = "collapse",
        `data-bs-target` = paste0("#collapse_", box_id),
        `aria-expanded` = "true",
        icon("minus")
      )
    ),
    
    div(
      id = paste0("collapse_", box_id),
      class = "collapse show",
      card_body(
        class = "p-0",
        div(
          class = "d-flex align-items-center",
          div(
            class = "p-3 d-flex align-items-center justify-content-center",
            style = paste0("width: 70px; min-height: 75px; ", icon_bg),
            icon(icon_name, class = "fa-2x")
          ),
          div(
            class = "p-3",
            h4(class = "m-0 font-weight-bold", value)
          )
        )
      )
    )
  )
}

# --- INTERFAZ DE USUARIO (UI) ---
ui <- page_sidebar(
  theme = bs_theme(version = 5, bootswatch = "default"),
  
  # --- ESTILOS CUSTOM (INCLUYE CLASE PARA MODALES FULLSCREEN) ---

  # --- ESTILOS CUSTOM (INCLUYE MODALES FULLSCREEN Y DATATABLES ORGANIZADO) ---
  tags$head(
    tags$style(HTML("
      /* Estilos de ocultamiento de scrollbars */
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

      /* Modal a Pantalla Completa (Full-Screen Overlay) */
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
            
      /* --- ESTILOS COMPACTOS Y ALINEACIÓN DE DATATABLES --- */
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

      /* 1. Izquierda: Selector 'Show entries' con dimensión amplia */
      .dataTables_wrapper .dataTables_length {
        float: left !important;
        margin-bottom: 0.8rem !important;
        display: flex !important;
        align-items: center !important;
      }

      /* Corrección del desplegable de 'Show entries' (Espaciado sin duplicar flecha) */
      .dataTables_wrapper .dataTables_length select {
        margin: 0 0.4rem !important;
        padding: 0.2rem 1.6rem 0.2rem 0.6rem !important; /* Padding suficiente a la derecha */
        font-size: 0.8rem !important;
        height: auto !important;
        min-width: 70px !important;                       /* Otorga espacio para 3 dígitos (100) */
        width: auto !important;
      }

      /* 2. Derecha: Grupo de Botones de Exportación + Columnas */
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

      /* 3. Derecha: Cuadro de Búsqueda (Search) */
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

      /* Limpieza de floats */
      .dataTables_wrapper::after {
        content: '';
        display: table;
        clear: both;
      }
    "))
  ),  
  
  # --- HEADER SUPERIOR CUSTOMIZADO ---
  title = div(
    class = "d-flex justify-content-between align-items-center w-100 pe-1",
    style = "font-family: 'Georgia', serif; font-weight: bold; font-size: 20px;",
    span("Era App"),
    
    div(
      class = "d-flex align-items-center gap-3 fs-6 font-sans-serif",
      style = "font-family: system-ui, -apple-system, sans-serif; font-weight: normal;",
      
      # 1. Menú de Noticias
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
      
      # 2. Menú de Notificaciones
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
      
      # 3. Menú de Tareas
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
        selected = "darkly",
        width = "120px"
      )
    )
  ),
  
  # --- BARRA LATERAL IZQUIERDA (OPCIONES CON ACCIÓN A PANTALLA COMPLETA) ---
  sidebar = sidebar(
    title = "Panel de Navegación",
    width = 350,
    div(
      class = "d-flex flex-column gap-1 mt-2",
      
      # Item 1: Dashboard
      tags$button(
        id = "btn_nav_dashboard",
        class = "nav-btn-link align-items-center mb-4 border-bottom pb-2",
        icon("gauge"), " Dashboard"
      ),
      
      # Item 2: Introducción
      actionButton("btn_nav_intro", label = tagList(icon("book-open"), " Introducción"), class = "nav-btn-link"),
      
      # Item 3: Equipo de trabajo
      actionButton("btn_nav_equipo", label = tagList(icon("users"), " Equipo de trabajo"), class = "nav-btn-link"),
      
      # Item 4: Créditos y agradecimientos
      actionButton("btn_nav_creditos", label = tagList(icon("award"), " Créditos y agradecimientos"), class = "nav-btn-link"),
      
      # Item 5: Contacto y soporte
      actionButton("btn_nav_contacto", label = tagList(icon("headset"), " Contacto y soporte"), class = "nav-btn-link")
    )
  ),
  
  # --- BARRA LATERAL DERECHA Y CONTENIDO PRINCIPAL ---
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
        hr(),
        h6("Filtros Adicionales", class = "fw-bold text-muted mb-3"),
        selectInput("filtro_capa", "Seleccionar opción:", choices = c("Todas", "Puntos", "Polígonos")),
        dateRangeInput("filtro_fecha", "Rango:", start = Sys.Date() - 30, end = Sys.Date())
      )
    ),
    
    # --- CONTENIDO PRINCIPAL ---
    div(
      h3("Dashboard"),
      hr(),
      
      layout_columns(
        fill = TRUE,
        custom_info_box("Indicador 1", "100", "credit-card", color = "primary", fill = TRUE, id = "w1"),
        custom_info_box("Indicador 2", "80%", "thumbs-up", color = "warning", fill = TRUE, id = "w2"),
        custom_info_box("Indicador 3", "45%", "list-check", color = "info", fill = TRUE, id = "w3")
      ),
      
      br(),
      
      # --- GEOVISOR ---
      card(
        full_screen = TRUE,
        class = "shadow-sm border-0",
        card_header(
          class = "d-flex align-items-center justify-content-between",
          div(icon("map-location-dot"), " Geovisor"),
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
            leafletOutput("mapa_principal", height = "520px")
          )
        )
      ),
      
      layout_columns(
        fill = TRUE,
        custom_info_box("Indicador 1", "100", "credit-card", color = "primary", fill = TRUE, id = "w1"),
        custom_info_box("Indicador 2", "80%", "thumbs-up", color = "warning", fill = TRUE, id = "w2"),
        custom_info_box("Indicador 3", "45%", "list-check", color = "info", fill = TRUE, id = "w3")
      ),
      
      layout_columns(
        col_widths = c(6, 6),
        
        # --- COLUMNA IZQUIERDA: TABLA INTERACTIVA (MPG) ---
        card(
          full_screen = TRUE,
          class = "shadow-sm border-0",
          card_header(
            class = "d-flex align-items-center justify-content-between",
            div(icon("table"), " Ejemplo de tabla interactiva"),
            tags$button(
              class = "btn btn-sm p-0 text-reset border-0 opacity-75 opacity-100-hover me-4",
              type = "button",
              `data-bs-toggle` = "collapse",
              `data-bs-target` = "#collapse_tabla_mpg_body",
              `aria-expanded` = "true",
              icon("minus")
            )
          ),
          div(
            id = "collapse_tabla_mpg_body",
            class = "collapse show",
            card_body(
              div(
                class = "row g-2 mb-3",
                div(
                  class = "col-4",
                  selectInput("man", "Manufacturer:", choices = c("All", unique(as.character(ggplot2::mpg$manufacturer))), selected = "All", width = "100%")
                ),
                div(
                  class = "col-4",
                  selectInput("trans", "Transmission:", choices = c("All", unique(as.character(ggplot2::mpg$trans))), selected = "All", width = "100%")
                ),
                div(
                  class = "col-4",
                  selectInput("cyl", "Cylinders:", choices = c("All", unique(as.character(ggplot2::mpg$cyl))), selected = "All", width = "100%")
                )
              ),
              div(
                class = "table-responsive",
                DT::dataTableOutput("table", width = "100%")
              )
            )
          )
        ),
        
        # --- COLUMNA DERECHA: GRÁFICO K-MEANS (IRIS) ---
        card(
          full_screen = TRUE,
          class = "shadow-sm border-0",
          card_header(
            class = "d-flex align-items-center justify-content-between",
            div(icon("chart-pie"), " Iris K-Means Clustering"),
            tags$button(
              class = "btn btn-sm p-0 text-reset border-0 opacity-75 opacity-100-hover me-4",
              type = "button",
              `data-bs-toggle` = "collapse",
              `data-bs-target` = "#collapse_kmeans_body",
              `aria-expanded` = "true",
              icon("minus")
            )
          ),
          div(
            id = "collapse_kmeans_body",
            class = "collapse show",
            card_body(
              # Controles del gráfico ordenados en una fila
              div(
                class = "row g-2 mb-2",
                div(
                  class = "col-4",
                  selectInput('xcol', 'X Variable:', setdiff(names(iris), "Species"), width = "100%")
                ),
                div(
                  class = "col-4",
                  selectInput('ycol', 'Y Variable:', setdiff(names(iris), "Species"), selected = setdiff(names(iris), "Species")[[2]], width = "100%")
                ),
                div(
                  class = "col-4",
                  numericInput('clusters', 'Clusters:', 3, min = 1, max = 9, width = "100%")
                )
              ),
              # Renderizado del Plot
              plotOutput('plot1', height = "280px")
            )
          )
        )
      )
    )
  )
)

# --- LÓGICA DEL SERVIDOR ---
server <- function(input, output, session) {
  
  # 1. CAMBIO DE TEMA DINÁMICO
  observeEvent(input$theme_choice, {
    session$setCurrentTheme(
      bs_theme(version = 5, bootswatch = input$theme_choice)
    )
  })
  
  # FUNCIÓN AUXILIAR PARA MODALES A PANTALLA COMPLETA / MAX ANCHO
  mostrar_seccion_fullscreen <- function(titulo, contenido_ui) {
    showModal(modalDialog(
      title = div(
        class = "d-flex justify-content-between align-items-center w-100",
        h4(titulo, class = "m-0 fw-bold"),
        modalButton("Cerrar", icon = icon("xmark"))
      ),
      contenido_ui,
      size = "xl", # Fuerza el ancho máximo de Bootstrap (Extra Large)
      easyClose = TRUE,
      footer = NULL,
      class = "modal-fullscreen-custom" # Puedes añadir estilos CSS adicionales si requieres 100% viewport width
    ))
  }
  
  # 3. EVENTOS PARA CADA OPCIÓN DEL PANEL IZQUIERDO
  
  # Option: Dashboard (Cierra cualquier modal abierta y regresa al estado principal)
  observeEvent(input$btn_nav_dashboard, {
    removeModal()
  })
  
  # Option: Introducción
  observeEvent(input$btn_nav_intro, {
    mostrar_seccion_fullscreen(
      "Introducción al Sistema",
      div(
        class = "p-4",
        h2("Bienvenido a Era App Dashboard", class = "mb-4"),
        p("Esta plataforma permite visualizar datos geoespaciales interactivos y analizar métricas relacionadas con los recursos hídricos.", 
          style = "text-align: justify;"),
        hr(),
        p("Utilice el panel izquierdo para navegar, el panel central para realizar consultas y visualizaciones de datos. Finalmente, utilice el panel derecho para importar archivos vectoriales y realizar análisis adicionales.", 
          style = "text-align: justify;")
      )
    )
  })
  
  # Option: Equipo de trabajo
  observeEvent(input$btn_nav_equipo, {
    mostrar_seccion_fullscreen(
      "Equipo de Trabajo",
      div(
        class = "p-3 p-md-4",
        h3("Nuestro Equipo", class = "fw-bold mb-4 border-bottom pb-2"),
        
        # Grid responsive: 1 col en celular, 2 en tablets, 5 en monitores
        div(
          class = "row row-cols-1 row-cols-sm-2 row-cols-md-3 row-cols-lg-5 g-3 justify-content-center",
          
          # INTEGRANTE 1
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
                tags$a(href = "#", class = "text-decoration-none text-secondary", icon("envelope"), " Email"),
                tags$a(href = "#", target = "_blank", class = "text-decoration-none text-secondary", icon("user-tie"), " Perfil CV")
              )
            )
          ),
          
          # INTEGRANTE 2
          div(
            class = "col d-flex align-items-stretch",
            div(
              class = "card w-100 p-3 border rounded-4 shadow-sm d-flex flex-column justify-content-between text-center",
              style = "background: rgba(255, 255, 255, 0.05); backdrop-filter: blur(10px); min-height: 340px;",
              div(
                div(class = "bg-secondary bg-opacity-20 rounded-circle mx-auto mb-3 d-flex align-items-center justify-content-center", style = "width: 96px; height: 96px;", icon("user", class = "fa-2x text-secondary")),
                h5("Integrante 2", class = "fw-bold text-primary mb-1", style = "font-size: 0.95rem;"),
                p("Cargo o Rol Principal", class = "text-muted small mb-3", style = "font-size: 0.78rem;")
              ),
              div(
                class = "border-top pt-2 d-flex flex-column gap-1 small",
                tags$a(href = "#", target = "_blank", class = "text-decoration-none text-info", icon("linkedin"), " LinkedIn"),
                tags$a(href = "#", class = "text-decoration-none text-secondary", icon("envelope"), " Email"),
                tags$a(href = "#", target = "_blank", class = "text-decoration-none text-secondary", icon("user-tie"), " Perfil CV")
              )
            )
          ),
          
          # INTEGRANTE 3
          div(
            class = "col d-flex align-items-stretch",
            div(
              class = "card w-100 p-3 border rounded-4 shadow-sm d-flex flex-column justify-content-between text-center",
              style = "background: rgba(255, 255, 255, 0.05); backdrop-filter: blur(10px); min-height: 340px;",
              div(
                div(class = "bg-secondary bg-opacity-20 rounded-circle mx-auto mb-3 d-flex align-items-center justify-content-center", style = "width: 96px; height: 96px;", icon("user", class = "fa-2x text-secondary")),
                h5("Integrante 3", class = "fw-bold text-primary mb-1", style = "font-size: 0.95rem;"),
                p("Cargo o Rol Principal", class = "text-muted small mb-3", style = "font-size: 0.78rem;")
              ),
              div(
                class = "border-top pt-2 d-flex flex-column gap-1 small",
                tags$a(href = "#", target = "_blank", class = "text-decoration-none text-info", icon("linkedin"), " LinkedIn"),
                tags$a(href = "#", class = "text-decoration-none text-secondary", icon("envelope"), " Email"),
                tags$a(href = "#", target = "_blank", class = "text-decoration-none text-secondary", icon("user-tie"), " Perfil CV")
              )
            )
          ),
          
          # INTEGRANTE 4
          div(
            class = "col d-flex align-items-stretch",
            div(
              class = "card w-100 p-3 border rounded-4 shadow-sm d-flex flex-column justify-content-between text-center",
              style = "background: rgba(255, 255, 255, 0.05); backdrop-filter: blur(10px); min-height: 340px;",
              div(
                div(class = "bg-secondary bg-opacity-20 rounded-circle mx-auto mb-3 d-flex align-items-center justify-content-center", style = "width: 96px; height: 96px;", icon("user", class = "fa-2x text-secondary")),
                h5("Integrante 4", class = "fw-bold text-primary mb-1", style = "font-size: 0.95rem;"),
                p("Cargo o Rol Principal", class = "text-muted small mb-3", style = "font-size: 0.78rem;")
              ),
              div(
                class = "border-top pt-2 d-flex flex-column gap-1 small",
                tags$a(href = "#", target = "_blank", class = "text-decoration-none text-info", icon("linkedin"), " LinkedIn"),
                tags$a(href = "#", class = "text-decoration-none text-secondary", icon("envelope"), " Email"),
                tags$a(href = "#", target = "_blank", class = "text-decoration-none text-secondary", icon("user-tie"), " Perfil CV")
              )
            )
          ),
          
          # INTEGRANTE 5
          div(
            class = "col d-flex align-items-stretch",
            div(
              class = "card w-100 p-3 border rounded-4 shadow-sm d-flex flex-column justify-content-between text-center",
              style = "background: rgba(255, 255, 255, 0.05); backdrop-filter: blur(10px); min-height: 340px;",
              div(
                div(class = "bg-secondary bg-opacity-20 rounded-circle mx-auto mb-3 d-flex align-items-center justify-content-center", style = "width: 96px; height: 96px;", icon("user", class = "fa-2x text-secondary")),
                h5("Integrante 5", class = "fw-bold text-primary mb-1", style = "font-size: 0.95rem;"),
                p("Cargo o Rol Principal", class = "text-muted small mb-3", style = "font-size: 0.78rem;")
              ),
              div(
                class = "border-top pt-2 d-flex flex-column gap-1 small",
                tags$a(href = "#", target = "_blank", class = "text-decoration-none text-info", icon("linkedin"), " LinkedIn"),
                tags$a(href = "#", class = "text-decoration-none text-secondary", icon("envelope"), " Email"),
                tags$a(href = "#", target = "_blank", class = "text-decoration-none text-secondary", icon("user-tie"), " Perfil CV")
              )
            )
          )
          
        )
      )
    )
  })
  
  # Option: Créditos y agradecimientos
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
  
  # Option: Contacto y soporte
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
  
  # 4. PROCESAMIENTO Y DIBUJO DE ARCHIVOS SPATIAL ZIP (.ZIP)
  observeEvent(input$cargar_zip, {
    req(input$cargar_zip)
    
    # Crear directorio temporal único
    temp_dir <- file.path(tempdir(), paste0("zip_ext_", sample(1000:9999, 1)))
    dir.create(temp_dir, showWarnings = FALSE)
    
    # Descomprimir archivo
    tryCatch({
      unzip(input$cargar_zip$datapath, exdir = temp_dir)
    }, error = function(e) {
      showNotification("Error al descomprimir el archivo ZIP.", type = "error")
      return()
    })
    
    # Buscar archivos espaciales válidos dentro del ZIP
    files_extracted <- list.files(temp_dir, full.names = TRUE, recursive = TRUE)
    spatial_file <- files_extracted[grep("\\.(shp|geojson|json|kml|topojson)$", files_extracted, ignore.case = TRUE)][1]
    
    if (is.na(spatial_file)) {
      showNotification("No se encontró ningún archivo vectorial válido (.shp, .geojson, .kml) dentro del .zip", type = "error")
      return()
    }
    
    # Leer, proyectar a EPSG:4326 y calcular área
    tryCatch({
      layer_sf <- sf::st_read(spatial_file, quiet = TRUE) %>%
        sf::st_transform(4326)
      
      layer_sf <- layer_sf %>%
        dplyr::mutate(
          area_m2 = as.numeric(sf::st_area(.)),
          area_ha = round(area_m2 / 10000, 2)
        )
      
      bbox <- sf::st_bbox(layer_sf)
      
      # Dibujar en el mapa Leaflet mediante Proxy
      leafletProxy("mapa_principal") %>%
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
  
  
output$table <- DT::renderDataTable({
  data <- ggplot2::mpg
  
  # Filtros de filas según la consulta del usuario
  if (!is.null(input$man) && input$man != "All") {
    data <- data[data$manufacturer == input$man, ]
  }
  if (!is.null(input$trans) && input$trans != "All") {
    data <- data[data$trans == input$trans, ]
  }
  if (!is.null(input$cyl) && input$cyl != "All") {
    data <- data[data$cyl == input$cyl, ]
  }
  
  DT::datatable(
    data,
    extensions = 'Buttons', # Extensión nativa de DT para botones
    class = "table-sm table-striped hover compact w-100",
    options = list(
      pageLength = 5,
      lengthMenu = c(5, 10, 25, 50, 100), # Permite al usuario elegir cuántos registros ver
      autoWidth = FALSE,
      scrollX = TRUE,
      dom = 'lBfrtip', # Incluye todos los elementos de control a la mano
      buttons = list(
        list(
          extend = 'colvis',
          text = 'Columnas',
          className = 'btn-sm btn-outline-secondary'
        ),
        list(
          extend = 'csv',
          text = 'Exportar CSV',
          className = 'btn-sm btn-outline-primary',
          exportOptions = list(
            columns = ':visible',        # Solo columnas activas/visibles
            modifier = list(page = 'all') # TODAS las filas de la consulta (sin limitar a la página actual)
          )
        ),
        list(
          extend = 'excel',
          text = 'Exportar Excel',
          className = 'btn-sm btn-outline-success',
          exportOptions = list(
            columns = ':visible',        # Solo columnas activas/visibles
            modifier = list(page = 'all') # TODAS las filas de la consulta (sin limitar a la página actual)
          )
        )
      )
    ),
    rownames = FALSE
  )
})
  

# --- LÓGICA REACTIVA DE K-MEANS ---
selectedData <- reactive({
  req(input$xcol, input$ycol)
  iris[, c(input$xcol, input$ycol)]
})

clusters <- reactive({
  req(selectedData(), input$clusters)
  kmeans(selectedData(), input$clusters)
})

output$plot1 <- renderPlot({
  palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
            "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
  
  # Ajuste de márgenes y colores adaptados a Tema Oscuro
  par(
    mar = c(4, 4, 1, 1),
    bg = "transparent",
    col.axis = "#e0e0e0",
    col.lab = "#e0e0e0",
    fg = "#e0e0e0"
  )
  
  plot(
    selectedData(),
    col = clusters()$cluster,
    pch = 20, 
    cex = 2.5,
    col.axis = "#e0e0e0",
    col.lab = "#e0e0e0"
  )
  
  # Dibujar los centroides del cluster
  points(clusters()$centers, pch = 4, cex = 3, lwd = 3, col = "#ffffff")
})


  # 5. RENDER DEL MAPA ÚNICO CON HERRAMIENTAS INTEGRADAS (OPTIMIZADO)
  output$mapa_principal <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri.WorldImagery") %>%
      addTiles(group = "OpenStreetMap") %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap, group = "Esri.NatGeoWorld") %>%
      addProviderTiles(providers$Esri.WorldPhysical, group = "Esri.WorldPhysical") %>%
      
      setView(lng = -74.8, lat = 4.8, zoom = 7) %>%
      
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
        overlayGroups = c("Herramientas de dibujo", "Capa Subida"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      
      addMiniMap(toggleDisplay = TRUE, position = "topright") %>%
      
      htmlwidgets::onRender("
        function(el, x) {
          var map = this;
          
          // CONTROL DE BÚSQUEDA (BOTTOMLEFT)
          var searchControl = L.control({position: 'bottomleft'});
          searchControl.onAdd = function (map) {
            var div = L.DomUtil.create('div', 'leaflet-bar leaflet-control');
            div.style.backgroundColor = 'white';
            div.style.padding = '3px';
            div.style.display = 'flex';
            div.style.alignItems = 'center';

            var inputId = 'customSearchInput_' + el.id;
            var listId = 'customSearchList_' + el.id;

            div.innerHTML = '<input type=\"text\" id=\"' + inputId + '\" list=\"' + listId + '\" placeholder=\"Buscar lugar en Colombia...\" style=\"width: 170px; padding: 2px 5px; border: 1px solid #ccc; outline: none; font-size: 12px; height: 24px;\">' +
                            '<datalist id=\"' + listId + '\"></datalist>' +
                            '<button id=\"customSearchBtn_' + el.id + '\" title=\"Buscar\" style=\"background: #fff; border: 1px solid #ccc; border-left: none; cursor: pointer; padding: 2px 6px; height: 24px;\">🔍</button>';

            L.DomEvent.disableClickPropagation(div);
            return div;
          };
          searchControl.addTo(map);

          // CONTROL GPS OPTIMIZADO (BOTTOMRIGHT)
          var gpsControl = L.control({position: 'bottomright'});
          var svgTarget = '<svg width=\"16\" height=\"16\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"#444\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\"><circle cx=\"12\" cy=\"12\" r=\"10\"/><line x1=\"12\" y1=\"2\" x2=\"12\" y2=\"6\"/><line x1=\"12\" y1=\"18\" x2=\"12\" y2=\"22\"/><line x1=\"2\" y1=\"12\" x2=\"6\" y2=\"12\"/><line x1=\"18\" y1=\"12\" x2=\"22\" y2=\"12\"/><circle cx=\"12\" cy=\"12\" r=\"3\" fill=\"#444\"/></svg>';
          var svgSpinner = '<svg width=\"16\" height=\"16\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"#2563eb\" stroke-width=\"2.5\" stroke-linecap=\"round\" style=\"animation: spin 1s linear infinite;\"><style>@keyframes spin { 100% { transform: rotate(360deg); } }</style><path d=\"M21 12a9 9 0 1 1-6.219-8.56\"/></svg>';

          gpsControl.onAdd = function (map) {
            var div = L.DomUtil.create('div', 'leaflet-bar leaflet-control');
            div.style.backgroundColor = 'white';
            div.style.padding = '0px';

            var gpsBtnId = 'gpsLocationBtn_' + el.id;
            div.innerHTML = '<button id=\"' + gpsBtnId + '\" title=\"Obtener mi ubicación\" style=\"background: #fff; border: none; cursor: pointer; height: 30px; width: 30px; display: flex; align-items: center; justify-content: center; outline: none;\">' + svgTarget + '</button>';

            L.DomEvent.disableClickPropagation(div);
            return div;
          };
          gpsControl.addTo(map);

          // CÁLCULO DE ÁREA EN HECTÁREAS AL DIBUJAR
          map.on(L.Draw.Event.CREATED, function (e) {
            var layer = e.layer;
            var type = e.layerType;

            if (type === 'polygon' || type === 'rectangle' || type === 'circle') {
              var areaM2 = 0;

              if (type === 'circle') {
                var radius = layer.getRadius();
                areaM2 = Math.PI * Math.pow(radius, 2);
              } else {
                areaM2 = L.GeometryUtil.geodesicArea(layer.getLatLngs()[0]);
              }

              var areaHa = (areaM2 / 10000).toFixed(2);
              var areaFormatted = Number(areaHa).toLocaleString('es-CO');

              layer.bindPopup(
                '<div style=\"font-family: sans-serif; text-align: center; padding: 5px;\">' +
                  '<strong style=\"color: #2563eb;\">Geometría Dibujada</strong><br>' +
                  '<span style=\"font-size: 15px; font-weight: bold;\">Área: ' + areaFormatted + ' ha</span><br>' +
                  '<small style=\"color: #6c757d;\">(' + Math.round(areaM2).toLocaleString('es-CO') + ' m²)</small>' +
                '</div>'
              );
            }

            map.addLayer(layer);
            if (type === 'polygon' || type === 'rectangle' || type === 'circle') {
              layer.openPopup();
            }
          });

          // EVENTOS DE BÚSQUEDA Y GPS ULTRA RÁPIDO
          setTimeout(function() {
            var input = document.getElementById('customSearchInput_' + el.id);
            var dataList = document.getElementById('customSearchList_' + el.id);
            var btn = document.getElementById('customSearchBtn_' + el.id);
            var gpsBtn = document.getElementById('gpsLocationBtn_' + el.id);
            var searchResults = {};

            var gpsMarker = null;
            var gpsCircle = null;

            if(input && dataList) {
              input.addEventListener('input', function() {
                var query = input.value;
                if (query.length < 3) return;

                var url = 'https://nominatim.openstreetmap.org/search?format=json&countrycodes=co&q=' + encodeURIComponent(query);

                fetch(url)
                  .then(function(res) { return res.json(); })
                  .then(function(data) {
                    dataList.innerHTML = '';
                    searchResults = {};
                    data.forEach(function(item) {
                      var option = document.createElement('option');
                      option.value = item.display_name;
                      dataList.appendChild(option);
                      searchResults[item.display_name] = { lat: item.lat, lon: item.lon };
                    });
                  });
              });

              function executeFlyTo() {
                var selectedVal = input.value;
                if (searchResults[selectedVal]) {
                  var coords = searchResults[selectedVal];
                  map.flyTo([parseFloat(coords.lat), parseFloat(coords.lon)], 14);
                } else if (selectedVal) {
                  fetch('https://nominatim.openstreetmap.org/search?format=json&countrycodes=co&q=' + encodeURIComponent(selectedVal))
                    .then(function(res) { return res.json(); })
                    .then(function(data) {
                      if (data && data.length > 0) {
                        map.flyTo([parseFloat(data[0].lat), parseFloat(data[0].lon)], 14);
                      } else {
                        alert('Lugar no encontrado en Colombia.');
                      }
                    });
                }
              }

              btn.addEventListener('click', executeFlyTo);
              input.addEventListener('change', executeFlyTo);
            }

            if(gpsBtn) {
              gpsBtn.addEventListener('click', function() {
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
                    alert('No se pudo obtener la ubicación. Recuerda ejecutar el proyecto abriéndolo en un navegador web (Chrome, Edge, Firefox) y no dentro de la ventana integrada de RStudio.');
                  },
                  { enableHighAccuracy: false, timeout: 5000, maximumAge: 60000 }
                );
              });
            }

            // ESCUCHADORES DE REDIMENSIONAMIENTO (EXPANDIR / COLAPSAR)
            var collapseElem = document.getElementById('collapse_mapa_body');
            if (collapseElem) {
              collapseElem.addEventListener('shown.bs.collapse', function () {
                map.invalidateSize();
              });
            }

            document.addEventListener('bslib.card', function() {
              setTimeout(function() { map.invalidateSize(); }, 250);
            });

          }, 500);
        }
      ")
  })
}

# --- EJECUCIÓN DE LA APLICACIÓN ---
shinyApp(ui = ui, server = server)