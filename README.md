# Era R Shinny App

```
mi-proyecto-shiny/
├── .gitignore               # Archivos que Git debe ignorar (básico para datos pesados)
├── README.md                # Presentación del proyecto y cómo ejecutarlo
├── app.R                    # Archivo central (opcional, o dividido en ui.R/server.R/global.R)
├── global.R                 # Carga de librerías, datos globales y preparación de GIS
├── ui.R                     # Definición de la interfaz de usuario (estructura de pestañas)
├── server.R                 # Lógica del servidor principal (orquestador de módulos)
│
├── R/                       # Carpeta de funciones
│   ├── utils_helpers.R      # Funciones auxiliares de R que no son reactivas
│   ├── mod_mapa.R           # Módulo Shiny para el mapa de Leaflet
│   ├── mod_perfiles.R       # Módulo Shiny para los gráficos de perfiles (reutilizable)
│   └── mod_tabla_datos.R    # Módulo Shiny para el visor de datos
│
├── data/                    # Archivos de datos (CSV, Excel, etc.)
│   ├── raw/                 # Datos originales sin modificar
│   └── processed/           # Datos ya limpios y filtrados listos para la app
│
├── shp/                     # Archivos geográficos (Shapefiles, GeoJSON, etc.)
│
└── www/                     # Carpeta estándar de Shiny para recursos web
    ├── css/
    │   └── custom.css       # Tus estilos de diseño personalizados (fuera de la UI)
    ├── img/
    │   ├── logotipo.png     # Logotipos y banners del proyecto
    │   └── favicon.ico      # Icono de la pestaña del navegador
    └── js/
        └── custom.js        # Scripts de JS complementarios (opcional)
```
