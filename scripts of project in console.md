install.packages("renv")
renv::init()

if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")
renv::init()

install.packages(c("shiny", "shinydashboard", "bslib", "shinyjs", "shinyWidgets", "shinyhelper", "tidyverse", "data.table", "vroom"))
install.packages(c("plotly", "highcharter", "heatmaply", "DT", "reactable"))
install.packages(c("sp", "sf", "raster", "terra", "stars", "lwgeom"))
install.packages(c("leaflet", "leafem", "leafsync", "tmap", "tidyterra"))
install.packages(c("zoo", "xts", "hydroTSM", "hydroGOF", "chirps", "automap"))

# 5. Guardar estado del proyecto
renv::snapshot()