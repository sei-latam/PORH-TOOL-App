

+-------------------------------------------------------+
|                       INTERFAZ (UI)                   |
|  - Entradas del usuario (ej. Selección de cuenca)    |
|  - Salidas visuales (ej. Mapas, gráficos, reportes)  |
+-------------------------------------------------------+
                           |
                           |  (Puente Reactivo: input$ / output$)
                           v
+-------------------------------------------------------+
|                     SERVIDOR (Server)                 |
|  - Lógica e ingeniería ambiental (rgee, sf, terra)    |
|  - Cálculos y generación de plots/mapas dinámicos     |
+-------------------------------------------------------+


1. Análisis del Stack Tecnológico Proporcionado
Tu entorno se agrupa orgánicamente en 5 grandes bloques funcionales:

A. El Núcleo de la Aplicación y la Interfaz (UI/UX)
shiny, shinydashboard, bslib: El trinomio de control. bslib te da acceso a Bootstrap 5 para que la app no parezca el clásico reporte antiguo de R, sino una plataforma web moderna con layouts limpios.

shinyjs y shinyWidgets: Críticos para mejorar la experiencia de usuario. Te permitirán habilitar/deshabilitar botones de cálculo dinámicamente y usar selectores avanzados.

shinyhelper: Excelente adición para colocar iconos de ayuda (?) al lado de tus parámetros algorítmicos para guiar al usuario.

B. El Motor Geoespacial y Teledetección (El núcleo pesado)
sf, stars, terra y raster: Tienes la evolución completa del GIS en R. Mientras que raster mantiene compatibilidad con código base antiguo, terra procesará tus capas ráster de manera ultra veloz. sf se encargará de los vectores (cuencas, estaciones) y stars para cubos de datos espacio-temporales.

tmap y tmaptools: Una de las mejores librerías para generar mapas temáticos estáticos y dinámicos con código muy limpio.

chirps: Esta librería es una joya para tu backend; automatiza la descarga de datos de precipitación global del sensor CHIRPS directo a tu entorno.

C. Visualización Geoespacial e Interactiva
leaflet, leafem, leafsync: Tu visor de mapas. leafem añade extensiones útiles (como capturas de pantalla del mapa o consulta de coordenadas al vuelo), y leafsync te servirá si necesitas poner dos mapas en espejo (por ejemplo, comparar el mapa de riesgo de inundación vs. mapa de cobertura del suelo).

plotly, highcharter, heatmaply: Gráficos dinámicos interactivos. highcharter genera visualizaciones comerciales e industriales sumamente pulidas para balances hídricos o curvas de excedencia.

D. Hidrología y Series Temporales
hydroTSM y hydroGOF: Específicas para hidrología. Sirven para la gestión, análisis y modelado de series temporales hidrológicas, además del cálculo de bondad de ajuste de modelos (eficiencia de Nash-Sutcliffe, RMSE, etc.).

xts y zoo: Estructuras de datos indexadas en el tiempo, obligatorias para el manejo limpio de datos de estaciones climáticas y caudales.

E. Gestión de Datos y Rendimiento
data.table y vroom: Velocidad pura. Si la app debe leer archivos planos gigantescos en segundos, vroom y data.table procesan millones de filas saltándose los cuellos de botella habituales de read.csv.

future y promises: Indispensables para la arquitectura asíncrona. Si tu modelo hidrológico toma tiempo, estas librerías evitarán que el servidor se congele para otros usuarios concurrentes.

Estructura y Componentes de los Datos
1. El Bloque de Vectores SIG (Shapefiles de Risaralda)
Cada juego de archivos (.shp, .shx, .dbf, .prj) representa las capas espaciales base que maneja la aplicación. Destacan:

Políticos y Delimitación: MunicipiosRisaralda y LimitesRisaralda.

Zonificación Hidrográfica: SZH (Subzonas Hidrográficas, estándar IDEAM), CuencasPOMCAS, UH_CARDER98 (Unidades Hidrográficas regionales) y WEAPCatchment (cuencas parametrizadas para el software de modelación hídrica WEAP).

Físicos y Ambientales: Suelos_CARDER, Cobertura_CARDER (coberturas de suelo, probablemente Corine Land Cover) y Pendientes_CARDER (ojo con este: pesa 133 MB, procesarlo en memoria dentro de Shiny de forma ineficiente puede ralentizar el servidor).

Restricciones e Hidrografía: AreasProtegidas, Humedales, RedHidrica, RedH y RondasHidricas.

2. El Motor de Atributos y Series Temporales (CSVs y Excel)
Son las bases de datos alfanuméricas de donde la app extrae la información analítica:

Monitoreo y Calidad: ERA_Caudales.csv (series temporales de ríos), ERA_ICA_datos.csv (Índice de Calidad del Agua) con su respectivo ERA_ICA_estaciones.csv. También tienes datos de la metodología IVICAS (Índice de Vulnerabilidad de la Infiltración de Contaminantes a Acuíferos) en versiones somera y profunda.

Demanda y Gestión del Agua: El archivo TUA_ERA-TOOL_Nov2025.xlsx (Tasa por Utilización de Aguas) y las capas de ConcesionesSuperficiales, ConcesionesSubterraneas, Concesiones_TUA y CorrientesAgotadas. Esto indica que la app calcula o cruza balances de cuánta agua está otorgada mediante permisos ambientales frente a la oferta hídrica disponible.

3. Activos Visuales e Interfaz (Imágenes)
Tienes una enorme cantidad de archivos .png numerados (3.png, 10.png, etc.) y otros llamados Introduccion1.png a Introduccion5.png y Encabezado3.png a Encabezado12.png.

Esto delata que la interfaz del app.R actual utiliza intensamente pestañas informativas fijas, guías visuales paso a paso (tutoriales), o banners institucionales dinámicos para justificar metodologías.



