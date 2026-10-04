# ==============================================================================
# Publicación de PORH-Tool en shinyapps.io  (versión 2)
#
# Qué hace:
#   1) arma una carpeta limpia "_publicar" con SOLO lo que la app necesita
#      (sin .RData, .Rhistory, locks de ArcGIS, shapefiles de 130 MB que no se usan, etc.),
#      normalizando nombres a lo que el código espera (shinyapps.io distingue mayúsculas);
#   2) verifica archivos, shapefiles completos y paquetes;
#   3) registra la cuenta (pide token y secret por consola, no quedan escritos aquí) y publica.
#
# Uso: guardar este archivo en la carpeta de la app (junto a app.R), abrirlo en RStudio y
#      Session > Set Working Directory > To Source File Location. Luego "Source" o ejecutar por bloques.
# ==============================================================================

# Paquetes necesarios para publicar (se instalan si faltan)
for (p in c("rsconnect", "rstudioapi")) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
if (!requireNamespace("rsconnect", quietly = TRUE)) stop("No se pudo instalar 'rsconnect'. Ejecute install.packages('rsconnect') y vea el error.", call. = FALSE)

cuenta        <- "latinoamericasei"
nombre_app    <- "porh-tool"
solo_preparar <- FALSE    # TRUE = solo arma y revisa _publicar, sin publicar
probar_local  <- FALSE    # TRUE = abre la app desde _publicar antes de publicar (ciérrela para continuar)

# Ubicarse automáticamente en la carpeta de este script (RStudio) si app.R no está en el directorio actual
if (!file.exists("app.R") && requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  ruta <- tryCatch(dirname(rstudioapi::getSourceEditorContext()$path), error = function(e) "")
  if (nzchar(ruta) && dir.exists(ruta)) setwd(ruta)
}
if (!file.exists("app.R")) stop("No encuentro app.R en: ", getwd(),
                                "\nUse Session > Set Working Directory > To Source File Location.", call. = FALSE)
carpeta   <- getwd()
cat("Carpeta de la app:", carpeta, "\n")
staging   <- file.path(carpeta, "_publicar")
en_raiz   <- list.files(carpeta, all.files = FALSE, include.dirs = FALSE)

# ---- 1) Qué usa la app --------------------------------------------------------
# Shapefile: se toman .shp .shx .dbf .prj (obligatorios) y .cpg (opcional), sin importar mayúsculas en la extensión
partes_shp <- function(base) list(obligatorias = paste0(base, c(".shp", ".shx", ".dbf", ".prj")),
                                  opcionales   = paste0(base, ".cpg"))

requeridos <- c(
  "ERA_CategoriasICA.csv", "ERA_ICA_datos.csv", "ERA_ICA_estaciones.csv",
  "ERA_CategoriasIMA.csv", "ERA_IMA_datos.csv", "ERA_IMA_estaciones.csv",
  "ERA_Perfiles_AbscisaInicio.csv", "ERA_PerfilesCapasMapa.csv", "ERA_Perfiles_Parametros.csv",
  "ERA_Perfiles_WQ_Depurados.csv", "ERA_Perfiles_Metas.csv", "ERA_Perfiles_Metas_Nuevas_Final.xlsx",
  "Res1-Q2K_Calibrado.xlsx"
)
shp_requeridos <- c("RedHidrica", "MunicipiosRisaralda", "CALIDAD_RIOS_TRAMOS", "TRAMOS_PORH",
                    "Tramos_PORH_estaciones_codrio_estqw_usos", "PUNTOS_OTUN", "PUNTOS_Q2Q")

# El mapa de "Perfiles de calidad" lee en vivo las capas de contexto que lista ERA_PerfilesCapasMapa.csv:
# esas capas también deben viajar con la app
shp_contexto <- character(0)
f_caps <- list.files(carpeta, pattern = "^ERA_PerfilesCapasMapa\\.csv$", ignore.case = TRUE)
if (length(f_caps)) {
  caps <- tryCatch(read.csv(f_caps[1], skip = 1, fileEncoding = "utf-8", stringsAsFactors = FALSE, check.names = FALSE),
                   error = function(e) NULL)
  if (!is.null(caps) && "ShapesContexto" %in% names(caps)) {
    shp_contexto <- unique(sub("\\.shp$", "", basename(trimws(caps$ShapesContexto)), ignore.case = TRUE))
    shp_contexto <- shp_contexto[nzchar(shp_contexto)]
    cat("Capas de contexto del mapa de Perfiles:", paste(shp_contexto, collapse = ", "), "\n")
  }
}
shp_requeridos <- unique(c(shp_requeridos, shp_contexto))

# Archivos que la app busca por patrón (nombre canónico con el que viajan)
patrones <- list(
  list(patron = "^Escenario_actual.*medio.*\\.xlsx$",  canonico = "Escenario_actual-Clima_medio_promedio.xlsx", req = FALSE),
  list(patron = "^Escenario_actual.*seco.*\\.xlsx$",   canonico = "Escenario_actual-_Clima_seco_bajo.xlsx",     req = FALSE)
)
opcionales <- c("analisis_hidrologico.xlsx", "Top_calidad_ROtun.html", "Dsol_PORH2_colores.csv")
shp_opcionales <- c("Drenajes_otun_q2q")

# ---- 2) Armar la carpeta limpia -----------------------------------------------
unlink(staging, recursive = TRUE); dir.create(staging)
informe <- data.frame(archivo = character(), tipo = character(), estado = character(), stringsAsFactors = FALSE)
reg <- function(a, t, e) informe[nrow(informe) + 1, ] <<- list(a, t, e)

real_de <- function(nombre) {   # nombre real en la carpeta, ignorando mayúsculas
  k <- which(tolower(en_raiz) == tolower(nombre)); if (length(k)) en_raiz[k[1]] else NA_character_
}
copiar <- function(nombre, tipo) {
  r <- real_de(nombre)
  if (is.na(r)) { reg(nombre, tipo, "FALTA"); return(invisible(FALSE)) }
  file.copy(file.path(carpeta, r), file.path(staging, nombre), overwrite = TRUE)
  reg(nombre, tipo, if (identical(r, nombre)) "OK" else paste0("OK (renombrado desde '", r, "')"))
  invisible(TRUE)
}
copiar_shp <- function(base, tipo) {
  p <- partes_shp(base)
  for (f in p$obligatorias) copiar(f, tipo)
  r <- real_de(p$opcionales); if (!is.na(r)) file.copy(file.path(carpeta, r), file.path(staging, p$opcionales), overwrite = TRUE)
}

file.copy(file.path(carpeta, "app.R"), file.path(staging, "app.R"), overwrite = TRUE)
# Carpeta www (logos del encabezado): viaja con la app. shinyapps.io distingue mayúsculas, así que
# los logos se copian con los nombres en minúscula que usa app.R (logo_sei.png, logo_carder.png)
dir.create(file.path(staging, "www"))
if (dir.exists(file.path(carpeta, "www"))) {
  for (f in list.files(file.path(carpeta, "www"))) {
    destino <- if (tolower(f) %in% c("logo_sei.png", "logo_carder.png")) tolower(f) else f
    file.copy(file.path(carpeta, "www", f), file.path(staging, "www", destino), overwrite = TRUE)
  }
}
for (l in c("logo_sei.png", "logo_carder.png"))
  if (!(l %in% list.files(file.path(staging, "www"))))
    warning("Falta www/", l, ": el encabezado saldrá sin ese logo.")
for (f in requeridos)      copiar(f, "requerido")
for (b in shp_requeridos)  copiar_shp(b, "requerido")
for (f in opcionales)      copiar(f, "opcional")
for (b in shp_opcionales)  copiar_shp(b, "opcional")
for (p in patrones) {
  k <- grep(p$patron, en_raiz, ignore.case = TRUE, value = TRUE); k <- k[!grepl("^~\\$", k)]
  if (length(k)) { file.copy(file.path(carpeta, k[1]), file.path(staging, p$canonico), overwrite = TRUE)
                   reg(p$canonico, "opcional", paste0("OK (desde '", k[1], "')")) }
  else reg(p$canonico, "opcional", "FALTA")
}

cat("\n=== Revisión de archivos ===\n")
print(informe[informe$estado != "OK", ], row.names = FALSE)
if (any(informe$tipo == "requerido" & informe$estado == "FALTA"))
  stop("Faltan archivos REQUERIDOS (lista de arriba). Cópielos a la carpeta de la app y vuelva a ejecutar.")
if (any(informe$tipo == "opcional" & informe$estado == "FALTA"))
  warning("Faltan archivos opcionales: las secciones que los usan saldrán vacías o con aviso en la app publicada.")

enstaging <- list.files(staging)
tam_mb <- sum(file.size(file.path(staging, enstaging))) / 1024^2
cat(sprintf("\nArchivos a subir: %d | tamaño total: %.1f MB\n", length(enstaging), tam_mb))
top <- data.frame(archivo = enstaging, MB = round(file.size(file.path(staging, enstaging)) / 1024^2, 2))
print(head(top[order(-top$MB), ], 6), row.names = FALSE)
if (tam_mb > 300) warning("Paquete grande (>300 MB): puede agotar la memoria del plan gratuito (1 GB).")

# ---- 3) Paquetes ----------------------------------------------------------------
dep <- tryCatch(rsconnect::appDependencies(appDir = staging), error = function(e) NULL)
if (!is.null(dep)) {
  raros <- dep[!(dep$Source %in% c("CRAN", "Bioconductor")), c("Package", "Version", "Source")]
  if (nrow(raros)) { cat("\nPaquetes que NO vienen de CRAN (shinyapps.io no puede instalarlos):\n"); print(raros, row.names = FALSE) }
}

if (solo_preparar) { cat("\nCarpeta lista en:", staging, "\n"); stop("solo_preparar = TRUE: no se publica.", call. = FALSE) }
if (probar_local)  shiny::runApp(staging)

# ---- 4) Cuenta ------------------------------------------------------------------
cuentas <- rsconnect::accounts()
if (is.null(cuentas) || !(cuenta %in% cuentas$name)) {
  message("Registrando la cuenta '", cuenta, "' (los valores se piden por consola y no se guardan en este archivo).")
  token  <- trimws(readline("Token: "))
  secret <- trimws(readline("Secret: "))
  rsconnect::setAccountInfo(name = cuenta, token = token, secret = secret)
}

# ---- 5) Publicar -----------------------------------------------------------------
rsconnect::deployApp(
  appDir        = staging,
  appPrimaryDoc = "app.R",
  appName       = nombre_app,
  appTitle      = "PORH-Tool",
  account       = cuenta,
  server        = "shinyapps.io",
  forceUpdate   = TRUE,
  lint          = FALSE
)

# ---- 6) Si falla en la nube, revise los registros ----------------------------------
# rsconnect::showLogs(appName = nombre_app, account = cuenta, entries = 300)
