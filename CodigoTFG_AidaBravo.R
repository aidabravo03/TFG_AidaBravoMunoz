# LIBRERIAS NECESARIAS
library(readr)       # Lectura rápida de archivos CSV
library(tidyverse)   # Manipulación y visualización de datos
library(dplyr)       # Manipulación de datos (filter, mutate, summarise, joins)
library(lubridate)   # Manejo y transformación de fechas
library(sf)          # Datos espaciales(shapefiles, coordenadas...)
library(units)       # Manejo de unidades físicas(ej.metros^2 al calcular áreas)
library(ggplot2)     # Visualización de gráficos
library(patchwork)   # Combinación de gráficos
library(scales)      # Formato de ejes y etiquetas
library(fixest)      # Estimación de modelos con efectos fijos (DID)
library(stringr)     # Manipulación y limpieza de texto, funciones sencillas.
library(stringi)     # Tratamiento avanzado de normalización de textos.
library(fuzzyjoin)   # Unión de tablas según similitud entre textos.

# ==============================================================================
# 0. CONFIGURACIÓN GENERAL DEL PROYECTO
# ==============================================================================
dir_datos    <- "Datos"
dir_graficos <- "Gráficos"

# ==============================================================================
# 1. CARGAMOS LOS DATOS
# ==============================================================================
air <- read_csv(file.path(dir_datos, "AirbnbBarcelonaOrig.csv"),
                show_col_types = FALSE)
colnames(air)

air <- air %>% as_tibble() %>%
  select(id, host_since, host_total_listings_count, 
         neighbourhood_cleansed,
         latitude,longitude,room_type,license) %>%
  # tratar cadenas vacías como NA
  mutate(across(where(is.character), ~na_if(.x, ""))) %>%
  # tratamos las fechas
  mutate(host_since = ymd(host_since)) %>% 
  # tratamos las coordenadas
  mutate(across(c(latitude, longitude),~ as.numeric(.x))) %>%
  # tratamos las categóricas (factores)
  mutate(across(c(host_total_listings_count,neighbourhood_cleansed,room_type),
                ~ as.factor(.x)))

# Rango temporal que queremos conservar
ini <- ymd("2015-01-01") # Enero de 2015
fin <- ymd("2023-12-31") # Dicimebre de 2023

air <- air %>%
  # asegurar fecha y llevarla al primer día de su mes
  mutate(
    host_since  = as_date(host_since),
    host_month  = floor_date(host_since, "month")) %>%
  # QUEDARNOS SOLO con ene-2015 .. dic-2023 (el resto se elimina)
  filter(between(host_month, ini, fin)) %>%
  
  # Creamos las variables referentes al año y mes 
  mutate(AÑO_ALTA = year(host_month),
         MES_ALTA = (interval(ini, host_month) %/% months(1)) + 1L) %>%
  
  # Creamos la variable multi_host
  mutate(MULTI_HOST = as.integer(as.numeric(
    as.character(host_total_listings_count)) >= 2)) %>%
  
  # Arreglamos "room type" quedándonos exclusivamente con datos airbnb 
  # y uniendo "Private room" y "Shared room" en "HC" (Hogar Compartido)
  mutate(
    room_type = case_when(
      room_type %in% c("Private room", "Shared room")  ~ "HC",
      room_type == "Hotel room"                        ~ NA_character_, 
      TRUE                                             ~ room_type)) %>%
  filter(!is.na(room_type)) %>%                           
  mutate(room_type = factor(room_type, levels = c("Entire home/apt","HC"))) %>%
  
  # LICENCIA: En En Barcelona, una licencia turística válida suele tener formato: 
  # HUTB-XXXXXX, HUT-XXXXXX, HB-XXXX, HT-XXXX
  mutate(
    license_clean = str_trim(as.character(license)),
    HAS_LICENSE = if_else(
      !is.na(license_clean) &
        str_detect(str_to_upper(license_clean),
                   "^(HUTB|HUT|HB|HT)[- ]?[0-9]+"),1L,0L)) %>%
  
  # Eliminamos variables innecesarias
  select(-host_since, -host_total_listings_count,-host_month,
         -license, -license_clean) 

# ==============================================================================
# 1.1 ASIGNACIÓN DE ZONA PEUAT
# ==============================================================================
# Cargamos la capa de zonas PEUAT
peuat <- st_read(file.path(dir_datos, "zonasPEUAT.gpkg"), layer = "BasePEUAT_")

# Convertimos Airbnb en objeto espacial(sf) usando las coordenadas lat. y long.
air_sf <- st_as_sf(air,                          
                   coords = c("longitude", "latitude"),
                   crs = 4326) # WGS84 (coordenadas geográficas)
#Transformamos las coordenadas de Airbnb al mismo CRS que el PEUAT
air_sf <- st_transform(air_sf, crs = st_crs(peuat))

# Función auxiliar para agrupar las zonas PEUAT
clasificar_ZONA_PEUAT <- function(x) {
  case_when(
    x == "ZE1" ~ "1",
    x == "ZE2" ~ "2",
    x %in% c("ZE3A", "ZE3B", "ZE3C", "ZE3D", "ZE3E") ~ "3",
    x %in% c("ZE4A", "ZE4B", "ZE4C") ~ "4",
    x == "EXCLO" ~ "0",
    TRUE ~ NA_character_)}

# Asignamos a cada airbnb su zona PEUAT correspondiente 
air <- st_join(air_sf, peuat, join = st_within) %>%
  # Eliminamos columnas técnicas (no necesarias)
  select(-OBJECTID, -Shape_Length, -Shape_Area) %>%
  # Creamos la variable agrupada ZONA_PEUAT
  mutate(
    ZONA_PEUAT = clasificar_ZONA_PEUAT(ZE),,
    ZONA_PEUAT = factor(ZONA_PEUAT,levels = c("0", "1", "2", "3", "4"))) %>%
  # Eliminamos zonas excluidas o sin asignación
  filter(ZONA_PEUAT != "0" & !is.na(ZONA_PEUAT)) %>%  
  mutate(ZONA_PEUAT = droplevels(ZONA_PEUAT)) %>%     
  select(-ZE, -AREA)

# Verificamos los recuentos ZONA_PEUAT
table(air$ZONA_PEUAT, useNA = "ifany")

# ==============================================================================
# 1.2 CARGA DE BARRIOS
# ==============================================================================
barris <- st_read(file.path(dir_datos, "0301040100_Barris_UNITATS_ADM.shp"), 
                  quiet = TRUE) %>%
  st_transform(st_crs(peuat)) %>%
  # Nos quedamos con los barrios, sus geometrías y el area de cada barrio
  select(NOM, geometry, AREA) %>%
  rename(neighbourhood_cleansed = NOM,
         AREA_BARRIO = AREA) %>%
  mutate(neighbourhood_cleansed = case_when(
         neighbourhood_cleansed == "el Poble-sec" ~ "el Poble Sec",
         TRUE ~ neighbourhood_cleansed))

# Preparamos PEUAT agregado por zona
peuat_agg <- peuat %>%
  mutate(ZONA_PEUAT = clasificar_ZONA_PEUAT(ZE)) %>%
  filter(ZONA_PEUAT %in% c("1", "2", "3", "4")) %>%
  select(ZONA_PEUAT, geom) %>%
  group_by(ZONA_PEUAT) %>%
  summarise(do_union = TRUE, .groups = "drop")

# Cruzamos barrios y zonas PEUAT
barris_zones <- st_intersection(barris, peuat_agg)

# Comprobamos qué barrios caen en más de una zona PEUAT relevante
check_barrios_partidos <- barris_zones %>%
  st_drop_geometry() %>%
  filter(ZONA_PEUAT %in% c("1", "2", "3")) %>%
  distinct(neighbourhood_cleansed, ZONA_PEUAT) %>%
  count(neighbourhood_cleansed, name = "n_zonas") %>%
  filter(n_zonas > 1)
check_barrios_partidos

# ==============================================================================
# 1.3 DEFINICIÓN DE BARRIOS PARTIDOS
# ==============================================================================
# A partir deL MAPA1, se dividen solo los barrios donde la
# partición PEUAT es relevante para el análisis.
barrios_partidos <- c("Sant Antoni",
                      "Vallcarca i els Penitents",
                      "el Putxet i el Farró")

# Nos quedamos con los fragmentos de estos barrios
barris_partits <- barris_zones %>%
  filter(neighbourhood_cleansed %in% barrios_partidos) %>%
  # Eliminamos fragmentos residuales de frontera no relevantes
  filter(!(neighbourhood_cleansed == "el Putxet i el Farró" & ZONA_PEUAT == "1"),
    !(neighbourhood_cleansed == "Vallcarca i els Penitents" & ZONA_PEUAT == "1"))

# Creamos etiqueta Nord/Sud usando la posición vertical del centroide interno
pts <- st_point_on_surface(barris_partits)
coords <- st_coordinates(pts)

barris_partits <- barris_partits %>%
  mutate(y_coord = coords[, 2]) %>%
  group_by(neighbourhood_cleansed) %>%
  arrange(desc(y_coord), .by_group = TRUE) %>%
  mutate(
    ns = case_when(
      row_number() == 1 ~ "Nord",
      row_number() == 2 ~ "Sud",
      TRUE ~ paste0("Part", row_number())),
    BARRIO = paste0(neighbourhood_cleansed, " ", ns),
    AREA_BARRIO = as.numeric(st_area(geometry))) %>%
  ungroup() %>%
  select(neighbourhood_cleansed, ZONA_PEUAT, BARRIO, AREA_BARRIO, geometry)

# Comprobación de barrios partidos
barris_partits %>%
  st_drop_geometry() %>%
  select(neighbourhood_cleansed, ZONA_PEUAT, BARRIO, AREA_BARRIO)

# ==============================================================================
# 1.4 ASIGNACIÓN DE BARRIO Y ÁREA_BARRIO A CADA AIRBNB
# ==============================================================================
# Barrios NO partidos
barris_area <- barris %>%
  st_drop_geometry() %>%
  select(neighbourhood_cleansed, AREA_BARRIO)

air_np <- air %>%
  filter(!(as.character(neighbourhood_cleansed) %in% barrios_partidos)) %>%
  left_join(barris_area, by = "neighbourhood_cleansed") %>%
  mutate(BARRIO = as.character(neighbourhood_cleansed))

# Barrios partidos
air_split <- air %>%
  filter(as.character(neighbourhood_cleansed) %in% barrios_partidos)

air_split <- st_join(air_split,
                    barris_partits %>% select(BARRIO, AREA_BARRIO),
                    join = st_within,left = TRUE)

# En caso de que justo esté en la frontera de la partición entre zonas,
# se le asigna el "subbarrio" más cercano
idx_na <- which(is.na(air_split$BARRIO))

if (length(idx_na) > 0) {
  nearest_idx <- st_nearest_feature(air_split[idx_na, ], barris_partits)
  air_split$BARRIO[idx_na] <- barris_partits$BARRIO[nearest_idx]
  air_split$AREA_BARRIO[idx_na] <- barris_partits$AREA_BARRIO[nearest_idx]}

# UNIMOS TODO
air <- bind_rows(air_np, air_split) %>%
  mutate(BARRIO = factor(BARRIO))

# COMPROBAMOS
# Finalmente comprobamos que cada barrio tenga UNA SOLA zona y UNA SOLA area
check_barrios <- air %>%
  st_drop_geometry() %>%
  group_by(BARRIO) %>%
  summarise(
    n_zonas = n_distinct(ZONA_PEUAT[!is.na(ZONA_PEUAT)]),
    n_areas = n_distinct(AREA_BARRIO[!is.na(AREA_BARRIO)]),
    zonas = paste(sort(unique(as.character(ZONA_PEUAT[!is.na(ZONA_PEUAT)]))), 
                  collapse = ", "),
    areas = paste(sort(unique(round(AREA_BARRIO[!is.na(AREA_BARRIO)], 3))), 
                  collapse = ", "),
    .groups = "drop")
check_barrios

# Al comprobar observamos que existen varios casos aislados que se encuentran en 
# un barrio que pertenecen por un tamaño muy pequeño a varias zonas, 
# la mayoría de casos en la misma zona, excepto 1 o 2 casos. 
# Estos los eliminamos
air <- air %>%
  filter(!(neighbourhood_cleansed=="el Putxet i el Farró" & ZONA_PEUAT == "1"),
    !(neighbourhood_cleansed=="el Clot" & ZONA_PEUAT == "2"),
    !(neighbourhood_cleansed=="el Poblenou" & ZONA_PEUAT == "2"),
    !(neighbourhood_cleansed=="la Vila Olímpica del Poblenou"&ZONA_PEUAT=="2"))

# ==============================================================================
# 1.5 GRÁFICO1: DISTRIBUCIÓN AIRBNB 01/01/2015-31/12/2023 POR ZONAS PEUAT
# ==============================================================================
## Gráfico 1: Nuevas altas por zona y tipo de vivienda
airz <- air %>%
  st_drop_geometry() %>%
  mutate(ZONA_PEUAT = as.integer(as.character(ZONA_PEUAT)),
         room_type = as.character(room_type)) 

graf1_data <- airz %>%
  count(MES_ALTA, ZONA_PEUAT, room_type, name = "n") %>%
  complete(MES_ALTA = 1:108,
           ZONA_PEUAT = 1:4,
           room_type = c("Entire home/apt", "HC"),
           fill = list(n = 0))

plot_zona_tipo <- function(z) {
  dz <- graf1_data %>%
    filter(ZONA_PEUAT == z)
  ymax_z <- max(dz$n, na.rm = TRUE) * 1.25
  
  eventos <- data.frame(x = c(68, 90),
             y = c(ymax_z * 0.95, ymax_z * 0.82),
             etiqueta = c("COVID-19", "PEUAT\nen vigor"))
          
  ggplot(dz, aes(MES_ALTA, n, color = room_type)) +
    geom_line(linewidth = 0.8) +
    geom_vline(xintercept = 63, linetype = "dashed", color = "grey65") +
    geom_vline(xintercept = 86, linetype = "dashed", color = "grey65") +
    geom_text(data = eventos,
              aes(x = x, y = y, label = etiqueta),inherit.aes = FALSE,
              color = "grey55",fontface = "bold", size = 4, nudge_x = 5) +
    scale_color_manual(values = c("Entire home/apt" = "#36648B",
                                  "HC" = "#9FB6CD"),
      labels = c("Entire home/apt" = "Viviendas de uso turístico",
                 "HC" = "Hogares compartidos")) +
    scale_x_continuous(breaks = c(1,13,25,37,49,61,73,85,97),
                       labels = c("2015","2016","2017","2018","2019",
                                 "2020","2021","2022","2023")) +
    labs(title = paste("Zona", z), x = NULL, y = "Altas") +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid = element_blank(),
      axis.line = element_line(color="black"),
      axis.text.x = element_text(size=10),
      axis.text.y = element_text(size=10),
      plot.title = element_text(face="bold",hjust=.5,
                                size=13),legend.position = "bottom",
      panel.background = element_rect(fill = "transparent", color = NA),
      plot.background  = element_rect(fill = "transparent", color = NA),
      legend.background = element_rect(fill = "transparent", color = NA),
      legend.box.background = element_rect(fill = "transparent", color = NA))}

p1 <- plot_zona_tipo(1)
p2 <- plot_zona_tipo(2)
p3 <- plot_zona_tipo(3)
p4 <- plot_zona_tipo(4)

g1 <- (p1 + p2) / (p3 + p4) + 
      plot_layout(guides = "collect") &
      theme(legend.position = "bottom")

g1 <- g1 +
  plot_annotation(
    title = "Evolución mensual de nuevas altas de Airbnb por zona PEUAT",
    subtitle="Diferenciando entre viviendas de uso turístico y hogares compartidos",
    theme = theme(
      plot.title = element_text(hjust = 0.5,face = "bold",size = 16),
      plot.subtitle = element_text(hjust = 0.5,size = 14),
      plot.background = element_rect(fill = "transparent", color = NA)))
g1
ggsave(file.path(dir_graficos, "Graf1.png"),g1,width = 12,
       height = 10,dpi = 500, bg = "transparent")

# ==============================================================================
# 2. CONSTRUCTOS Y SUS RESPECTIVOS PROXIES
# ==============================================================================
# FUNCIONES AUXILIARES COMUNES
# Barrios que en el análisis se dividen en Nord/Sud por quedar partidos
# por la delimitación de las zonas PEUAT.
barrios_partidos <- c("Sant Antoni",
                      "Vallcarca i els Penitents",
                      "el Putxet i el Farró")

# Homogeneizamos nombres de barrios para asegurar el cruce entre fuentes.
homogeneizar_barrio <- function(x) {
  case_when(
    x == "el Poble-sec" ~ "el Poble Sec",
    x == "Sant Gervasi- Galvany" ~ "Sant Gervasi - Galvany",
    x == "Sant Gervasi- la Bonanova" ~ "Sant Gervasi - la Bonanova",
    x == "Sants-Badal" ~ "Sants - Badal",
    TRUE ~ x)}

# Creamos el código de sección censal en formato homogéneo.
# El %% 1000 permite quedarnos con los tres últimos dígitos cuando el código
# de sección censal viene en formato largo.
crear_sec_map <- function(x) {
  str_pad(as.integer(x) %% 1000, width = 3, pad = "0")}

# Asignamos manualmente las secciones censales a los subbarrios Nord/Sud.
# Información obtenida mediante el MAPA 1
# Esta función se utilizará en las variables disponibles por sección censal.
asignar_barrio_partido <- function(Nom_Barri, SEC_NUM) {
  case_when(
    Nom_Barri == "Sant Antoni" &
      SEC_NUM %in% c(154:157, 163:173) ~ "Sant Antoni Nord",
    
    Nom_Barri == "Sant Antoni" &
      SEC_NUM %in% c(150:153, 158:162) ~ "Sant Antoni Sud",
    
    Nom_Barri == "Vallcarca i els Penitents" &
      SEC_NUM %in% 6:12 ~ "Vallcarca i els Penitents Nord",
    
    Nom_Barri == "Vallcarca i els Penitents" &
      SEC_NUM %in% 1:4 ~ "Vallcarca i els Penitents Sud",
    
    Nom_Barri == "el Putxet i el Farró" &
      SEC_NUM %in% 91:98 ~ "el Putxet i el Farró Nord",
    
    Nom_Barri == "el Putxet i el Farró" &
      SEC_NUM %in% 80:90 ~ "el Putxet i el Farró Sud",
    TRUE ~ NA_character_)}

# ==============================================================================
# 2.1 CONSTRUCTO 1: POBLACIÓN TOTAL --> PROXY 1: POBLACIÓN
# ==============================================================================
# La población se utiliza como proxy del tamaño demográfico de cada barrio.
filesp <- list.files(file.path(dir_datos, "POBLACION"),full.names = TRUE)

poblacion <- map_dfr(filesp, function(f) {
    read_csv(f, show_col_types = FALSE) %>%
    mutate(AÑO_ALTA = as.numeric(str_extract(basename(f), "\\d{4}")))})

# Limpieza de población a nivel de sección censal
poblacion <- poblacion %>%
  transmute(AÑO_ALTA,
            Nom_Barri = str_squish(as.character(Nom_Barri)),
            Seccio_Censal = as.character(Seccio_Censal),
            Valor = as.numeric(Valor)) %>%
  mutate(Nom_Barri = homogeneizar_barrio(Nom_Barri),
         SEC_MAP = crear_sec_map(Seccio_Censal),
         SEC_NUM = as.integer(SEC_MAP)) 

# Barrios no partidos
poblacion_np <- poblacion %>%
  filter(!Nom_Barri %in% barrios_partidos) %>%
  group_by(AÑO_ALTA, Nom_Barri) %>%
  summarise(POBLACION = sum(Valor, na.rm = TRUE),.groups = "drop") %>%
  rename(BARRIO = Nom_Barri)

# Barrios partidos
poblacion_p <- poblacion %>%
  filter(Nom_Barri %in% barrios_partidos) %>%
  mutate(BARRIO = asignar_barrio_partido(Nom_Barri, SEC_NUM))

# Comprobamos secciones de barrios partidos que no se han asignado.
secciones_sin_asignar_poblacion <- poblacion_p %>%
  filter(is.na(BARRIO)) %>%
  distinct(Nom_Barri, Seccio_Censal, SEC_MAP, SEC_NUM) %>%
  arrange(Nom_Barri, SEC_NUM)
secciones_sin_asignar_poblacion

# Agregamos población de barrios partidos
poblacion_p <- poblacion_p %>%
  filter(!is.na(BARRIO)) %>%
  group_by(AÑO_ALTA, BARRIO) %>%
  summarise(POBLACION = sum(Valor, na.rm = TRUE),.groups = "drop")

# Unión final
poblacion_final <- bind_rows(poblacion_np, poblacion_p) %>%
                   arrange(BARRIO, AÑO_ALTA) %>%
                   select(AÑO_ALTA, BARRIO, POBLACION) %>%
                   distinct()

# ==============================================================================
# 2.2 CONSTRUCTO 2: MOVILIDAD POBLACIONAL --> PROXIES 2 Y 3
# ==============================================================================
# ------------------------------------------------------------------------------
# 2.2.1 PROXY 2: INMIGRACION
# ------------------------------------------------------------------------------
filesi <- list.files(file.path(dir_datos, "INMIGRACION"), full.names = TRUE)

inmigracion <- map_dfr(filesi, function(f) {
  read_csv(f, show_col_types = FALSE) %>%
    mutate(AÑO_ALTA = as.numeric(str_extract(basename(f), "\\d{4}")),
           Valor = Valor %>%
                   as.character() %>%
                   str_trim() %>%
                   gsub("\\.", "", .) %>%
                   as.numeric())})

# Limpieza de inmigración a nivel de sección censal
inmigracion <- inmigracion %>%
  transmute(AÑO_ALTA,
            Nom_Barri = str_squish(as.character(Nom_Barri)),
            Seccio_Censal = as.character(Seccio_Censal),
            Valor) %>%
  mutate(Nom_Barri = homogeneizar_barrio(Nom_Barri),
         SEC_MAP = crear_sec_map(Seccio_Censal),
         SEC_NUM = as.integer(SEC_MAP))

# Barrios no partidos
inmigracion_np <- inmigracion %>%
  filter(!Nom_Barri %in% barrios_partidos) %>%
  group_by(AÑO_ALTA, Nom_Barri) %>%
  summarise(INMIGRACION = sum(Valor, na.rm = TRUE) / 12,.groups = "drop") %>%
  rename(BARRIO = Nom_Barri)

# Barrios partidos
inmigracion_p <- inmigracion %>%
  filter(Nom_Barri %in% barrios_partidos) %>%
  mutate(BARRIO = asignar_barrio_partido(Nom_Barri, SEC_NUM))

# Comprobamos secciones de barrios partidos que no se han asignado.
secciones_sin_asignar_inmigracion <- inmigracion_p %>%
  filter(is.na(BARRIO)) %>%
  distinct(Nom_Barri, Seccio_Censal, SEC_MAP, SEC_NUM) %>%
  arrange(Nom_Barri, SEC_NUM)
secciones_sin_asignar_inmigracion

# Agregamos inmigración de barrios partidos
inmigracion_p <- inmigracion_p %>%
  filter(!is.na(BARRIO)) %>%
  group_by(AÑO_ALTA, BARRIO) %>%
  summarise(INMIGRACION = sum(Valor, na.rm = TRUE) / 12,.groups = "drop")

# Unión final
inmigracion_final <- bind_rows(inmigracion_np, inmigracion_p) %>%
                     arrange(BARRIO, AÑO_ALTA) %>%
                     select(AÑO_ALTA, BARRIO, INMIGRACION) %>%
                     distinct()
# ------------------------------------------------------------------------------
# 2.2.2 PROXY 3: INCREMENTO_NETO_POBLACION
# ------------------------------------------------------------------------------
# La movilidad interna no está disponible a nivel de sección censal, sino a nivel
# de barrio administrativo original. Por ello, en los barrios partidos se replica
# el mismo valor de movilidad neta para las dos partes Nord/Sud del barrio.
filesm <- list.files(file.path(dir_datos, "MOVILIDAD"), full.names = TRUE)

movilidad <- map_dfr(filesm, function(f) {
  read_csv(f, show_col_types = FALSE) %>%
    mutate(AÑO_ALTA = as.numeric(str_extract(basename(f), "\\d{4}")),
           Valor = Valor %>%
                   as.character() %>%
                   str_trim() %>%
                   na_if("..") %>%
                   as.numeric())})

# Limpieza inicial y eliminación de movimientos internos
movilidad <- movilidad %>%
  filter(Codi_Barri != CODI_BARRI_DEST) %>%
  mutate(Nom_Barri = homogeneizar_barrio(str_squish(as.character(Nom_Barri))))

# Salidas por barrio de origen
salidas <- movilidad %>%
  group_by(AÑO_ALTA, Codi_Barri) %>%
  summarise(SALIDAS = sum(Valor, na.rm = TRUE),.groups = "drop")

# Llegadas por barrio de destino
llegadas <- movilidad %>%
  group_by(AÑO_ALTA, CODI_BARRI_DEST) %>%
  summarise(LLEGADAS = sum(Valor, na.rm = TRUE),.groups = "drop")

# Diccionario código de barrio - nombre original
dic_barrios_movilidad <- movilidad %>%
  select(Codi_Barri, Nom_Barri) %>%
  distinct()

# Movilidad final por barrio administrativo original
movilidad_original <- salidas %>%
  left_join(llegadas, by = c("AÑO_ALTA", "Codi_Barri" = "CODI_BARRI_DEST")) %>%
  mutate(LLEGADAS = replace_na(LLEGADAS, 0),
         SALIDAS = replace_na(SALIDAS, 0),
         INCREMENTO_NETO_POBLACION = (LLEGADAS - SALIDAS) / 12) %>%
  left_join(dic_barrios_movilidad, by = "Codi_Barri") %>%
  rename(neighbourhood_cleansed = Nom_Barri)

# Diccionario entre barrio original y BARRIO final usado en air.
# Esto permite asignar también los barrios partidos Nord/Sud.
dic_barrio_air <- air %>%
  { if (inherits(., "sf")) st_drop_geometry(.) else . } %>%
  select(neighbourhood_cleansed, BARRIO) %>%
  distinct()

# Añadimos BARRIO final.
# Si un neighbourhood_cleansed tiene dos BARRIO, se duplican las filas
# y se repite el mismo INCREMENTO_NETO_POBLACION.
movilidad_final <- movilidad_original %>%
                   left_join(dic_barrio_air, by = "neighbourhood_cleansed") %>%
                   mutate(BARRIO = if_else(is.na(BARRIO), neighbourhood_cleansed, 
                                           as.character(BARRIO))) %>%
                   select(AÑO_ALTA, BARRIO, INCREMENTO_NETO_POBLACION) %>%
                   distinct()

# ==============================================================================
# 2.3 CONSTRUCTO 3: NIVEL ECONÓMICO --> PROXIES 4 Y 5
# ==============================================================================
# ------------------------------------------------------------------------------
# 2.3.1 PROXY 4: RENTA_MEDIA
# ------------------------------------------------------------------------------
filesr <- list.files(file.path(dir_datos, "RENDA_BRUTA_PERSONA"), full.names = TRUE)

renta <- map_dfr(filesr, function(f) {
  read_csv(f, show_col_types = FALSE) %>%
    mutate(AÑO_ALTA = as.numeric(str_extract(basename(f), "\\d{4}")))})

# Limpieza de renta a nivel de sección censal
renta <- renta %>%
  transmute(AÑO_ALTA,
            Nom_Barri = str_squish(as.character(Nom_Barri)),
            Seccio_Censal = as.character(Seccio_Censal),
            Import_Renda_Bruta_EUR = as.numeric(`Import_Renda_Bruta_€`)) %>%
  mutate(Nom_Barri = homogeneizar_barrio(Nom_Barri),
         SEC_MAP = crear_sec_map(Seccio_Censal),
         SEC_NUM = as.integer(SEC_MAP))

# Añadimos población de sección para calcular renta media ponderada.
# Se usa poblacion a nivel sección, no poblacion_final.
renta <- renta %>%
  left_join(poblacion %>%
            select(AÑO_ALTA, Nom_Barri, SEC_MAP, Valor),
            by = c("AÑO_ALTA", "Nom_Barri", "SEC_MAP")) %>%
  rename(POBLACION_SECCION = Valor)

# Comprobamos secciones sin población asociada
renta %>%
  filter(is.na(POBLACION_SECCION)) %>%
  distinct(AÑO_ALTA, Nom_Barri, Seccio_Censal, SEC_MAP, SEC_NUM) %>%
  arrange(AÑO_ALTA, Nom_Barri, SEC_NUM)

# Barrios no partidos
renta_np <- renta %>%
  filter(!Nom_Barri %in% barrios_partidos) %>%
  group_by(AÑO_ALTA, Nom_Barri) %>%
  summarise(
    RENTA_MEDIA = sum(Import_Renda_Bruta_EUR * POBLACION_SECCION, na.rm = TRUE)/
                  sum(POBLACION_SECCION, na.rm = TRUE),.groups = "drop") %>%
  rename(BARRIO = Nom_Barri)

# Barrios partidos
renta_p <- renta %>%
  filter(Nom_Barri %in% barrios_partidos) %>%
  mutate(BARRIO = asignar_barrio_partido(Nom_Barri, SEC_NUM))

# Comprobamos secciones de barrios partidos que no se han asignado.
renta_sin_asignar <- renta_p%>%
  filter(is.na(BARRIO)) %>%
  distinct(Nom_Barri, Seccio_Censal, SEC_MAP, SEC_NUM) %>%
  arrange(Nom_Barri, SEC_NUM)
renta_sin_asignar

# Agregamos renta media ponderada de barrios partidos
renta_p <- renta_p %>%
  filter(!is.na(BARRIO)) %>%
  group_by(AÑO_ALTA, BARRIO) %>%
  summarise(
    RENTA_MEDIA = sum(Import_Renda_Bruta_EUR * POBLACION_SECCION, na.rm = TRUE)/
                  sum(POBLACION_SECCION, na.rm = TRUE),.groups = "drop")

# Unión final
renta_final <- bind_rows(renta_np, renta_p) %>%
               arrange(BARRIO, AÑO_ALTA) %>%
               select(AÑO_ALTA, BARRIO, RENTA_MEDIA) %>%
               distinct()
# ------------------------------------------------------------------------------
# 2.3.2 PROXY 5: GINI
# ------------------------------------------------------------------------------
filesg <- list.files(file.path(dir_datos, "GINI"), full.names = TRUE)

gini <- map_dfr(filesg, function(f) {
  read_csv(f, show_col_types = FALSE) %>%
    mutate(AÑO_ALTA = as.numeric(str_extract(basename(f), "\\d{4}")),
           Index_Gini = Index_Gini %>%
                        as.character() %>%
                        str_trim() %>%
                        na_if("..") %>%
                        as.numeric())})

# Limpieza de Gini a nivel de sección censal
gini <- gini %>%
  transmute(AÑO_ALTA,
            Nom_Barri = str_squish(as.character(Nom_Barri)),
            Seccio_Censal = as.character(Seccio_Censal),
            GINI_ORIGINAL = as.numeric(Index_Gini)) %>%
  mutate(Nom_Barri = homogeneizar_barrio(Nom_Barri),
         SEC_MAP = crear_sec_map(Seccio_Censal),
         SEC_NUM = as.integer(SEC_MAP))

# Añadimos población de sección para calcular Gini ponderado.
# Se usa poblacion a nivel sección, no poblacion_final.
gini <- gini %>%
  left_join(poblacion %>%
            select(AÑO_ALTA, Nom_Barri, SEC_MAP, Valor),
            by = c("AÑO_ALTA", "Nom_Barri", "SEC_MAP")) %>%
  rename(POBLACION_SECCION = Valor)

# Comprobamos secciones sin población asociada
gini %>%
  filter(is.na(POBLACION_SECCION)) %>%
  distinct(AÑO_ALTA, Nom_Barri, Seccio_Censal, SEC_MAP, SEC_NUM) %>%
  arrange(AÑO_ALTA, Nom_Barri, SEC_NUM)

# Barrios no partidos
gini_np <- gini %>%
  filter(!Nom_Barri %in% barrios_partidos) %>%
  group_by(AÑO_ALTA, Nom_Barri) %>%
  summarise(GINI = (sum(GINI_ORIGINAL * POBLACION_SECCION, na.rm = TRUE) /
                    sum(POBLACION_SECCION, na.rm = TRUE))/100,.groups="drop")%>%
  rename(BARRIO = Nom_Barri)

# Barrios partidos
gini_p <- gini %>%
  filter(Nom_Barri %in% barrios_partidos) %>%
  mutate(BARRIO = asignar_barrio_partido(Nom_Barri, SEC_NUM))

# Comprobamos secciones de barrios partidos que no se han asignado.
gini_sin_asignar <- gini_p %>%
  filter(is.na(BARRIO)) %>%
  distinct(Nom_Barri, Seccio_Censal, SEC_MAP, SEC_NUM) %>%
  arrange(Nom_Barri, SEC_NUM)
gini_sin_asignar

# Agregamos Gini ponderado de barrios partidos
gini_p <- gini_p %>%
  filter(!is.na(BARRIO)) %>%
  group_by(AÑO_ALTA, BARRIO) %>%
  summarise(GINI = (sum(GINI_ORIGINAL * POBLACION_SECCION, na.rm = TRUE) /
                  sum(POBLACION_SECCION, na.rm = TRUE)) / 100,.groups = "drop")

# Unión final
gini_final <- bind_rows(gini_np, gini_p) %>%
              arrange(BARRIO, AÑO_ALTA) %>%
              select(AÑO_ALTA, BARRIO, GINI) %>%
              distinct()


# ==============================================================================
# 3. CONTRUCCIÓN DE PANELES DE DATOS PARA FUTUROS MODELOS
# ==============================================================================
# PREPARACIÓN DE AIR PARA MODELOS
# Quitamos la zona 4, ya que no formará parte de los modelos principales.
# Conservamos únicamente las zonas 1, 2 y 3:
#   - Zonas 1 y 2: zonas tratadas
#   - Zona 3: zona de comparación

air_model <- air %>%
  filter(ZONA_PEUAT %in% c("1", "2", "3")) %>%
  mutate(ZONA_PEUAT = droplevels(ZONA_PEUAT))

# Si air todavía conserva geometría, la eliminamos.
# A partir de aquí trabajamos con dataframes, no con objetos espaciales.
air_model <- if (inherits(air_model, "sf")) {
                st_drop_geometry(air_model)} else {air_model}

# Comprobamos que cada BARRIO tenga una sola zona PEUAT y una sola área.
# Esto es importante porque BARRIO será la unidad geográfica del panel.
check_barrios2 <- air_model %>%
  group_by(BARRIO) %>%
  summarise(n_zonas = n_distinct(ZONA_PEUAT[!is.na(ZONA_PEUAT)]),
          n_areas = n_distinct(AREA_BARRIO[!is.na(AREA_BARRIO)]),
          zonas=paste(sort(unique(as.character(ZONA_PEUAT[!is.na(ZONA_PEUAT)]))),
                    collapse = ", "),
          areas=paste(sort(unique(round(AREA_BARRIO[!is.na(AREA_BARRIO)], 3))),
                    collapse = ", "),.groups = "drop")
check_barrios2
# Barrios problemáticos: más de una zona o más de un área
check_barrios2 %>%
  filter(n_zonas > 1 | n_areas > 1)

# ==============================================================================
# 3.1 TABLAS AUXILIARES (ZONA Y ÁREA POR BARRIO + PROXIES)
# ==============================================================================
# Tabla única BARRIO -> ZONA_PEUAT
zona_barrio <- air_model %>%
  filter(!is.na(BARRIO), !is.na(ZONA_PEUAT)) %>%
  distinct(BARRIO, ZONA_PEUAT) %>%
  group_by(BARRIO) %>%
  summarise(ZONA_PEUAT = first(ZONA_PEUAT),.groups = "drop")

# Tabla única BARRIO -> AREA_BARRIO
area_barrio <- air_model %>%
  filter(!is.na(BARRIO), !is.na(AREA_BARRIO)) %>%
  distinct(BARRIO, AREA_BARRIO) %>%
  group_by(BARRIO) %>%
  summarise(AREA_BARRIO = first(AREA_BARRIO),.groups = "drop")

# Creamos la lista de barrios candidatos a entrar en el panel a partir de las
# tablas finales de proxies.
barrios_panel <- bind_rows(
  poblacion_final %>% select(BARRIO),
  movilidad_final %>% select(BARRIO),
  inmigracion_final %>% select(BARRIO),
  renta_final %>% select(BARRIO),
  gini_final %>% select(BARRIO)) %>%
  distinct() %>%
  mutate(BARRIO = as.character(BARRIO))

# Creamos una tabla BARRIO x AÑO_ALTA con todas las proxies anuales.
proxies <- barrios_panel %>%
  crossing(AÑO_ALTA = sort(unique(poblacion_final$AÑO_ALTA))) %>%
  left_join(poblacion_final, by = c("BARRIO", "AÑO_ALTA")) %>%
  left_join(movilidad_final, by = c("BARRIO", "AÑO_ALTA")) %>%
  left_join(inmigracion_final, by = c("BARRIO", "AÑO_ALTA")) %>%
  left_join(renta_final, by = c("BARRIO", "AÑO_ALTA")) %>%
  left_join(gini_final, by = c("BARRIO", "AÑO_ALTA"))

# ==============================================================================
# 3.2 SEPARACIÓN POR TIPO DE ALOJAMIENTO
# ==============================================================================
air_entire_list <- air_model %>%
  filter(room_type == "Entire home/apt") %>%
  mutate(room_type = droplevels(room_type),
         BARRIO = as.character(BARRIO))

air_hc_list <- air_model %>%
  filter(room_type == "HC") %>%
  mutate(room_type = droplevels(room_type),
         BARRIO = as.character(BARRIO))

# ==============================================================================
# 3.3 PANEL BARRIO x MES: ENTIRE HOME/APT
# ==============================================================================
altas_entire <- air_entire_list %>%
  group_by(BARRIO, MES_ALTA) %>%
  summarise(NUEVAS_ALTAS = n(),
            NUEVOS_MULTIHOST = sum(MULTI_HOST, na.rm = TRUE),
            NUEVAS_LICENSE = sum(HAS_LICENSE, na.rm = TRUE),.groups = "drop")

panel_entire <- barrios_panel %>%
  crossing(MES_ALTA = 1:108) %>%
  mutate(AÑO_ALTA = 2015 + (MES_ALTA - 1) %/% 12) %>%
  left_join(altas_entire, by = c("BARRIO", "MES_ALTA")) %>%
  mutate(NUEVAS_ALTAS = replace_na(NUEVAS_ALTAS, 0),
         NUEVOS_MULTIHOST = replace_na(NUEVOS_MULTIHOST, 0),
         NUEVAS_LICENSE = replace_na(NUEVAS_LICENSE, 0)) %>%
  left_join(zona_barrio, by = "BARRIO") %>%
  left_join(area_barrio, by = "BARRIO") %>%
  left_join(proxies, by = c("BARRIO", "AÑO_ALTA")) %>%
  arrange(BARRIO, MES_ALTA) %>%
  group_by(BARRIO) %>%
  mutate(STOCK_PREV = lag(cumsum(NUEVAS_ALTAS), default = 0),
         STOCK_MULTIHOST_PREV = lag(cumsum(NUEVOS_MULTIHOST), default = 0),
         STOCK_LICENSE_PREV = lag(cumsum(NUEVAS_LICENSE), default = 0),
         PROP_MULTIHOST = if_else(STOCK_PREV > 0,
                                  STOCK_MULTIHOST_PREV / STOCK_PREV,NA_real_),
         PROP_LICENSE = if_else(STOCK_PREV > 0,
                                STOCK_LICENSE_PREV / STOCK_PREV,NA_real_)) %>%
  ungroup() %>%
  mutate(DENSIDAD_STOCK = if_else(!is.na(AREA_BARRIO) & AREA_BARRIO > 0,
                                  STOCK_PREV / AREA_BARRIO,NA_real_),
         ZONA = as.integer(ZONA_PEUAT %in% c("1", "2")),
         TREAT = as.integer(MES_ALTA >= 86)) %>%
  select(-AREA_BARRIO,-NUEVOS_MULTIHOST,-NUEVAS_LICENSE,-STOCK_PREV,
         -STOCK_MULTIHOST_PREV,-STOCK_LICENSE_PREV) %>%
  select(
    # 1. Identificadores unidad-tiempo
    BARRIO, ZONA_PEUAT, AÑO_ALTA, MES_ALTA,
    # 2. Variable respuesta
    NUEVAS_ALTAS,
    # 3. Variables centrales DID
    TREAT,ZONA,
    # 4. Exposición / población
    POBLACION,
    # 5. Covariables construidas a partir de Airbnb
    DENSIDAD_STOCK,PROP_MULTIHOST,PROP_LICENSE,
    # 6. Proxies demográficas y socioeconómicas
    INMIGRACION,INCREMENTO_NETO_POBLACION,RENTA_MEDIA,GINI)

# ------------------------------------------------------------------------------
# 3.3.1 Eliminación de barrios sin ninguna alta en todo el periodo
# ------------------------------------------------------------------------------
# Identificamos los barrios que tienen 0 nuevas altas en todos los meses 1-108.
barrios_sin_altas_entire <- panel_entire %>%
  group_by(BARRIO) %>%
  summarise(TOTAL_ALTAS = sum(NUEVAS_ALTAS, na.rm = TRUE),.groups = "drop") %>%
  filter(TOTAL_ALTAS == 0)
barrios_sin_altas_entire

# Eliminamos esos barrios del panel
panel_entire <- panel_entire %>%
  anti_join(barrios_sin_altas_entire, by = "BARRIO")

# Barrios finales
panel_entire %>%
  summarise(n_barrios_final = n_distinct(BARRIO))

# ==============================================================================
# 3.4 PANEL BARRIO x MES: HC
# ==============================================================================
altas_hc <- air_hc_list %>%
  group_by(BARRIO, MES_ALTA) %>%
  summarise(NUEVAS_ALTAS = n(),
            NUEVOS_MULTIHOST = sum(MULTI_HOST, na.rm = TRUE),
            NUEVAS_LICENSE = sum(HAS_LICENSE, na.rm = TRUE),.groups = "drop")

panel_hc <- barrios_panel %>%
  crossing(MES_ALTA = 1:108) %>%
  mutate(AÑO_ALTA = 2015 + (MES_ALTA - 1) %/% 12) %>%
  left_join(altas_hc, by = c("BARRIO", "MES_ALTA")) %>%
  mutate(NUEVAS_ALTAS = replace_na(NUEVAS_ALTAS, 0),
         NUEVOS_MULTIHOST = replace_na(NUEVOS_MULTIHOST, 0),
         NUEVAS_LICENSE = replace_na(NUEVAS_LICENSE, 0)) %>%
  left_join(zona_barrio, by = "BARRIO") %>%
  left_join(area_barrio, by = "BARRIO") %>%
  left_join(proxies, by = c("BARRIO", "AÑO_ALTA")) %>%
  arrange(BARRIO, MES_ALTA) %>%
  group_by(BARRIO) %>%
  mutate(STOCK_PREV = lag(cumsum(NUEVAS_ALTAS), default = 0),
         STOCK_MULTIHOST_PREV = lag(cumsum(NUEVOS_MULTIHOST), default = 0),
         STOCK_LICENSE_PREV = lag(cumsum(NUEVAS_LICENSE), default = 0),
         PROP_MULTIHOST = if_else(STOCK_PREV > 0,
                                  STOCK_MULTIHOST_PREV / STOCK_PREV,NA_real_),
         PROP_LICENSE = if_else(STOCK_PREV > 0,
                                STOCK_LICENSE_PREV / STOCK_PREV,NA_real_)) %>%
  ungroup() %>%
  mutate(DENSIDAD_STOCK = if_else(!is.na(AREA_BARRIO) & AREA_BARRIO > 0,
                                  STOCK_PREV / AREA_BARRIO,NA_real_),
         ZONA = as.integer(ZONA_PEUAT %in% c("1", "2")),
         TREAT = as.integer(MES_ALTA >= 86)) %>%
  select(-AREA_BARRIO,-NUEVOS_MULTIHOST,-NUEVAS_LICENSE,-STOCK_PREV,
         -STOCK_MULTIHOST_PREV,-STOCK_LICENSE_PREV) %>%
  select(
    # 1. Identificadores unidad-tiempo
    BARRIO,ZONA_PEUAT,AÑO_ALTA,MES_ALTA,
    # 2. Variable respuesta
    NUEVAS_ALTAS,
    # 3. Variables centrales DID
    TREAT,ZONA,
    # 4. Exposición / población
    POBLACION,
    # 5. Covariables construidas a partir de Airbnb
    DENSIDAD_STOCK,PROP_MULTIHOST,PROP_LICENSE,
    # 6. Proxies demográficas y socioeconómicas
    INMIGRACION,INCREMENTO_NETO_POBLACION,RENTA_MEDIA,GINI)

# ------------------------------------------------------------------------------
# 3.4.1 Eliminación de barrios sin ninguna alta en todo el periodo
# ------------------------------------------------------------------------------
# Identificamos los barrios que tienen 0 nuevas altas en todos los meses 1-108.
barrios_sin_altas_hc <- panel_hc %>%
  group_by(BARRIO) %>%
  summarise(TOTAL_ALTAS = sum(NUEVAS_ALTAS, na.rm = TRUE),.groups = "drop") %>%
  filter(TOTAL_ALTAS == 0)
barrios_sin_altas_hc

# Eliminamos esos barrios del panel
panel_hc <- panel_hc %>%
  anti_join(barrios_sin_altas_hc, by = "BARRIO")

# Barrios finales
panel_hc %>%
  summarise(n_barrios_final = n_distinct(BARRIO))

# ==============================================================================
# 3.5 RESUMEN FINAL DE PANELES
# ==============================================================================
resumen_paneles <- tibble(
  PANEL = c("Entire home/apt", "HC"),
  N_OBSERVACIONES = c(nrow(panel_entire), nrow(panel_hc)),
  N_BARRIOS = c(n_distinct(panel_entire$BARRIO),
                n_distinct(panel_hc$BARRIO)),
  N_MESES = c(n_distinct(panel_entire$MES_ALTA),
              n_distinct(panel_hc$MES_ALTA)),
  TOTAL_ALTAS = c(sum(panel_entire$NUEVAS_ALTAS, na.rm = TRUE),
                  sum(panel_hc$NUEVAS_ALTAS, na.rm = TRUE)),
  BARRIOS_ELIMINADOS = c(nrow(barrios_sin_altas_entire),
                         nrow(barrios_sin_altas_hc)))
resumen_paneles


# ==============================================================================
# 4. ANÁLISIS DESCRIPTIVO
# ==============================================================================
# ==============================================================================
# 4.1 Evolución de las nuevas altas
# ==============================================================================
# Función general para cada gráfico
grafico2 <- function(datos, zona , titulo, mostrar_y = TRUE) {
  
  datosg <- datos %>%
    filter(ZONA == zona,
           !is.na(NUEVAS_ALTAS)) %>%
    mutate(
      GRUPO_ALTAS = case_when(
        zona == "1" & NUEVAS_ALTAS <= 5 ~ as.character(NUEVAS_ALTAS),
        zona == "1" & NUEVAS_ALTAS >= 6 ~ ">=6",
        zona == "0" & NUEVAS_ALTAS <= 2 ~ as.character(NUEVAS_ALTAS),
        zona == "0" & NUEVAS_ALTAS >= 3 ~ ">=3"),
      GRUPO_ALTAS = factor(GRUPO_ALTAS,
        levels = if (zona == "1") {
          c("0", "1", "2", "3", "4", "5", ">=6")} else {
          c("0", "1", "2",">=3")})) %>%
    count(TREAT, GRUPO_ALTAS) %>%
    group_by(TREAT) %>%
    mutate(prob = n / sum(n)) %>%
    ungroup() %>%
    complete(TREAT,GRUPO_ALTAS, fill = list(n = 0, prob = 0)) %>%
    mutate(TREAT = factor(TREAT,levels = c("0", "1"),
           labels = c("Antes del PEUAT", "Después del PEUAT")))
  
  ggplot(datosg,
         aes(x = factor(GRUPO_ALTAS),
             y = prob,
             fill = TREAT)) +
    geom_col(position = position_dodge(width = 0.72),width = 0.60,
             color = "black",linewidth = 0.18) +
    scale_fill_manual(values = c("Antes del PEUAT" = "#D8D0D4",
                                 "Después del PEUAT" = "#A45B8D")) +
    scale_y_continuous(labels = percent_format(accuracy = 1),
                       expand = expansion(mult = c(0, 0.08))) +
    labs(title = titulo,x = "Número de nuevas altas",
         y = ifelse(mostrar_y, "Porcentaje de observaciones", ""),fill = NULL) +
    theme_minimal(base_size = 12) +
    theme(panel.grid = element_blank(),
          axis.line = element_line(color = "black", linewidth = 0.25),
          axis.ticks = element_line(color = "black", linewidth = 0.25),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          axis.title.x = element_text(size = 11, margin = margin(t = 3)),
          axis.title.y = element_text(size = 11, margin = margin(r = 3)),
          plot.title = element_text(hjust = 0.5,face = "bold",
                                    size = 11,margin = margin(b = 4)),
          legend.position = "bottom",
          legend.text = element_text(size = 11),
          legend.key.size = unit(0.35, "cm"),
          legend.margin = margin(t = -4),
          plot.margin = margin(4, 6, 4, 6),
          panel.background = element_rect(fill = "transparent", color = NA),
          plot.background  = element_rect(fill = "transparent", color = NA),
          legend.background = element_rect(fill = "transparent", color = NA),
          legend.box.background=element_rect(fill = "transparent", color = NA))}

g1 <- grafico2(datos = panel_entire,
               zona = "1",
               titulo = "Viviendas de uso turítico - Zonas tratadas (Z1 y Z2)",
               mostrar_y = TRUE)

g2 <- grafico2(datos = panel_entire,
               zona = "0",
               titulo = "Viviendas de uso turítico - Zona de control (Z3)",
               mostrar_y = FALSE)

g3 <- grafico2(datos = panel_hc,
               zona = "1",
               titulo = "Hogares Compartidos - Zonas tratadas (Z1 y Z2)",
               mostrar_y = TRUE)

g4 <- grafico2(datos = panel_hc,
               zona = "0",
               titulo = "Hogares Compartidos - Zona de control (Z3)",
               mostrar_y = FALSE)

grafico2 <- (g1 | g2) / (g3 | g4) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Distribución de nuevas altas antes y después del PEUAT",
    subtitle = "Comparación por tipo de alojamiento y zona de análisis",
    theme = theme(
      plot.title = element_text(hjust = 0.5,face = "bold",
                                size = 16,margin = margin(b = 3)),
      plot.subtitle=element_text(hjust = 0.5,size = 14,margin = margin(b = 6)),
      plot.margin = margin(4, 4, 4, 4),
      plot.background = element_rect(fill = "transparent", color = NA))) &
  theme(legend.position = "bottom",
        legend.text = element_text(size = 11),
        legend.key.size = unit(0.38, "cm"))

grafico2

ggsave(file.path(dir_graficos, "Graf2.png"),
       plot = grafico2,width = 11, height = 6.5,dpi = 500, bg = "transparent")

# ==============================================================================
# 4.2 Evolución de las covariables construidas a partir de Airbnb
# ==============================================================================
# FUNCIÓN AUXILIAR GRÁFICOS PROP_MULTIHOST Y PROP_LICENSE
grafico3 <- function(panel_entire, panel_hc, variable, y_label,
                                titulo, subtitulo = NULL,
                                nombre = NULL,
                                escala_y = 1, ejey_fijo = FALSE) {
  
  datos_plot <- bind_rows(
    panel_entire %>% mutate(TIPO_ALOJAMIENTO = "Entire home/apt"),
    panel_hc %>% mutate(TIPO_ALOJAMIENTO = "HC")) %>%
    mutate(
      GRUPO_ZONA = case_when(
        ZONA_PEUAT %in% c("1", "2") ~ "Zonas tratadas",
        ZONA_PEUAT == "3" ~ "Zona de control"),
      PERIODO = case_when(
        TREAT == 0 | TREAT == "0" ~ "Antes del PEUAT",
        TREAT == 1 | TREAT == "1" ~ "Después del PEUAT"),
      TIPO_ALOJAMIENTO = factor(
        TIPO_ALOJAMIENTO,
        levels = c("Entire home/apt", "HC")),
      GRUPO_ZONA = factor(
        GRUPO_ZONA,
        levels = c("Zonas tratadas", "Zona de control")),
      PERIODO = factor(
        PERIODO,
        levels = c("Antes del PEUAT", "Después del PEUAT")),
      VALOR_GRAFICO = .data[[variable]] * escala_y) %>%
    filter(!is.na(VALOR_GRAFICO),
           !is.na(GRUPO_ZONA),
           !is.na(PERIODO),
           !is.na(TIPO_ALOJAMIENTO))

  bp <- function(tipo, zona, titulo, mostrar_y = TRUE) {
    
    dz <- datos_plot %>%
      filter(TIPO_ALOJAMIENTO == tipo,
             GRUPO_ZONA == zona)
    
    vals <- dz$VALOR_GRAFICO
    
    if (ejey_fijo) {
      lim_inf <- 0
      lim_sup <- 1
    } else {
      lim_inf <- min(vals, na.rm = TRUE)
      lim_sup <- max(vals, na.rm = TRUE)
      margen <- (lim_sup - lim_inf) * 0.08
      if (margen == 0) {
        margen <- abs(lim_sup) * 0.08
      }
      if (margen == 0) {
        margen <- 1
      }
      lim_inf <- lim_inf - margen
      lim_sup <- lim_sup + margen
      if (min(vals, na.rm = TRUE) >= 0) {
        lim_inf <- 0
      }
    }
    
    ggplot(dz, aes(x = PERIODO,
                   y = VALOR_GRAFICO,
                   fill = PERIODO)) +
      geom_boxplot(color = "black",linewidth = 0.25,width = 0.50,
                   outlier.alpha = 0.30,outlier.size = 0.70) +
      scale_fill_manual(values = c("Antes del PEUAT" = "#D8D0D4",
                                   "Después del PEUAT" = "#A45B8D")) +
      coord_cartesian(ylim = c(lim_inf, lim_sup)) +
      labs(title = titulo, x = NULL,
           y = ifelse(mostrar_y, y_label, ""),fill = NULL) +
      theme_minimal(base_size = 12) +
      theme(panel.grid = element_blank(),
            axis.line = element_line(color = "black", linewidth = 0.25),
            axis.ticks = element_line(color = "black", linewidth = 0.25),
            axis.text.x = element_text(size = 10),
            axis.text.y = element_text(size = 10),
            axis.title.y = element_text(size = 11, margin = margin(r = 3)),
            plot.title = element_text(hjust = 0.5,face = "bold",
                                      size = 11,margin = margin(b = 4)),
            legend.position = "bottom",
            legend.text = element_text(size = 11),
            legend.key.size = unit(0.35, "cm"),
            plot.margin = margin(4, 6, 4, 6),
            panel.background = element_rect(fill = "transparent", color = NA),
            plot.background  = element_rect(fill = "transparent", color = NA),
            legend.background = element_rect(fill = "transparent", color = NA),
            legend.box.background=element_rect(fill = "transparent", color=NA))}
  
  g1 <- bp(
    tipo = "Entire home/apt",
    zona = "Zonas tratadas",
    titulo = "Viviendas de uso turístico - Zonas tratadas (Z1 y Z2)",
    mostrar_y = TRUE)
  
  g2 <- bp(
    tipo = "Entire home/apt",
    zona = "Zona de control",
    titulo = "Viviendas de uso turístico - Zona de control (Z3)",
    mostrar_y = TRUE)
  
  g3 <- bp(
    tipo = "HC",
    zona = "Zonas tratadas",
    titulo = "Hogares Compartidos - Zonas tratadas (Z1 y Z2)",
    mostrar_y = TRUE)
  
  g4 <- bp(
    tipo = "HC",
    zona = "Zona de control",
    titulo = "Hogares Compartidos - Zona de control (Z3)",
    mostrar_y = TRUE)
  
  grafico3 <- (g1 | g2) / (g3 | g4) +
    plot_layout(guides = "collect") +
    plot_annotation(title = titulo,subtitle = subtitulo,
      theme = theme(
        plot.title = element_text(hjust = 0.5,face = "bold",
                                  size = 16,margin = margin(b = 3)),
        plot.subtitle = element_text(hjust = 0.5,size=14,margin=margin(b = 6)),
        plot.margin = margin(4, 4, 4, 4),
        plot.background = element_rect(fill = "transparent", color = NA))) &
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 11),
        legend.key.size = unit(0.38, "cm"))
  
  if (!is.null(nombre)) {
    ggsave(filename = nombre,plot = grafico3,width = 11,
           height = 6.5,dpi = 500, bg = "transparent")}
  
  return(grafico3)}

# ------------------------------------------------------------------------------
# 4.2.1 PROPORCIÓN DE VIVIENDAS DE USO TURÍSTICO
# ------------------------------------------------------------------------------
graf3_1_data <- bind_rows(
  panel_entire %>%
    mutate(TIPO_ALOJAMIENTO = "Entire home/apt"),
  panel_hc %>%
    mutate(TIPO_ALOJAMIENTO = "HC")) %>%
  mutate(GRUPO_ZONA = case_when(
      ZONA_PEUAT %in% c("1", "2", 1, 2) ~ "Zonas tratadas (Z1 y Z2)",
      ZONA_PEUAT %in% c("3", 3) ~ "Zona de control (Z3)")) %>%
  filter(!is.na(GRUPO_ZONA),!is.na(MES_ALTA),!is.na(NUEVAS_ALTAS),
    TIPO_ALOJAMIENTO %in% c("Entire home/apt", "HC")) %>%
  group_by(MES_ALTA, GRUPO_ZONA, TIPO_ALOJAMIENTO) %>%
  summarise(ALTAS = sum(NUEVAS_ALTAS, na.rm = TRUE),.groups = "drop") %>%
  complete(MES_ALTA = 1:108,
    GRUPO_ZONA = c("Zonas tratadas (Z1 y Z2)", "Zona de control (Z3)"),
    TIPO_ALOJAMIENTO = c("Entire home/apt", "HC"),
    fill = list(ALTAS = 0)) %>%
  pivot_wider(names_from=TIPO_ALOJAMIENTO,values_from=ALTAS,values_fill=0) %>%
  mutate(TOTAL_ALTAS = `Entire home/apt` + HC,
    PROP_VUT = ifelse(TOTAL_ALTAS > 0,`Entire home/apt` / TOTAL_ALTAS,NA_real_),
    GRUPO_ZONA = factor(GRUPO_ZONA,
      levels = c("Zonas tratadas (Z1 y Z2)", "Zona de control (Z3)")))

ymax <- max(graf3_1_data$PROP_VUT, na.rm = TRUE) * 1.12

eventos <- data.frame(
  x = c(63, 85),
  y = c(ymax * 0.95, ymax * 0.97),
  etiqueta = c("COVID-19", "PEUAT\nen vigor"))

grafico3_1 <- ggplot(graf3_1_data,
  aes(x = MES_ALTA,y = PROP_VUT,color = GRUPO_ZONA)) +
  geom_line(linewidth = 0.85, na.rm = TRUE) +
  geom_point(size = 1.15, na.rm = TRUE) +
  geom_vline(xintercept = 63,linetype = "dashed",color = "grey65") +
  geom_vline(xintercept = 86,linetype = "dashed",color = "grey65") +
  geom_text(data = eventos,aes(x = x, y = y, label = etiqueta),
            inherit.aes = FALSE,color = "grey55",fontface = "bold",
            size = 4,nudge_x = 5) +
  scale_color_manual(
    values = c("Zonas tratadas (Z1 y Z2)" = "#2E8B57",
               "Zona de control (Z3)" = "#7CCD7C")) +
  scale_x_continuous(
    breaks = c(1, 13, 25, 37, 49, 61, 73, 85, 97),
    labels = c("2015", "2016", "2017", "2018", "2019",
               "2020", "2021", "2022", "2023")) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, ymax),
    expand = expansion(mult = c(0, 0.04))) +
  labs(
    title = "Evolución mensual de la proporción de viviendas de uso turístico",
    subtitle="Comparación entre zonas tratadas (Z1 y Z2) y zona de control (Z3)",
    x = NULL,
    y = "Proporción de viviendas de uso turístico",
    color = NULL) +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 11, margin = margin(r = 3)),
    plot.title = element_text(hjust = 0.5,face = "bold",size = 16),
    plot.subtitle = element_text(hjust = 0.5,size = 14),
    legend.position = "bottom",
    legend.text = element_text(size = 11),
    legend.key.size = unit(0.38, "cm"),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background  = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA))

grafico3_1

ggsave(file.path(dir_graficos, "Graf3_1.png"),
       grafico3_1,width = 11,height = 6.5,dpi = 500,bg = "transparent")

# ------------------------------------------------------------------------------
# 4.2.2 PROP_MULTIHOST
# ------------------------------------------------------------------------------
gbp_multihost <- grafico3(
  panel_entire = panel_entire,
  panel_hc = panel_hc,
  variable = "PROP_MULTIHOST",
  y_label = "Proporción multihost",
  titulo = "Distribución de la proporción multihost antes y después del PEUAT",
  subtitulo = "Comparación por tipo de alojamiento y zona de análisis",
  nombre = file.path(dir_graficos, "Graf3_2.png"),
  escala_y = 1,
  ejey_fijo = TRUE)

gbp_multihost

# ------------------------------------------------------------------------------
# 4.2.3 PROP_LICENSE
# ------------------------------------------------------------------------------
gbp_license <- grafico3(
  panel_entire = panel_entire,
  panel_hc = panel_hc,
  variable = "PROP_LICENSE",
  y_label = "Proporción con licencia",
  titulo = "Distribución de la proporción con licencia antes y después del PEUAT",
  subtitulo = "Comparación por tipo de alojamiento y zona de análisis",
  nombre = file.path(dir_graficos, "Graf3_3.png"),
  escala_y = 1,
  ejey_fijo = TRUE)

gbp_license

# ==============================================================================
# 4.3 Evolución de las proxies demográficas (Z1 & Z2) vs Z3
# ==============================================================================
# FUNCIÓN AUXILIAR
grafico4 <- function(panel_entire, panel_hc, variable, y_label,
                     titulo, subtitulo = NULL,
                     nombre = NULL) {
  
  datos_plot <- bind_rows(panel_entire, panel_hc) %>%
    mutate(
      GRUPO_ZONA = case_when(
        ZONA_PEUAT %in% c("1", "2") ~ "Zonas tratadas (Z1 y Z2)",
        ZONA_PEUAT == "3" ~ "Zona de control (Z3)"),
      PERIODO = case_when(
        TREAT == 0 | TREAT == "0" ~ "Antes del PEUAT",
        TREAT == 1 | TREAT == "1" ~ "Después del PEUAT"),
      GRUPO_ZONA = factor(
        GRUPO_ZONA,
        levels = c("Zonas tratadas (Z1 y Z2)", "Zona de control (Z3)")),
      PERIODO = factor(
        PERIODO,
        levels = c("Antes del PEUAT", "Después del PEUAT")),
      VALOR_GRAFICO = .data[[variable]]) %>%
    filter(!is.na(VALOR_GRAFICO),
           !is.na(GRUPO_ZONA),
           !is.na(PERIODO))
  
  bp <- function(zona, titulo_sub, mostrar_y = TRUE) {
    
    dz <- datos_plot %>%
      filter(GRUPO_ZONA == zona)
    
    vals <- dz$VALOR_GRAFICO

    lim_inf <- min(vals, na.rm = TRUE)
    lim_sup <- max(vals, na.rm = TRUE)
    
    ggplot(dz, aes(x = PERIODO,
                   y = VALOR_GRAFICO,
                   fill = PERIODO)) +
      geom_boxplot(color = "black",linewidth = 0.25,width = 0.50,
                   outlier.alpha = 0.30,outlier.size = 0.70) +
      scale_fill_manual(values = c("Antes del PEUAT" = "#D8D0D4",
                                   "Después del PEUAT" = "#A45B8D") ) +
      coord_cartesian(ylim = c(lim_inf, lim_sup)) +
      labs(title = titulo_sub,x = NULL,
           y = ifelse(mostrar_y, y_label, ""),fill = NULL) +
      theme_minimal(base_size = 12) +
      theme(panel.grid = element_blank(),
            axis.line = element_line(color = "black", linewidth = 0.25),
            axis.ticks = element_line(color = "black", linewidth = 0.25),
            axis.text.x = element_text(size = 10),
            axis.text.y = element_text(size = 10),
            axis.title.y = element_text(size = 11, margin = margin(r = 3)),
            plot.title = element_text(hjust = 0.5,face = "bold",
                                      size = 13,margin = margin(b = 4)),
            legend.position = "bottom",
            legend.text = element_text(size = 11),
            legend.key.size = unit(0.35, "cm"),
            plot.margin = margin(4, 6, 4, 6),
            panel.background = element_rect(fill = "transparent", color = NA),
            plot.background  = element_rect(fill = "transparent", color = NA),
            legend.background = element_rect(fill = "transparent", color = NA),
            legend.box.background=element_rect(fill = "transparent", color=NA))}
  
  g1 <- bp(
    zona = "Zonas tratadas (Z1 y Z2)",
    titulo_sub = "Zonas tratadas (Z1 y Z2)",
    mostrar_y = TRUE)
  
  g2 <- bp(
    zona = "Zona de control (Z3)",
    titulo_sub = "Zona de control (Z3)",
    mostrar_y = TRUE)
  
  grafico4 <- (g1 | g2) +
    plot_layout(guides = "collect") +
    plot_annotation(title = titulo,subtitle = subtitulo,
      theme = theme(
        plot.title = element_text(hjust = 0.5,face = "bold",
                                  size = 16,margin = margin(b = 3)),
        plot.subtitle = element_text(hjust = 0.5,size = 14,
                                     margin = margin(b = 6)),
        plot.margin = margin(4, 4, 4, 4),
        plot.background = element_rect(fill = "transparent", color = NA))) &
    theme(
        legend.position = "bottom",
        legend.text = element_text(size = 11),
        legend.key.size = unit(0.38, "cm"))
  
  if (!is.null(nombre)) {
    ggsave(filename = nombre,plot = grafico4,width = 11,
           height = 4.4,dpi = 500, bg = "transparent")}
  
  return(grafico4)}

# ------------------------------------------------------------------------------
# 4.3.1 INMIGRACION
# ------------------------------------------------------------------------------
gbp_inmigracion <- grafico4(
  panel_entire = panel_entire,
  panel_hc = panel_hc,
  variable = "INMIGRACION",
  y_label = "Inmigración",
  titulo = "Distribución de la inmigración antes y después del PEUAT",
  subtitulo = "Comparación entre zonas tratadas y zona de control",
  nombre = file.path(dir_graficos, "Graf4_1.png"))
gbp_inmigracion

# ------------------------------------------------------------------------------
# 4.3.2 INCREMENTO_NETO_POBLACION
# ------------------------------------------------------------------------------
gbp_incremento <- grafico4(
  panel_entire = panel_entire,
  panel_hc = panel_hc,
  variable = "INCREMENTO_NETO_POBLACION",
  y_label = "Incremento neto de población",
  titulo="Distribución del incremento neto de población antes y después del PEUAT",
  subtitulo = "Comparación entre zonas tratadas y zona de control",
  nombre = file.path(dir_graficos, "Graf4_2.png"))
gbp_incremento

# ==============================================================================
# 4.4 Evolución de las proxies económicas (Z1 & Z2) vs Z3
# ==============================================================================
# MISMA FUNCIÓN AUXILIAR QUE EL APARTADO ANTERIOR
# ------------------------------------------------------------------------------
# 4.4.1 GINI
# ------------------------------------------------------------------------------
gbp_gini <- grafico4(
  panel_entire = panel_entire,
  panel_hc = panel_hc,
  variable = "GINI",
  y_label = "Índice de Gini",
  titulo = "Distribución del índice de Gini antes y después del PEUAT",
  subtitulo = "Comparación entre zonas tratadas y zona de control",
  nombre = file.path(dir_graficos, "Graf5.png"))
gbp_gini


# ==============================================================================
# 5. MODELO DID PRINCIPAL Y RESULTADOS
# ==============================================================================
# ==============================================================================
# 5.1 MODELO DID PRINCIPAL: ENTIRE HOME/APT
# ==============================================================================
mod_step_entire <- fepois(
  NUEVAS_ALTAS ~
    # Variable principal DiD
    TREAT:ZONA +
    # Proceso stepwise de proxies
    mvsw(
      DENSIDAD_STOCK,
      PROP_MULTIHOST,
      PROP_LICENSE,
      INMIGRACION,
      INCREMENTO_NETO_POBLACION,
      RENTA_MEDIA,
      GINI) +
    # Offset poblacional
    offset(log(POBLACION)) |
    # Efectos fijos
    BARRIO + MES_ALTA,
  data = panel_entire,
  cluster = ~BARRIO)
summary(mod_step_entire)

# Seleccionamos el modelo final dentro del proceso stepwise.
# ESCOGEMOS AQUEL CON MENOR BIC
mod1e <- mod_step_entire[[102]]
summary(mod1e)
# Tabla resumida del modelo
etable(mod1e)
# Número de observaciones utilizadas por el modelo
nobs(mod1e)

# ------------------------------------------------------------------------------
# 5.1.1 Extracción de coeficientes: Entire home/apt
# ------------------------------------------------------------------------------
tabla_coef_entire <- summary(mod1e)$coeftable

coefs_entire <- as.data.frame(tabla_coef_entire) %>%
  tibble::rownames_to_column("VARIABLE") %>%
  rename(BETA = Estimate,
         ERROR_ESTANDAR = `Std. Error`,
         ESTADISTICO_Z = `z value`,
         P_VALOR = `Pr(>|z|)`) %>%
  mutate(EFECTO_PORCENTUAL = (exp(BETA) - 1) * 100,
        SIGNIFICATIVO_5 = if_else(P_VALOR < 0.05, "Sí", "No"),
        P_VALOR=if_else(P_VALOR<0.001,"<0.001",as.character(round(P_VALOR, 3))),
        BETA = round(BETA, 3),
        ERROR_ESTANDAR = round(ERROR_ESTANDAR, 3),
        ESTADISTICO_Z = round(ESTADISTICO_Z, 3),
        EFECTO_PORCENTUAL = round(EFECTO_PORCENTUAL, 2))
coefs_entire

# ------------------------------------------------------------------------------
# 5.1.2 Resultado principal DiD: Entire home/apt
# ------------------------------------------------------------------------------
# Identificamos el coeficiente DID por nombre, no por posición.
coef_did_entire <- coefs_entire %>%
  filter(VARIABLE %in% c("TREAT:ZONA", "ZONA:TREAT"))
coef_did_entire

# Intervalo de confianza del coeficiente DID
ic_beta_did_entire <- confint(mod1e,parm = coef_did_entire$VARIABLE[1])
ic_porcentual_did_entire <- (exp(ic_beta_did_entire) - 1) * 100

resultado_did_entire <- coef_did_entire %>%
  transmute(PANEL = "Entire home/apt",
            VARIABLE,
            BETA = BETA,
            ERROR_ESTANDAR = ERROR_ESTANDAR,
            ESTADISTICO_Z = ESTADISTICO_Z,
            P_VALOR = P_VALOR,
            EFECTO_PORCENTUAL = EFECTO_PORCENTUAL,
            IC_INF_PORCENTUAL = round(as.numeric(ic_porcentual_did_entire[1]),2),
            IC_SUP_PORCENTUAL = round(as.numeric(ic_porcentual_did_entire[2]),2),
            SIGNIFICATIVO_5)
resultado_did_entire

cat("MODELO DID PRINCIPAL - ENTIRE HOME/APT\n",
  "El coeficiente DiD estimado para treat:zona es",
  round(resultado_did_entire$BETA, 4), ".\n",
  "Como el modelo es Poisson, este coeficiente se interpreta transformándolo 
  con exp(beta) - 1.\n","El resultado indica que, tras la entrada en vigor del 
  PEUAT, las zonas tratadas (zonas 1 y 2) presentan una reducción estimada del",
  abs(round(resultado_did_entire$EFECTO_PORCENTUAL, 2)), "% en las nuevas 
  altas observadas de VIVIENDAS DE USO TURÍSTICO COMPLETAS respecto a la zona de
  comparación (zona 3), manteniendo constantes las covariables incluidas, 
  los efectos fijos de barrio y los efectos fijos de mes.\n El intervalo de 
  confianza al 95% indica que esta reducción podría situarse aproximadamente",
  "entre el", round(resultado_did_entire$IC_INF_PORCENTUAL, 2), "% y el", 
  round(resultado_did_entire$IC_SUP_PORCENTUAL, 2), "%.\n")

# ------------------------------------------------------------------------------
# 5.1.3 EFECTOS FIJOS ENTIRE HOME/APT (ANEXO)
# ------------------------------------------------------------------------------
# Para poder interpretar los coeficientes de los efectos fijos debemos hacer
# un modelo paralelo, fijando referencias para cada uno de los efectos fijos ***
ref_barrio <- sort(unique(panel_entire$BARRIO))[1]
ref_mes <- 2

modEF1e <- fepois(
  NUEVAS_ALTAS ~
    TREAT:ZONA +
    DENSIDAD_STOCK +
    PROP_MULTIHOST + 
    PROP_LICENSE +
    INMIGRACION +
    GINI +
    i(BARRIO, ref = ref_barrio) +
    i(MES_ALTA, ref = ref_mes) +
    offset(log(POBLACION)),
  data = panel_entire,
  cluster = ~BARRIO)
smef <- summary(modEF1e)

# Representación efectos fijos BARRIOS
# MAPA DE CALOR
# Valores positivos: su efecto es positivo en la tasa de airbnb respecto 
#                    de la media de los barrios (SUPERIOR A LA MEDIA) 
# Valores iguales a 0: tienen el mismo efecto en la tasa de airbnb respecto 
#         el efecto que tiene la media de los barrios (IGUAL A LA MEDIA)
# Valores negativos: su efecto es positivo en la tasa de airbnb respecto de 
#                    la media de los barrios (INFERIOR A LA MEDIA)

# Extraemos los efectos fijos de barrio del modelo
coefs_ef <- coef(modEF1e)

ef_barrios <- coefs_ef[grepl("^BARRIO::", names(coefs_ef))]

df_ef_barrios <- tibble(
  BARRIO = gsub("^BARRIO::", "", names(ef_barrios)),
  EF_BARRIO = as.numeric(ef_barrios))

# Añadimos el barrio de referencia, cuyo efecto fijo se fija en 0
df_ref <- tibble(
  BARRIO = ref_barrio,
  EF_BARRIO = 0)

df_ef_barrios <- bind_rows(df_ref, df_ef_barrios)

# Centramos los efectos fijos respecto a la media de barrios
df_ef_barriosE <- df_ef_barrios %>%
  mutate(
    EF_CENTRADO = EF_BARRIO - mean(EF_BARRIO, na.rm = TRUE),
    INTERPRETACION = case_when(
      EF_CENTRADO > 0 ~ "Superior a la media",
      EF_CENTRADO < 0 ~ "Inferior a la media",
      TRUE ~ "Igual a la media"))

# Creamos función para normalizar nombres de barrios
normalizar_nombre <- function(x) {
  x %>%
    str_to_lower() %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    str_replace_all("[-’`´]", " ") %>%
    str_replace_all("[[:punct:]]", " ") %>%
    str_squish()}

ef_entire_norm <- df_ef_barriosE %>%
  mutate(BARRIO_NORM = normalizar_nombre(BARRIO))

# Calculamos la media de los barrios partidos en Nord/Sud
medias_barrios_partidos <- ef_entire_norm %>%
  mutate(
    BARRIO_BASE = case_when(
      str_detect(BARRIO_NORM, "sant antoni") ~ "sant antoni",
      str_detect(BARRIO_NORM,
                 "vallcarca i els penitents") ~ "vallcarca i els penitents",
      str_detect(BARRIO_NORM, "el putxet i el farro") ~ "el putxet i el farro",
      TRUE ~ NA_character_)) %>%
  filter(!is.na(BARRIO_BASE)) %>%
  group_by(BARRIO_BASE) %>%
  summarise(
    BARRIO_MODELO = paste(BARRIO, collapse = " / "),
    EF_BARRIO = mean(EF_BARRIO, na.rm = TRUE),
    EF_CENTRADO = mean(EF_CENTRADO, na.rm = TRUE),
    DISTANCIA_UNION = 0,
    .groups = "drop") %>%
  mutate(
    NOM = case_when(
      BARRIO_BASE == "sant antoni" ~ "Sant Antoni",
      BARRIO_BASE == "vallcarca i els penitents" ~ "Vallcarca i els Penitents",
      BARRIO_BASE == "el putxet i el farro" ~ "el Putxet i el Farró"),
    INTERPRETACION = case_when(
      EF_CENTRADO > 0 ~ "Superior a la media",
      EF_CENTRADO < 0 ~ "Inferior a la media",
      TRUE ~ "Igual a la media")) %>%
  select(NOM, BARRIO_MODELO, EF_BARRIO, 
         EF_CENTRADO, INTERPRETACION, DISTANCIA_UNION)

# Leemos la capa oficial de barrios de QGIS
barrios_qgis <- st_read("0301040100_Barris_UNITATS_ADM.shp", quiet = TRUE) %>%
  st_transform(st_crs(peuat)) %>%
  st_drop_geometry() %>%
  select(NOM) %>%
  distinct() %>%
  mutate(BARRIO_OFICIAL_NORM = normalizar_nombre(NOM))

# Unión exacta entre barrios oficiales y barrios del modelo
union_exacta <- barrios_qgis %>%
  left_join(
    ef_entire_norm,
    by = c("BARRIO_OFICIAL_NORM" = "BARRIO_NORM")) %>%
  mutate(distancia = ifelse(!is.na(BARRIO), 0, NA_real_))

# Identificar barrios sin unión exacta
barrios_partidos_norm <- c(
  "sant antoni",
  "vallcarca i els penitents",
  "el putxet i el farro")

barrios_sin_match <- union_exacta %>%
  filter(is.na(BARRIO)) %>%
  filter(!BARRIO_OFICIAL_NORM %in% barrios_partidos_norm) %>%
  select(NOM, BARRIO_OFICIAL_NORM)

# Aplicar unión aproximada solo a barrios no partidos
union_fuzzy <- stringdist_left_join(
  barrios_sin_match,
  ef_entire_norm,
  by = c("BARRIO_OFICIAL_NORM" = "BARRIO_NORM"),
  method = "jw",
  max_dist = 0.08,
  distance_col = "distancia") %>%
  group_by(NOM) %>%
  slice_min(order_by = distancia, n = 1, with_ties = FALSE) %>%
  ungroup()

# Construir una tabla limpia con uniones exactas, fuzzy y barrios partidos
union_exacta_limpia <- union_exacta %>%
  filter(!is.na(BARRIO)) %>%
  transmute(
    NOM = NOM,
    BARRIO_MODELO = BARRIO,
    EF_BARRIO = EF_BARRIO,
    EF_CENTRADO = EF_CENTRADO,
    INTERPRETACION = INTERPRETACION,
    DISTANCIA_UNION = distancia)

union_fuzzy_limpia <- union_fuzzy %>%
  filter(!is.na(BARRIO)) %>%
  transmute(
    NOM = NOM,
    BARRIO_MODELO = BARRIO,
    EF_BARRIO = EF_BARRIO,
    EF_CENTRADO = EF_CENTRADO,
    INTERPRETACION = INTERPRETACION,
    DISTANCIA_UNION = distancia)

union_entire_limpia <- bind_rows(
  union_exacta_limpia,
  union_fuzzy_limpia,
  medias_barrios_partidos) %>%
  group_by(NOM) %>%
  slice(1) %>%
  ungroup()

# Corregir la Trinitat Nova como barrio no estimado
union_entire_limpia <- union_entire_limpia %>%
  mutate(
    BARRIO_MODELO = ifelse(NOM=="la Trinitat Nova",NA_character_, BARRIO_MODELO),
    EF_BARRIO = ifelse(NOM=="la Trinitat Nova", NA_real_, EF_BARRIO),
    EF_CENTRADO = ifelse(NOM=="la Trinitat Nova", NA_real_, EF_CENTRADO),
    INTERPRETACION=ifelse(NOM=="la Trinitat Nova","No estimado",INTERPRETACION),
    DISTANCIA_UNION = ifelse(NOM=="la Trinitat Nova",NA_real_,DISTANCIA_UNION))

# Crear tabla final para QGIS
EF_Barrios_Entire_QGIS <- barrios_qgis %>%
  select(NOM) %>%
  left_join(union_entire_limpia, by = "NOM") %>%
  mutate(
    INTERPRETACION = case_when(
      is.na(EF_CENTRADO) ~ "No estimado",
      EF_CENTRADO > 0 ~ "Superior a la media",
      EF_CENTRADO < 0 ~ "Inferior a la media",
      TRUE ~ "Igual a la media")) %>%
  arrange(NOM)

# Exportar tabla para unir en QGIS
write_csv(EF_Barrios_Entire_QGIS, 
          file.path(dir_datos, "EF_Barrios_Entire_QGIS.csv"))

# Representación efectos fijos MES_ALTA
# Extraemos los coeficientes correspondientes a los efectos fijos de MES_ALTA
ef_mes <- smef$coefficients[75:180]

# Construimos dataframe para ggplot
datos_ef_mes <- tibble(
  MES_ALTA = 3:108,
  EFECTO_MES = as.numeric(ef_mes))
# Eventos relevantes
ymax_mes <- max(datos_ef_mes$EFECTO_MES, na.rm = TRUE)
ymin_mes <- min(datos_ef_mes$EFECTO_MES, na.rm = TRUE)
eventos_mes <- data.frame(
  x = c(63, 86),
  y = c(2.8,2.5),
  etiqueta = c("COVID-19", "PEUAT\nen vigor"))

# Gráfico efectos fijos MES_ALTA
ggEF_mes_alta <- ggplot(datos_ef_mes, aes(x = MES_ALTA, y = EFECTO_MES)) +
  geom_line(color = "#CD8500", linewidth = 0.8) +
  geom_point(color = "#CD8500", size = 1.3) +
  geom_hline(yintercept=0,linetype="solid",color = "grey75",linewidth = 0.4) +
  geom_vline(xintercept = 63, linetype = "dashed", color = "grey65") +
  geom_vline(xintercept = 86, linetype = "dashed", color = "grey65") +
  geom_text(data = eventos_mes,aes(x = x, y = y, label = etiqueta),
            inherit.aes = FALSE,color = "grey55",fontface = "bold",
            size = 4,nudge_x = 5) +
  scale_x_continuous(breaks = c(1, 13, 25, 37, 49, 61, 73, 85, 97),
                     labels = c("2015", "2016", "2017", "2018", "2019",
                                "2020", "2021", "2022", "2023")) +
  labs(title = "Evolución de los efectos fijos mensuales",
       subtitle = "Efectos fijos estimados para MES_ALTA en el modelo Poisson 
       (viviendas de uso turístico)",
       x = NULL,
       y = "Efecto fijo mensual estimado") +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(hjust = 0.5,face = "bold",size = 16),
        plot.subtitle = element_text(hjust = 0.5,size = 14),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.position = "none")

ggEF_mes_alta
ggsave(file.path(dir_graficos, "Graf6.png"), ggEF_mes_alta,
       width = 10, height = 5.5, dpi = 500, bg = "transparent")

# ==============================================================================
# 5.2 MODELO DID PRINCIPAL: HC
# ==============================================================================
mod_step_HC <- fepois(
  NUEVAS_ALTAS ~
    # Variable principal DiD
    TREAT:ZONA +
    # Proceso stepwise de proxies
    mvsw(
      DENSIDAD_STOCK,
      PROP_MULTIHOST,
      PROP_LICENSE,
      INMIGRACION,
      INCREMENTO_NETO_POBLACION,
      RENTA_MEDIA,
      GINI) +
    # Offset poblacional
    offset(log(POBLACION)) |
    # Efectos fijos
    BARRIO + MES_ALTA,
  data = panel_hc,
  cluster = ~BARRIO)
summary(mod_step_HC)

# Seleccionamos el modelo final dentro del proceso stepwise.
# ESCOGEMOS AQUEL CON MENOR BIC
mod1HC <- mod_step_HC[[33]]
summary(mod1HC)
# Tabla resumida del modelo
etable(mod1HC)
# Número de observaciones utilizadas por el modelo
nobs(mod1HC)

# ------------------------------------------------------------------------------
# 5.2.1 Extracción de coeficientes: HC
# ------------------------------------------------------------------------------
tabla_coefHC <- summary(mod1HC)$coeftable

coefsHC <- as.data.frame(tabla_coefHC) %>%
  tibble::rownames_to_column("VARIABLE") %>%
  rename(BETA = Estimate,
         ERROR_ESTANDAR = `Std. Error`,
         ESTADISTICO_Z = `z value`,
         P_VALOR = `Pr(>|z|)`) %>%
  mutate(EFECTO_PORCENTUAL = (exp(BETA) - 1) * 100,
        SIGNIFICATIVO_5 = if_else(P_VALOR < 0.05, "Sí", "No"),
        P_VALOR = if_else(P_VALOR<0.001,"<0.001",as.character(round(P_VALOR,3))),
        BETA = round(BETA, 3),
        ERROR_ESTANDAR = round(ERROR_ESTANDAR, 3),
        ESTADISTICO_Z = round(ESTADISTICO_Z, 3),
        EFECTO_PORCENTUAL = round(EFECTO_PORCENTUAL, 2))
coefsHC

# ------------------------------------------------------------------------------
# 5.2.2 Resultado principal DiD: HC
# ------------------------------------------------------------------------------
# Identificamos el coeficiente DID por nombre, no por posición.
coef_didHC<- coefsHC %>%
  filter(VARIABLE %in% c("TREAT:ZONA", "ZONA:TREAT"))
coef_didHC

# Intervalo de confianza del coeficiente DID
ic_beta_didHC <- confint(mod1HC,parm = coef_didHC$VARIABLE[1])
ic_porcentual_didHC <- (exp(ic_beta_didHC) - 1) * 100

resultado_didHC <- coef_didHC %>%
  transmute(PANEL = "HC",
            VARIABLE,
            BETA = BETA,
            ERROR_ESTANDAR = ERROR_ESTANDAR,
            ESTADISTICO_Z = ESTADISTICO_Z,
            P_VALOR = P_VALOR,
            EFECTO_PORCENTUAL = EFECTO_PORCENTUAL,
            IC_INF_PORCENTUAL = round(as.numeric(ic_porcentual_didHC[1]),2),
            IC_SUP_PORCENTUAL = round(as.numeric(ic_porcentual_didHC[2]),2),
            SIGNIFICATIVO_5)
resultado_didHC

cat("MODELO DID PRINCIPAL - HC\n",
    "El coeficiente DiD estimado para treat:zona es",
    round(resultado_didHC$BETA, 4), ".\n",
    "Como el modelo es Poisson, este coeficiente se interpreta transformándolo 
  con exp(beta) - 1.\n","El resultado indica que, tras la entrada en vigor del 
  PEUAT, las zonas tratadas (zonas 1 y 2) presentan una reducción estimada del",
    abs(round(resultado_didHC$EFECTO_PORCENTUAL, 2)), "% en las nuevas 
  altas observadas de HOGARES COMPARTIDOS respecto a la zona de
  comparación (zona 3), manteniendo constantes las covariables incluidas, 
  los efectos fijos de barrio y los efectos fijos de mes.\n El intervalo de 
  confianza al 95% indica que esta reducción podría situarse aproximadamente",
    "entre el", round(resultado_didHC$IC_INF_PORCENTUAL, 2), "% y el", 
    round(resultado_didHC$IC_SUP_PORCENTUAL, 2), "%.\n")

# ------------------------------------------------------------------------------
# 5.2.3 EFECTOS FIJOS HC (ANEXO)
# ------------------------------------------------------------------------------
ref_barrio <- sort(unique(panel_hc$BARRIO))[1]
ref_mes <- 2

modEF1hc <- fepois(
  NUEVAS_ALTAS ~
    TREAT:ZONA +
    DENSIDAD_STOCK +
    PROP_MULTIHOST + 
    RENTA_MEDIA +
    i(BARRIO, ref = ref_barrio) +
    i(MES_ALTA, ref = ref_mes) +
    offset(log(POBLACION)),
  data = panel_hc,
  cluster = ~BARRIO)
smef <- summary(modEF1hc)

# Representación efectos fijos BARRIOS
# MAPA DE CALOR
# Valores positivos: su efecto es positivo en la tasa de airbnb respecto 
#                    de la media de los barrios (SUPERIOR A LA MEDIA) 
# Valores iguales a 0: tienen el mismo efecto en la tasa de airbnb respecto 
#         el efecto que tiene la media de los barrios(IGUAL A LA MEDIA)
# Valores negativos: su efecto es positivo en la tasa de airbnb respecto de 
#                    la media de los barrios(INFERIOR A LA MEDIA)
# Extraemos los efectos fijos de barrio del modelo HC
coefs_ef <- coef(modEF1hc)

ef_barrios <- coefs_ef[grepl("^BARRIO::", names(coefs_ef))]

df_ef_barrios <- tibble(
  BARRIO = gsub("^BARRIO::", "", names(ef_barrios)),
  EF_BARRIO = as.numeric(ef_barrios))

# Añadimos el barrio de referencia, cuyo efecto fijo se fija en 0
df_ref <- tibble(BARRIO = ref_barrio, EF_BARRIO = 0)

df_ef_barrios <- bind_rows(df_ref, df_ef_barrios)

# Centramos los efectos fijos respecto a la media de barrios
df_ef_barriosHC <- df_ef_barrios %>%
  mutate(
    EF_CENTRADO = EF_BARRIO - mean(EF_BARRIO, na.rm = TRUE),
    INTERPRETACION = case_when(
      EF_CENTRADO > 0 ~ "Superior a la media",
      EF_CENTRADO < 0 ~ "Inferior a la media",
      TRUE ~ "Igual a la media"))

ef_hc_norm <- df_ef_barriosHC %>%
  mutate(BARRIO_NORM = normalizar_nombre(BARRIO))

# Calculamos la media de los barrios partidos en Nord/Sud
medias_barrios_partidos_hc <- ef_hc_norm %>%
  mutate(
    BARRIO_BASE = case_when(
      str_detect(BARRIO_NORM, "sant antoni") ~ "sant antoni",
      str_detect(BARRIO_NORM, 
                 "vallcarca i els penitents") ~ "vallcarca i els penitents",
      str_detect(BARRIO_NORM, "el putxet i el farro") ~ "el putxet i el farro",
      TRUE ~ NA_character_)) %>%
  filter(!is.na(BARRIO_BASE)) %>%
  group_by(BARRIO_BASE) %>%
  summarise(
    BARRIO_MODELO = paste(BARRIO, collapse = " / "),
    EF_BARRIO = mean(EF_BARRIO, na.rm = TRUE),
    EF_CENTRADO = mean(EF_CENTRADO, na.rm = TRUE),
    DISTANCIA_UNION = 0,
    .groups = "drop") %>%
  mutate(
    NOM = case_when(
      BARRIO_BASE == "sant antoni" ~ "Sant Antoni",
      BARRIO_BASE == "vallcarca i els penitents" ~ "Vallcarca i els Penitents",
      BARRIO_BASE == "el putxet i el farro" ~ "el Putxet i el Farró"),
    INTERPRETACION = case_when(
      EF_CENTRADO > 0 ~ "Superior a la media",
      EF_CENTRADO < 0 ~ "Inferior a la media",
      TRUE ~ "Igual a la media")) %>%
  select(NOM,BARRIO_MODELO,EF_BARRIO,
         EF_CENTRADO,INTERPRETACION,DISTANCIA_UNION)

# Unión exacta entre barrios oficiales y barrios del modelo
union_exacta_hc <- barrios_qgis %>%
  left_join(ef_hc_norm,by = c("BARRIO_OFICIAL_NORM" = "BARRIO_NORM")) %>%
  mutate(distancia = ifelse(!is.na(BARRIO), 0, NA_real_))

# Barrios sin unión exacta
barrios_partidos_norm <- c(
  "sant antoni",
  "vallcarca i els penitents",
  "el putxet i el farro")

barrios_sin_match_hc <- union_exacta_hc %>%
  filter(is.na(BARRIO)) %>%
  filter(!BARRIO_OFICIAL_NORM %in% barrios_partidos_norm) %>%
  select(NOM, BARRIO_OFICIAL_NORM)

# Unión aproximada solo a barrios no partidos
union_fuzzy_hc <- stringdist_left_join(
  barrios_sin_match_hc,
  ef_hc_norm,
  by = c("BARRIO_OFICIAL_NORM" = "BARRIO_NORM"),
  method = "jw",
  max_dist = 0.08,
  distance_col = "distancia") %>%
  group_by(NOM) %>%
  slice_min(order_by = distancia, n = 1, with_ties = FALSE) %>%
  ungroup()


# Tabla limpia con uniones exactas, fuzzy y barrios partidos
union_exacta_hc_limpia <- union_exacta_hc %>%
  filter(!is.na(BARRIO)) %>%
  transmute(
    NOM = NOM,
    BARRIO_MODELO = BARRIO,
    EF_BARRIO = EF_BARRIO,
    EF_CENTRADO = EF_CENTRADO,
    INTERPRETACION = INTERPRETACION,
    DISTANCIA_UNION = distancia)

union_fuzzy_hc_limpia <- union_fuzzy_hc %>%
  filter(!is.na(BARRIO)) %>%
  transmute(
    NOM = NOM,
    BARRIO_MODELO = BARRIO,
    EF_BARRIO = EF_BARRIO,
    EF_CENTRADO = EF_CENTRADO,
    INTERPRETACION = INTERPRETACION,
    DISTANCIA_UNION = distancia)

union_hc_limpia <- bind_rows(
  union_exacta_hc_limpia,
  union_fuzzy_hc_limpia,
  medias_barrios_partidos_hc) %>%
  group_by(NOM) %>%
  slice(1) %>%
  ungroup()

# Tabla final para QGIS
EF_Barrios_HC_QGIS <- barrios_qgis %>%
  select(NOM) %>%
  left_join(union_hc_limpia, by = "NOM") %>%
  mutate(
    INTERPRETACION = case_when(
      is.na(EF_CENTRADO) ~ "No estimado",
      EF_CENTRADO > 0 ~ "Superior a la media",
      EF_CENTRADO < 0 ~ "Inferior a la media",
      TRUE ~ "Igual a la media")) %>%
  arrange(NOM)

# Exportamos la tabla para unir en QGIS
write_csv(EF_Barrios_HC_QGIS, file.path(dir_datos, "EF_Barrios_HC_QGIS.csv"))

# Representación efectos fijos MES_ALTA
# Extraemos los coeficientes correspondientes a los efectos fijos de MES_ALTA
ef_mes <- smef$coefficients[76:180]

# Construimos dataframe para ggplot
datos_ef_mes <- tibble(
  MES_ALTA = 4:108,
  EFECTO_MES = as.numeric(ef_mes))
# Eventos relevantes
ymax_mes <- max(datos_ef_mes$EFECTO_MES, na.rm = TRUE)
ymin_mes <- min(datos_ef_mes$EFECTO_MES, na.rm = TRUE)
eventos_mes <- data.frame(
  x = c(62, 84),
  y = c(2.3,2),
  etiqueta = c("COVID-19", "PEUAT\nen vigor"))

# Gráfico efectos fijos MES_ALTA
ggEF_mes_altaHC <- ggplot(datos_ef_mes, aes(x = MES_ALTA, y = EFECTO_MES)) +
  geom_line(color = "#CD8500", linewidth = 0.8) +
  geom_point(color = "#CD8500", size = 1.3) +
  geom_hline(yintercept=0,linetype="solid",color = "grey75",linewidth = 0.4) +
  geom_vline(xintercept = 63, linetype = "dashed", color = "grey65") +
  geom_vline(xintercept = 86, linetype = "dashed", color = "grey65") +
  geom_text(data = eventos_mes,aes(x = x, y = y, label = etiqueta),
            inherit.aes = FALSE,color = "grey55",fontface = "bold",
            size = 3,nudge_x = 5) +
  scale_x_continuous(breaks = c(1, 13, 25, 37, 49, 61, 73, 85, 97),
                     labels = c("2015", "2016", "2017", "2018", "2019",
                                "2020", "2021", "2022", "2023")) +
  labs(title = "Evolución de los efectos fijos mensuales",
       subtitle = "Efectos fijos estimados para MES_ALTA en el modelo Poisson 
       (hogares compartidos)",
       x = NULL,
       y = "Efecto fijo mensual estimado") +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(hjust = 0.5,face = "bold",size = 16),
        plot.subtitle = element_text(hjust = 0.5,size = 14),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.position = "none")

ggEF_mes_altaHC
ggsave(file.path(dir_graficos, "Graf7.png"),ggEF_mes_altaHC,
       width = 10, height = 5.5, dpi = 500, bg = "transparent")

# ==============================================================================
# 5.3 COMPARACIÓN FINAL DE RESULTADOS DID
# ==============================================================================
# Tabla comparativa de los dos modelos principales
etable(mod1e,mod1HC,keep = "%TREAT:ZONA|ZONA:TREAT")

# Tabla comparativa completa de resultados principales
resultados_did <- bind_rows(
  resultado_did_entire,
  resultado_didHC) %>%
  select(PANEL,VARIABLE,BETA,ERROR_ESTANDAR,ESTADISTICO_Z,P_VALOR,
         EFECTO_PORCENTUAL,IC_INF_PORCENTUAL,IC_SUP_PORCENTUAL,SIGNIFICATIVO_5)
resultados_did


# ==============================================================================
# 6 VALIDACIÓN DE LOS MODELOS
# ==============================================================================
# ==============================================================================
# 6.1 DIAGNÓSTICO DE SOBREDISPERSIÓN DEL MODELO POISSON
# ==============================================================================
# En un modelo Poisson, la media y la varianza se asumen iguales.
resid_pearson_entire <- residuals(mod1e, type = "pearson")
resid_pearson_hc <- residuals(mod1HC, type = "pearson")

x2_pearson_entire <- sum(resid_pearson_entire^2, na.rm = TRUE)
x2_pearson_hc <- sum(resid_pearson_hc^2, na.rm = TRUE)

df_entire <- df.residual(mod1e)
df_hc <- df.residual(mod1HC)

dispersion_entire <- x2_pearson_entire / df_entire
dispersion_hc <- x2_pearson_hc / df_hc

pvalor_entire <- pchisq(x2_pearson_entire, df = df_entire, lower.tail = FALSE)
pvalor_hc <- pchisq(x2_pearson_hc, df = df_hc, lower.tail = FALSE)

ic95_inf_entire <- x2_pearson_entire / qchisq(0.975, df = df_entire)
ic95_sup_entire <- x2_pearson_entire / qchisq(0.025, df = df_entire)

ic95_inf_hc <- x2_pearson_hc / qchisq(0.975, df = df_hc)
ic95_sup_hc <- x2_pearson_hc / qchisq(0.025, df = df_hc)

diagnostico_dispersion <- tibble(
  MODELO = c("Viviendas de uso turístico", "Hogares compartidos"),
  N_OBS_MODELO = c(nobs(mod1e), nobs(mod1HC)),
  DF_RESIDUAL = c(df_entire, df_hc),
  X2_PEARSON = c(x2_pearson_entire, x2_pearson_hc),
  DISPERSION_PEARSON = c(dispersion_entire, dispersion_hc),
  IC95_INF = c(ic95_inf_entire, ic95_inf_hc),
  IC95_SUP = c(ic95_sup_entire, ic95_sup_hc),
  P_VALOR = c(pvalor_entire, pvalor_hc),
  DIAGNOSTICO = case_when(
    DISPERSION_PEARSON > 1.5 & P_VALOR < 0.05 ~ "Sobredispersión significativa",
    DISPERSION_PEARSON > 1.5 &
      P_VALOR >= 0.05 ~ "Posible sobredispersión no significativa",
    DISPERSION_PEARSON < 0.7 ~ "Posible subdispersión",
    TRUE ~ "Dispersión razonablemente cercana a 1")) %>%
  mutate(X2_PEARSON = round(X2_PEARSON, 2),
    DISPERSION_PEARSON = round(DISPERSION_PEARSON, 2),
    IC95_INF = round(IC95_INF, 2),
    IC95_SUP = round(IC95_SUP, 2),
    P_VALOR=ifelse(P_VALOR < 0.001, "<0.001", as.character(round(P_VALOR, 4))))
diagnostico_dispersion

# ==============================================================================
# 6.2 ANÁLISIS DE RESIDUOS DEL MODELO
# ==============================================================================
resid_deviance_entire <- residuals(mod1e, type = "deviance")
resid_deviance_hc <- residuals(mod1HC, type = "deviance")
fitted_entire <- fitted(mod1e)
fitted_hc <- fitted(mod1HC)

# ------------------------------------------------------------------------------
# 6.2.1 Resumen numérico de residuos
# ------------------------------------------------------------------------------
resumen_residuos <- tibble(
  MODELO = c("Entire home/apt", "HC"),
  MEDIA_RESID_PEARSON = c(mean(resid_pearson_entire, na.rm = TRUE),
                          mean(resid_pearson_hc, na.rm = TRUE)),
  SD_RESID_PEARSON = c(sd(resid_pearson_entire, na.rm = TRUE),
                       sd(resid_pearson_hc, na.rm = TRUE)),
  MIN_RESID_PEARSON = c(min(resid_pearson_entire, na.rm = TRUE),
                        min(resid_pearson_hc, na.rm = TRUE)),
  MAX_RESID_PEARSON = c(max(resid_pearson_entire, na.rm = TRUE),
                        max(resid_pearson_hc, na.rm = TRUE)),
  MEDIA_RESID_DEVIANCE = c(mean(resid_deviance_entire, na.rm = TRUE),
                           mean(resid_deviance_hc, na.rm = TRUE)),
  SD_RESID_DEVIANCE = c(sd(resid_deviance_entire, na.rm = TRUE),
                        sd(resid_deviance_hc, na.rm = TRUE)))
resumen_residuos

# ------------------------------------------------------------------------------
# 6.2.2 Identificación de observaciones extremas
# ------------------------------------------------------------------------------
# Se consideran observaciones potencialmente extremas aquellas con:
#   |residuo de Pearson| > 3
outliers_residuos <- tibble(
  MODELO = c("Entire home/apt", "HC"),
  N_OUTLIERS_PEARSON_3 = c(sum(abs(resid_pearson_entire) > 3, na.rm = TRUE),
                           sum(abs(resid_pearson_hc) > 3, na.rm = TRUE)),
  PORC_OUTLIERS_PEARSON_3 = c(
    mean(abs(resid_pearson_entire) > 3, na.rm = TRUE) * 100,
    mean(abs(resid_pearson_hc) > 3, na.rm = TRUE) * 100))
outliers_residuos

# ==============================================================================
# 6.2.3 DIAGNÓSTICO GRÁFICO DE LOS RESIDUOS DE PEARSON
# ==============================================================================
# Construimos la base común de residuos
datos_residuos_entire <- tibble(
  MODELO = "Viviendas de uso turístico",
  FITTED = as.numeric(fitted_entire),
  RESID_PEARSON = as.numeric(resid_pearson_entire),
  RESID_DEVIANCE = as.numeric(resid_deviance_entire))

datos_residuos_hc <- tibble(
  MODELO = "Hogares Compartidos",
  FITTED = as.numeric(fitted_hc),
  RESID_PEARSON = as.numeric(resid_pearson_hc),
  RESID_DEVIANCE = as.numeric(resid_deviance_hc))

datos_residuos <- bind_rows(datos_residuos_entire, datos_residuos_hc)

# Función 1: Residuos de Pearson frente a valores ajustados
plot_resid_vs_fitted <- function(modelo_sel) {
  dz <- datos_residuos %>%
    filter(MODELO == modelo_sel)
  ggplot(dz, aes(x = FITTED, y = RESID_PEARSON)) +
    geom_point(color = "#BC8F8F", alpha = 0.45, size = 1.2) +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +
    geom_hline(yintercept = c(-3, 3), linetype = "dashed", color = "grey65") +
    labs(title = modelo_sel,x = NULL,y = "Residuos de Pearson") +
    theme_minimal(base_size = 10) +
    theme(panel.grid = element_blank(),
          axis.line = element_line(color = "black"),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          plot.title = element_text(face = "bold", hjust = 0.5, size = 13),
          legend.position = "none")}

# Función 2: Distribución de residuos de Pearson
plot_dist_residuos <- function(modelo_sel) {
  dz <- datos_residuos %>%
    filter(MODELO == modelo_sel)
  ggplot(dz, aes(x = RESID_PEARSON)) +
    geom_histogram(bins = 40,fill = "#BC8F8F",color = "black",linewidth = 0.25)+
    geom_vline(xintercept = 0, color = "black", linewidth = 0.4) +
    coord_cartesian(xlim = c(min(dz$RESID_PEARSON, na.rm = TRUE), 10)) +
    labs(title = modelo_sel,
         x = "Residuos de Pearson",
         y = "Frecuencia") +
    theme_minimal(base_size = 10) +
    theme(panel.grid = element_blank(),
          axis.line = element_line(color = "black"),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          plot.title = element_text(face = "bold", hjust = 0.5, size = 13),
          panel.background = element_rect(fill = "transparent", color = NA),
          plot.background  = element_rect(fill = "transparent", color = NA),
          legend.position = "none")}

p1 <- plot_resid_vs_fitted("Viviendas de uso turístico")
p2 <- plot_resid_vs_fitted("Hogares Compartidos")
p3 <- plot_dist_residuos("Viviendas de uso turístico")
p4 <- plot_dist_residuos("Hogares Compartidos")

gr <- (p1 + p2) / (p3 + p4) +
  plot_annotation(title = "Diagnóstico gráfico de los residuos de Pearson",
    subtitle = "Residuos frente a valores ajustados y distribución de residuos",
    theme = theme(plot.title=element_text(hjust = 0.5, face = "bold", size = 16),
                  plot.subtitle = element_text(hjust = 0.5, size = 14),
                  plot.background = element_rect(fill="transparent",color=NA)))
gr
ggsave(file.path(dir_graficos, "Graf8.png"),gr,width = 11,
       height = 8,dpi = 500, bg = "transparent")


# ==============================================================================
# 7. MODELO COMPLEMENTARIO: MODELO DID LINEAL (ANNEXO)
# ==============================================================================
# Creamos la nueva variable respuesta para ambos paneles
panel_entire2 <- panel_entire %>%
  mutate(ALTAS_PER_1000 = if_else(!is.na(POBLACION) & POBLACION > 0,
                                (NUEVAS_ALTAS / POBLACION) * 1000,NA_real_))%>%
  select(
    # 1. Identificadores unidad-tiempo
    BARRIO,ZONA_PEUAT,AÑO_ALTA,MES_ALTA,
    # 2. Variable respuesta
    ALTAS_PER_1000,
    # 3. Variables centrales DID
    TREAT,ZONA,
    # 4. Covariables construidas a partir de Airbnb
    DENSIDAD_STOCK,PROP_MULTIHOST,PROP_LICENSE,
    # 5. Proxies demográficas y socioeconómicas
    INMIGRACION,INCREMENTO_NETO_POBLACION,RENTA_MEDIA,GINI)

panel_hc2 <- panel_hc %>%
  mutate(ALTAS_PER_1000 = if_else(!is.na(POBLACION) & POBLACION > 0,
                                (NUEVAS_ALTAS / POBLACION) * 1000,NA_real_))%>%
  select(
    # 1. Identificadores unidad-tiempo
    BARRIO,ZONA_PEUAT,AÑO_ALTA,MES_ALTA,
    # 2. Variable respuesta
    ALTAS_PER_1000,
    # 3. Variables centrales DID
    TREAT,ZONA,
    # 4. Covariables construidas a partir de Airbnb
    DENSIDAD_STOCK,PROP_MULTIHOST,PROP_LICENSE,
    # 5. Proxies demográficas y socioeconómicas
    INMIGRACION,INCREMENTO_NETO_POBLACION,RENTA_MEDIA,GINI)

# ==============================================================================
# 7.1 MODELO COMPLEMENTARIO: MODELO DID LINEAL (ENTIRE HOME/APT)
# ==============================================================================
mod2e <- feols(   
  ALTAS_PER_1000 ~
    TREAT:ZONA +    
    DENSIDAD_STOCK +
    PROP_MULTIHOST +
    PROP_LICENSE +
    INMIGRACION +
    GINI |
    BARRIO + MES_ALTA,
  data = panel_entire2,
  cluster = ~BARRIO)
summary(mod2e)
etable(mod2e)

tabla_coef_mod2e <- summary(mod2e)$coeftable
coefs_mod2e <- as.data.frame(tabla_coef_mod2e) %>%
  tibble::rownames_to_column("VARIABLE") %>%
  rename(BETA = Estimate,
         ERROR_ESTANDAR = `Std. Error`,
         ESTADISTICO_T = `t value`,
         P_VALOR = `Pr(>|t|)`) %>%
  mutate(SIGNIFICATIVO_5 = if_else(P_VALOR < 0.05, "Sí", "No"),
         BETA = round(BETA, 4),
         ERROR_ESTANDAR = round(ERROR_ESTANDAR, 4),
         ESTADISTICO_T = round(ESTADISTICO_T, 4),
         P_VALOR = round(P_VALOR, 4))

coef_did_mod2e <- coefs_mod2e %>%
  filter(VARIABLE %in% c("TREAT:ZONA", "ZONA:TREAT"))
ic_beta_did_mod2e <- confint(mod2e,parm = coef_did_mod2e$VARIABLE[1])

# Resultado principal
resultado_did_mod2e <- coef_did_mod2e %>%
  transmute(PANEL = "Entire home/apt",
            MODELO = "Lineal complementario",
            VARIABLE,
            BETA,
            ERROR_ESTANDAR,
            ESTADISTICO_T,
            P_VALOR,
            IC_INF = round(as.numeric(ic_beta_did_mod2e[1]), 4),
            IC_SUP = round(as.numeric(ic_beta_did_mod2e[2]), 4),
            SIGNIFICATIVO_5)
resultado_did_mod2e

# ==============================================================================
# 7.2 MODELO COMPLEMENTARIO: MODELO DID LINEAL (HC)
# ==============================================================================
mod2HC <- feols(
  ALTAS_PER_1000 ~
    TREAT:ZONA +
    DENSIDAD_STOCK +
    PROP_MULTIHOST + 
    RENTA_MEDIA |
    BARRIO + MES_ALTA,
  data = panel_hc2,
  cluster = ~BARRIO)
summary(mod2HC)
etable(mod2HC)

tabla_coef_mod2HC <- summary(mod2HC)$coeftable
coefs_mod2HC <- as.data.frame(tabla_coef_mod2HC) %>%
  tibble::rownames_to_column("VARIABLE") %>%
  rename(BETA = Estimate,
         ERROR_ESTANDAR = `Std. Error`,
         ESTADISTICO_T = `t value`,
         P_VALOR = `Pr(>|t|)`) %>%
  mutate(SIGNIFICATIVO_5 = if_else(P_VALOR < 0.05, "Sí", "No"),
         BETA = round(BETA, 4),
         ERROR_ESTANDAR = round(ERROR_ESTANDAR, 4),
         ESTADISTICO_T = round(ESTADISTICO_T, 4),
         P_VALOR = round(P_VALOR, 4))

coef_did_mod2HC <- coefs_mod2HC %>%
  filter(VARIABLE %in% c("TREAT:ZONA", "ZONA:TREAT"))
ic_beta_did_mod2HC <- confint(mod2HC,parm = coef_did_mod2HC$VARIABLE[1])

# Resultado principal
resultado_did_mod2HC <- coef_did_mod2HC %>%
  transmute(PANEL = "HC",
            MODELO = "Lineal complementario",
            VARIABLE,
            BETA,
            ERROR_ESTANDAR,
            ESTADISTICO_T,
            P_VALOR,
            IC_INF = round(as.numeric(ic_beta_did_mod2HC[1]), 4),
            IC_SUP = round(as.numeric(ic_beta_did_mod2HC[2]), 4),
            SIGNIFICATIVO_5)
resultado_did_mod2HC

# ==============================================================================
# 7.3 MODELO COMPLEMENTARIO: COMPARACIÓN FINAL DE RESULTADOS DID
# ==============================================================================
# Tabla comparativa de los dos modelos principales
etable(mod2e,mod2HC,keep = "%TREAT:ZONA|ZONA:TREAT")

# Tabla comparativa completa de resultados principales
resultados_did2 <- bind_rows(
  resultado_did_mod2e,
  resultado_did_mod2HC) %>%
  select(PANEL,VARIABLE,BETA,ERROR_ESTANDAR,ESTADISTICO_T,P_VALOR,
         IC_INF,IC_SUP,SIGNIFICATIVO_5)
resultados_did2


# ==============================================================================
# 8. VALIDACIÓN DE LOS MODELOS COMPLEMENTARIOS
# ==============================================================================
# ==============================================================================
# 8.1 COMPARACIÓN CON LOS RESULTADOS DEL MODELO PRINCIPAL
# ==============================================================================
comparacion_modelos <- resultados_did %>%
  mutate(MODELO = "Poisson principal",
    ESTADISTICO = ESTADISTICO_Z,
    EFECTO_INTERPRETABLE = paste0(EFECTO_PORCENTUAL, "%"),
    IC_INF = IC_INF_PORCENTUAL,
    IC_SUP = IC_SUP_PORCENTUAL,
    P_VALOR = as.character(P_VALOR)) %>%
  select(PANEL,MODELO,VARIABLE,BETA,ERROR_ESTANDAR,ESTADISTICO,P_VALOR,
         EFECTO_INTERPRETABLE,IC_INF,IC_SUP,SIGNIFICATIVO_5) %>%
  bind_rows(resultados_did2 %>%
  mutate(MODELO = "Lineal complementario",
    ESTADISTICO = ESTADISTICO_T,
    EFECTO_INTERPRETABLE = paste0(round(BETA,4), " altas por 1.000 hab."),
    P_VALOR=if_else(P_VALOR<0.001,"<0.001",as.character(round(P_VALOR,3)))) %>%
  select(PANEL,MODELO,VARIABLE,BETA,ERROR_ESTANDAR,ESTADISTICO,P_VALOR,
         EFECTO_INTERPRETABLE,IC_INF,IC_SUP,SIGNIFICATIVO_5)) %>%
  mutate(SIGNO_EFECTO = case_when(BETA < 0 ~ "Negativo",
                                  BETA > 0 ~ "Positivo",
                                  BETA == 0 ~ "Nulo"))

comparacion_modelos

# ==============================================================================
# 8.2 GRÁFICOS PRINCIPALES PARA LA VALIDACIÓN
# ==============================================================================
graficos_validacion <- function(mod2e, mod2HC, nombre = NULL) {
  
  preparar_datos <- function(modelo, etiqueta) {
    
    datos <- tibble(MODELO = etiqueta,
                    AJUSTADOS = fitted(modelo),
                    RESIDUOS = resid(modelo))
    
    lim_inf <- quantile(datos$RESIDUOS, 0.01, na.rm = TRUE)
    lim_sup <- quantile(datos$RESIDUOS, 0.99, na.rm = TRUE)
    
    datos %>%mutate(RESIDUOS_GRAFICO = pmin(pmax(RESIDUOS, lim_inf), lim_sup))}
  
  datos_mod2e <- preparar_datos(mod2e, "mod2e")
  datos_mod2HC <- preparar_datos(mod2HC, "mod2HC")
  
  tema_validacion <- theme_minimal(base_size = 10) +
    theme(panel.grid = element_blank(),
          axis.line = element_line(color = "black", linewidth = 0.25),
          axis.ticks = element_line(color = "black", linewidth = 0.25),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          axis.title.x = element_text(size = 11),
          axis.title.y = element_text(size = 11),
          plot.title = element_text(hjust = 0.5, face = "bold", size = 11),
          plot.background = element_rect(fill = "transparent", color = NA))
  
  residuos_ajustados <- function(datos, titulo) {
    ggplot(datos, aes(x = AJUSTADOS, y = RESIDUOS)) +
      geom_point(color = "#BC8F8F", alpha = 0.45, size = 1.1) +
      geom_hline(yintercept = 0, color = "grey40", linewidth = 0.4) +
      geom_smooth(method = "loess", se=FALSE, color="black", linewidth = 0.55) +
      labs(title = titulo,
           x = "Valores ajustados",
           y = "Residuos") +
      tema_validacion}
  
  hist_residuos <- function(datos, titulo) {
    ggplot(datos, aes(x = RESIDUOS_GRAFICO)) +
      geom_histogram(aes(y = after_stat(density)),bins = 35,fill = "#BC8F8F",
                     color = "black",alpha = 0.65,linewidth = 0.20) +
      geom_density(color = "black", linewidth = 0.55) +
      labs(title = titulo,
           x = "Residuos",
           y = "Densidad") +
      tema_validacion}
  
  qq_residuos <- function(datos, titulo) {
    ggplot(datos, aes(sample = RESIDUOS)) +
      stat_qq(color = "#BC8F8F", alpha = 0.50, size = 1.1) +
      stat_qq_line(color = "black", linewidth = 0.55) +
      labs(title = titulo,
           x = "Cuantiles teóricos",
           y = "Cuantiles muestrales") +
      tema_validacion}
  
  g1 <- residuos_ajustados(datos_mod2e, "mod2e: Residuos vs ajustados")
  g2 <- hist_residuos(datos_mod2e, "mod2e: Distribución de residuos")
  g3 <- qq_residuos(datos_mod2e, "mod2e: QQ-plot")
  
  g4 <- residuos_ajustados(datos_mod2HC, "mod2HC: Residuos vs ajustados")
  g5 <- hist_residuos(datos_mod2HC, "mod2HC: Distribución de residuos")
  g6 <- qq_residuos(datos_mod2HC, "mod2HC: QQ-plot")
  
  grafico_validacion <- (g1 | g2 | g3) / (g4 | g5 | g6) +
    plot_annotation(
    title = "Validación gráfica de los modelos complementarios lineales",
    subtitle="Viviendas de uso turístico (mod2e) |  Hogares Compartidos (mod2HC)",
    theme = theme(
        plot.title = element_text(hjust = 0.5,face = "bold",size = 16,
                                  margin = margin(b = 3)),
        plot.subtitle = element_text(hjust = 0.5,size = 14,
                                     margin = margin(b = 6)),
        plot.background = element_rect(fill = "transparent", color = NA)))
  
  if (!is.null(nombre)) {
    ggsave(filename = nombre,plot = grafico_validacion,width = 12,
           height = 7,dpi = 500,bg = "transparent")}
  return(grafico_validacion)}

graficos_validacion(mod2e,mod2HC,file.path(dir_graficos, "Graf9.png"))
