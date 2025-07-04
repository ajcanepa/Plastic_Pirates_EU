# Instalación y Carga de Paquetes -----------------------------------------
# Instala y carga las librerías necesarias
# install.packages(c("ggplot2", "sf", "giscoR", "ggspatial", "cowplot", "dplyr"))
library(ggplot2)
library(sf)
library(giscoR)
library(ggspatial)
library(cowplot)
library(dplyr)
library(rmapshaper)

# Carga de datos mundiales ------------------------------------------------

# 1. Carga los datos mundiales UNA SOLA VEZ para mayor eficiencia
#----------------------------------------------------------------
wld_data <- gisco_get_countries(resolution = "20")


# Mapa País Completo ------------------------------------------------------
# 2. Define la función para crear el mapa
#-----------------------------------------
# 2. Define la función para crear el mapa
#-----------------------------------------
crear_mapa_pais <- function(nombre_pais, wld = wld_data) {
  
  #------------ Creación del Globo Terráqueo (Inset) ------------
  
  # Proyección ortográfica centrada cerca de Europa/África
  ortho_crs <- '+proj=ortho +lat_0=30 +lon_0=0.5 +x_0=0 +y_0=0 +R=6371000 +units=m +no_defs +type=crs'
  
  # Creación del océano
  ocean <- st_point(x = c(0,0)) %>%
    st_buffer(dist = 6371000) %>%
    st_sfc(crs = ortho_crs)
  
  # Recorta los países visibles en el globo y crea una variable para colorear el país de interés
  world_ortho <- st_intersection(wld, st_transform(ocean, 4326)) %>%
    st_transform(crs = ortho_crs) %>% 
    mutate(dummy = ifelse(NAME_ENGL == nombre_pais, "País seleccionado", "Otro"))
  
  # Extrae las fronteras internas para dibujarlas en blanco
  world_lines <- ms_innerlines(world_ortho)
  
  # Gráfico del globo terráqueo
  wld_map <- ggplot() +
    geom_sf(data = ocean, fill = "#deebf7", color = NA) +
    geom_sf(data = world_ortho, aes(fill = dummy), color = NA, show.legend = FALSE) +
    geom_sf(data = world_lines, linewidth = 0.1, colour = "white") +
    scale_fill_manual(values = c("País seleccionado" = "red", "Otro" = "grey50")) + 
    theme_void()
  
  #------------ Creación del Mapa Principal ------------
  
  # Descarga las divisiones administrativas (NUTS nivel 2) para el país de interés
  # Usamos una resolución más alta para el mapa principal.
  # El manejo de errores tryCatch es útil si un país no tiene datos NUTS.
  country_subdivisions <- tryCatch({
    gisco_get_nuts(country = nombre_pais, nuts_level = "2", resolution = "03")
  }, error = function(e) {
    # Si falla (p.ej., no hay NUTS), descarga solo el contorno del país
    gisco_get_countries(country = nombre_pais, resolution = "03")
  })
  
  # Gráfico del país principal
  main_map <- ggplot(data = country_subdivisions) +
    geom_sf(colour = "grey50", fill = "grey85", linewidth = 0.2) +
    annotation_north_arrow(
      style = north_arrow_fancy_orienteering, 
      location = "tr", # "top right" es una mejor localización por defecto
      which_north = "true",
      height = unit(1, "cm"),
      width = unit(1, "cm")
    ) +
    labs(title = nombre_pais) + # Añadimos un título para saber qué país es
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
    )
  
  #------------ Combinación de ambos mapas ------------
  
  # Usamos ggdraw de cowplot para un control más preciso del 'inset'
  final_map <- ggdraw(main_map) +
    draw_plot(wld_map, x = 0.70, y = 0.70, width = 0.25, height = 0.25)
  
  return(final_map)
}


## Como usar la función
# Creamos el mapa para Francia
mapa_francia <- crear_mapa_pais("France")

# Mostramos el mapa
print(mapa_francia)

# Para guardarlo en un archivo
# ggsave("mapa_francia.png", plot = mapa_francia, width = 8, height = 8)


# Vector con los nombres de los países de interés
paises <- c("Portugal", "Italy", "Germany")

# Usamos lapply para aplicar la función a cada elemento del vector
lista_de_mapas <- lapply(paises, crear_mapa_pais)

# Ahora `lista_de_mapas` es una lista donde cada elemento es un gráfico de ggplot
# Puedes ver el mapa de Italia así:
print(lista_de_mapas[[2]])

# O puedes guardarlos todos en un bucle
# for (i in 1:length(paises)) {
#   ggsave(paste0("mapa_", paises[i], ".png"), plot = lista_de_mapas[[i]], width = 8, height = 8)
# }

# Suponiendo que `mis_puntos_sf` es tu objeto sf con los datos de muestreo
# y que tiene una columna que identifica el país.
puntos_francia_sf <- mis_puntos_sf %>% filter(pais == "Francia")

mapa_final_francia <- mapa_francia +
  geom_sf(data = puntos_francia_sf, color = "blue", size = 2)

print(mapa_final_francia)


# Mapa solo continente Europeo --------------------------------------------

# Los datos mundiales para el globo (cargados una vez)
wld_data <- gisco_get_countries(resolution = "20")


crear_mapa_pais_europa <- function(nombre_pais, wld = wld_data) {
  
  #------------ Globo Terráqueo (Sin cambios) ------------
  # (El código para el globo se mantiene exactamente igual que antes)
  ortho_crs <- '+proj=ortho +lat_0=30 +lon_0=0.5 +x_0=0 +y_0=0 +R=6371000 +units=m +no_defs +type=crs'
  ocean <- st_point(x = c(0,0)) %>% st_buffer(dist = 6371000) %>% st_sfc(crs = ortho_crs)
  world_ortho <- st_intersection(wld, st_transform(ocean, 4326)) %>%
    st_transform(crs = ortho_crs) %>%
    mutate(dummy = ifelse(NAME_ENGL == nombre_pais, "País seleccionado", "Otro"))
  world_lines <- ms_innerlines(world_ortho)
  wld_map <- ggplot() +
    geom_sf(data = ocean, fill = "#deebf7", color = NA) +
    geom_sf(data = world_ortho, aes(fill = dummy), color = NA, show.legend = FALSE) +
    geom_sf(data = world_lines, linewidth = 0.1, colour = "white") +
    scale_fill_manual(values = c("País seleccionado" = "red", "Otro" = "grey50")) +
    theme_void()
  
  #------------ Mapa Principal (CON MODIFICACIONES) ------------
  
  # Descarga las geometrías del país
  country_subdivisions_raw <- tryCatch({
    gisco_get_nuts(country = nombre_pais, nuts_level = "2", resolution = "03")
  }, error = function(e) {
    gisco_get_countries(country = nombre_pais, resolution = "03")
  })
  
  #--- NUEVO: Recortar a los límites de Europa ---
  # Define los límites aproximados de Europa (longitud/latitud)
  bbox_europa <- st_bbox(c(xmin = -25, ymin = 34, xmax = 45, ymax = 72), crs = st_crs(4326))
  
  # Recorta los datos del país a esta área
  country_subdivisions <- st_crop(country_subdivisions_raw, bbox_europa)
  #------------------------------------------------
  
  #--- NUEVO: Comprobación de error ---
  # Si el recorte elimina todos los polígonos, el país no está en Europa.
  if (nrow(country_subdivisions) == 0) {
    stop(paste("El país '", nombre_pais, "' se encuentra fuera de los límites definidos para Europa."))
  }
  #------------------------------------
  
  # Gráfico del país principal
  main_map <- ggplot(data = country_subdivisions) +
    geom_sf(colour = "grey50", fill = "grey85", linewidth = 0.2) +
    annotation_north_arrow(
      style = north_arrow_fancy_orienteering,
      location = "tr",
      which_north = "true",
      height = unit(1, "cm"),
      width = unit(1, "cm")
    ) +
    labs(title = nombre_pais) +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))
  
  #------------ Combinación de mapas (Sin cambios) ------------
  final_map <- ggdraw(main_map) +
    draw_plot(wld_map, x = 0.70, y = 0.70, width = 0.25, height = 0.25)
  
  return(final_map)
}

# Creará el mapa de Francia centrado en Europa
mapa_francia_europa <- crear_mapa_pais_europa("France")

print(mapa_francia_europa)

# Esto producirá un error controlado
mapa_japon <- crear_mapa_pais_europa("Japan")
#> Error in crear_mapa_pais_europa("Japan") : 
#>   El país ' Japan ' se encuentra fuera de los límites definidos para Europa.