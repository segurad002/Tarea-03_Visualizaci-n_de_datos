#Cargamos los paquetes
library(dplyr)
library(sf)
library(DT)
library(plotly)
library(leaflet)
library(rgdal)
library(raster)

#Carga de registros de primates en Costa Rica 
primates_cr <-
  st_read(
    "https://raw.githubusercontent.com/gf0604-procesamientodatosgeograficos/2021i-datos/main/gbif/primates-cr-registros.csv",
    options = c(
      "X_POSSIBLE_NAMES=decimalLongitude",
      "Y_POSSIBLE_NAMES=decimalLatitude"
    ),
    quiet = TRUE
  )

# Tabla de registros de presencia
primates_cr %>%
  st_drop_geometry() %>%
  select(kingdom, phylum, class, order, family, genus, species, stateProvince, individualCount, eventDate) %>%
  datatable(
    colnames = c("reino", "filo", "clase", "orden", "familia", "genero", "especie", "privincia", "cantón", "fecha"),
    options = list(searchHighlight = TRUE)
  ) 


# Tabla de datos de registros de presencia
primates_cr %>%
 ## st_drop_geometry() %>%
  select(family, species, stateProvince, individualCount, eventDate
  ) %>%
  DT::datatable(
    colnames = c("familia", "especie", "provincia", "cantón","fecha"),
    rownames = FALSE,
    options = list(  language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),

      searchHighlight = TRUE
    ),
  )



#Se manipulan los datos 
total_species <- primates_cr %>% count(species)

##Grafico de pastel para datos de primates en Costa Rica 
data <- total_species[,c('species', 'n')]

fig <- plot_ly(data, labels = ~species, values = ~n, type = 'pie')
fig <- fig %>% layout(title = 'Cantidad de registros para cada especie y sus respectivos porcentajes',
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
fig



###Se debe traducir 
# Create a palette that maps factor levels to colors
palSpecies <- colorFactor(c("navy", "red", "blue", "yellow"), domain = c("Alouatta palliata", "Ateles geoffroyi", "Cebus capucinus
", "Saimiri oerstedii"))

# Obtención de la capa de altitud
alt <- getData(
  "worldclim",
  var = "alt",
  res = .5,
  lon = -84,
  lat = 10
)

# Capa de altitud recortada para los límites aproximados de Costa Rica
provincias <-
  st_read(
    "https://raw.githubusercontent.com/gf0604-procesamientodatosgeograficos/2021i-datos/main/ign/delimitacion-territorial-administrativa/cr_provincias_simp_wgs84.geojson",
    quiet = TRUE
  )
altitud <- crop(alt, extent(-86, -82.3, 8, 11.3))
altitud <-
  alt %>%
  crop(provincias) %>%
  mask(provincias)

#Mapa de distribución de primates en Costa Rica 
leaflet() %>%
  addTiles() %>%
  addRasterImage(
    altitud, 
    # colors = pal, 
    opacity = 0.8,
    group = "Altitud"
  ) %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik, group = "OpenStreetMap") %>%
  addProviderTiles(providers$Stamen.TonerLite, group = "Stamen Toner Lite") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Imágenes de ESRI") %>%
  addProviderTiles(providers$Stamen.TerrainBackground, group = "Profundida de terreno") %>%
  addProviderTiles(providers$OpenTopoMap, group = "Topografía") %>%
  addLayersControl(
    baseGroups = c("OpenStreetMap", "Stamen Toner Lite", "Imágenes de ESRI", "Profundida de terreno", "Topografía", "Altitud"),
    overlayGroups = c("Primates")
  ) %>%
  addCircleMarkers(
    data = primates_cr,
    stroke = F,
    radius = 4,
    color = ~palSpecies(species),
    fillOpacity = 1,
    popup = paste(
      primates_cr$family,
      primates_cr$species,
      primates_cr$stateProvince,
      primates_cr$individualCount,
      primates_cr$eventDate,
      primates_cr$decimalLongitude,
      primates_cr$decimalLatitude,
      sep = '<br/>'
    ),
    group = "primates de Costa Rica"
  ) %>%
  addMiniMap(
    tiles = providers$Stamen.OpenStreetMap.Mapnik,
    position = "bottomleft",
    toggleDisplay = TRUE
  )