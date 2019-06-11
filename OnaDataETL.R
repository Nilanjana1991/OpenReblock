#Ona Services Data ETL 
#Updating this by adding level of access data: Particular help needed: 
# - How do we toggle between different sorts of access indices to this (Social Infra, Quality of Life)
#Spatial visualization of acccess (services information + survey responses)

rm(list=ls())
library(tidyverse)
library(sf)
library(osmdata)
library(leaflet)
library(leaflet.extras)
library(sp)
library(tidyverse)
library(forcats)
library(shiny)
library(shinydashboard)
library(rgdal)
library(mapview)

#-----Mapping services point data  ---------------------------
sle_ona <- read.csv('E:\\Open Reblock\\Datasets\\sle_sdi_services.csv')
colnames(sle_ona ) <- tolower(gsub("[^[:alnum:]| ]", "", colnames(sle_ona )))

# See columns names
names(sle_ona)

# See column values
unique(sle_ona$sectioncc1servicetype)
# df_ona %>% mutate(count = 1) %>% group_by(sectioncc1servicetype, sectiondd1toilettype) %>% mutate(total = sum(count)) %>% summarize_at(vars(count), funs(sum), na.rm=TRUE)

sle_ona <- sle_ona %>% 
  mutate_all(function(x) gsub("other|n/a","",x)) %>%
  mutate(sectioncc1servicetype = case_when(sectioncc1servicetype == 'toilet' ~ paste0(sectioncc1servicetype,': ',sectiondd1toilettype),
                                           sectioncc1servicetype == 'water_point' ~ paste0(sectioncc1servicetype,': ',sectiondd1watertype),
                                           TRUE ~ as.character(sectioncc1servicetype)), 
         sectioncc1servicetype = case_when(grepl("fish", tolower(.$sectioncc1servicetypeother)) ~ "Fishmonger",
                                           grepl("shop|bakery|room", tolower(.$sectioncc1servicetypeother)) ~ "Shop",
                                           grepl("police", tolower(.$sectioncc1servicetypeother)) ~ "Police",
                                           grepl("school", tolower(.$sectioncc1servicetypeother)) ~ "School",
                                           grepl("streelight", tolower(.$sectioncc1servicetypeother)) ~ "Streetlight",
                                           grepl("religious", tolower(.$sectioncc1servicetypeother)) ~ "Religious Institutions",
                                           grepl("electric|pole", tolower(.$sectioncc1servicetypeother)) ~ 'Electricity pole',
                                           grepl("field", tolower(.$sectioncc1servicetypeother)) ~ 'Field',
                                           grepl("market", tolower(.$sectioncc1servicetypeother)) ~ 'Market',
                                           grepl("drainage", tolower(.$sectioncc1servicetypeother)) ~ 'Drainage ditch',
                                           grepl("bridge", tolower(.$sectioncc1servicetypeother)) ~ 'Bridge',
                                           grepl("pig|hunt", tolower(.$sectioncc1servicetypeother)) ~ 'Livestock',
                                           grepl("community|office|network", tolower(.$sectioncc1servicetypeother)) ~ 'Community resource',
                                           grepl('aluminium|sand|charcoal|mining|plastic|scrap|metal', tolower(.$sectioncc1servicetypeother)) ~ 'Scrap or recycling site',
                                           grepl('health', tolower(.$sectioncc1servicetypeother)) ~ 'Health center',
                                           grepl('drug_store', tolower(.$sectioncc1servicetypeother)) ~ 'Drugstore',
                                           TRUE ~ as.character(sectioncc1servicetype))) %>%
  mutate(label = paste0(sectioncc1servicetype,ifelse(sectiondd3servicestatus != 'broken', '',' (broken)'))) %>%
  filter(label != '',
         sectioncc2gpslatitude != '',
         sectioncc2gpslongitude != '') %>%
  mutate(label = paste(paste(toupper(substr(gsub("_", " ", label), 1, 1)), substr(gsub("_", " ", label), 2, nchar(gsub("_", " ", label))), sep=""), "", sep="")) %>%
  select(sectioncc2gpslatitude,sectioncc2gpslongitude,label) %>%
  rename(lat = sectioncc2gpslatitude,
         lon = sectioncc2gpslongitude) %>%
  mutate(lat = as.numeric(lat),
         lon = as.numeric(lon)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) 

#Writing this to CSV 
#write.csv(df_ona, "E:\\Open Reblock\\Datasets\\libsdiservices.csv")

#Write as a shapefile 
#st_write(st_cast(df_ona, "POINT"), "//Viz//liberiaservices.shp")

# Basemap Information from OSM -------------------------------

setwd("E:\\Open Reblock\\Viz\\Housing Index Maps")

xmin_1 = -13.2588313529
xmax_1 = -13.2460028469
ymin_1 = 8.4750580572
ymax_1 = 8.4882129992
bbpoly_1 <- st_bbox(c(xmin = xmin_1, xmax = xmax_1, ymax = ymax_1, ymin = ymin_1), crs = st_crs(4326)) %>% st_as_sfc()

sle_roads <- opq(bbox = c(xmin_1, ymin_1, xmax_1, ymax_1)) %>%
  add_osm_feature(key = 'highway') %>%
  add_osm_feature(key = 'name') %>%
  osmdata_sf() %>%
  pluck("osm_lines") %>% 
  select(name) %>%
  mutate(name = fct_drop(name)) #%>%
#st_set_geometry(NULL) 
names(sle_roads$geometry) <- NULL 

#Natural water boundaries from OSM
sle_water <- opq(bbox = c(xmin_1, ymin_1, xmax_1, ymax_1)) %>%
  add_osm_feature(key = 'waterway') %>%
  add_osm_feature(key = 'name') %>%
  osmdata_sf() %>%
  pluck("osm_lines") %>% 
  select(name) %>%
  mutate(name = fct_drop(name))# %>%
#st_set_geometry(NULL)
names(sle_water$geometry) <- NULL 

sle_coast <- opq(bbox = c(xmin_1, ymin_1, xmax_1, ymax_1)) %>%
  add_osm_feature(key = 'natural', value = 'coastline') %>% 
  osmdata_sf() %>%
  pluck("osm_lines") %>% 
  select(boundary) %>% 
  st_intersection(x = ., y = bbpoly_1)

# Land use and amenities from OSM
sle_landamenity <- opq(bbox = c(xmin_1, ymin_1, xmax_1, ymax_1)) %>%
  add_osm_feature(key = 'amenity') %>% 
  osmdata_sf() %>%
  pluck("osm_polygons") %>%
  select(amenity) %>%
  mutate(amenity = ifelse(!is.na(amenity), paste(paste(toupper(substr(gsub("_", " ", amenity), 1, 1)), substr(gsub("_", " ", amenity), 2, nchar(gsub("_", " ", amenity))), sep=""), "", sep=""), amenity)) %>%
  rename(landuse = amenity) %>%
  st_intersection(x=.,y= bbpoly_1)

sle_landuse <- opq(bbox = c(xmin_1, ymin_1, xmax_1, ymax_1)) %>%
  add_osm_feature(key = 'landuse') %>% 
  osmdata_sf() %>%
  pluck("osm_polygons") %>%
  select(landuse) %>% 
  filter(landuse %in% c('landfill','cemetery')) %>%
  mutate(landuse= ifelse(!is.na(landuse), paste(paste(toupper(substr(gsub("_", " ", landuse), 1, 1)), substr(gsub("_", " ", landuse), 2, nchar(gsub("_", " ", landuse))), sep=""), "", sep=""),landuse)) %>%
  st_intersection(x=. , y=bbpoly_1)

sle_amenity <- opq(bbox = c(xmin_1, ymin_1, xmax_1, ymax_1)) %>%
  add_osm_feature(key = 'amenity') %>% 
  osmdata_sf() %>%
  pluck("osm_points") %>%
  select(amenity) %>%
  mutate(amenity = ifelse(!is.na(amenity), paste(paste(toupper(substr(gsub("_", " ", amenity), 1, 1)), substr(gsub("_", " ", amenity), 2, nchar(gsub("_", " ", amenity))), sep=""), "", sep=""), amenity)) %>%
  filter(!is.na(amenity))

sle_toilets <- opq(bbox = c(xmin_1, ymin_1, xmax_1, ymax_1)) %>%
  add_osm_feature(key = 'amenity', value = 'toilets') %>%
  osmdata_sf() %>%
  pluck("osm_points")

sle_power <- opq(bbox = c(xmin_1, ymin_1, xmax_1, ymax_1)) %>%
  add_osm_feature(key = 'power') %>% 
  osmdata_sf() %>%
  pluck("osm_points")

sle_shop <- opq(bbox = c(xmin_1, ymin_1, xmax_1, ymax_1)) %>%
  add_osm_feature(key = 'shop') %>% 
  osmdata_sf() %>%
  pluck("osm_points")

#Mapping Settlement Profile Metrics ------------------------------
sle_shp <- st_read("sierraleaone.shp")

#ENVIRONMENT --------------------------------------------------------
#Hazards
sle_publicgoods <- sle_shp %>%
  select(geometry, publicgood, settlement)
sle_publicgoods <- sle_publicgoods[sle_publicgoods %>% st_is_valid()== TRUE, ]
sle_physical<- sle_shp %>%
  select(geometry, physical, settlement)
sle_physical <- sle_physical[sle_physical %>% st_is_valid()== TRUE, ]
sle_garbagedump <- sle_shp %>%
  select(geometry, garbagedum, settlement)
sle_garbagedump <- sle_garbagedump[sle_garbagedump %>% st_is_valid()== TRUE, ]
sle_industrial <- sle_shp %>%
  select(geometry, industrial, settlement)
sle_industrial <- sle_industrial[sle_industrial %>% st_is_valid()== TRUE, ]
#Disasters
sle_firestruct <- sle_shp %>%
  select(geometry, firestruct, settlement)
sle_floodstruct <- sle_shp %>%
  select(geometry, floodstruc, settlement)
sle_malejobs <- sle_shp %>%
  select(geometry, sle_shp$malejobcon, sle_shp$malejobfis, sle_shp$malejobpet, sle_shp$malejobsdo, sle_shp$malejobser, settlement)

#User Interface
ui <- fluidPage(
  leafletOutput("leafmap", height = "90vh"),
  br(),
  #column(3, ('runReblock', 'Run Reblock')),
  column(12,downloadButton('downloadData', 'Download .kml'))#,
  #column(8,fileInput("importData", multiple = FALSE,accept = c(".shp", ".kml"))) #"Import .shp or .kml",
)

#AServer
#Creating color palettes
pal_publicgood <- colorBin("Reds", domain = as.integer(sle_shp$publicgood), bins = 3)
pal_physical <- colorBin("Greens", domain = as.integer(sle_shp$physical), bins = 3)
pal_garbagedump <- colorBin("Purples", domain = as.integer(sle_shp$garbagedum), bins = 3)
pal_industrial <- colorBin("Blues", domain = as.integer(sle_shp$industrial), bins = 3)
pal_firestruct <- colorBin("Reds", domain = as.integer(sle_shp$firestruct), bins = 3)
pal_floodstruct <- colorBin("Blues", domain = as.integer(sle_shp$floodstruc), bins = 3)

server <- function(input, output, session) 
  # Leaflet Map - Freetown 
  output$leafmap <- renderLeaflet({
    leaflet() %>%
      setView(lng = -13.252769 , lat = 8.484531, zoom = 15) %>% 
      addHomeButton(ext = raster::extent(bbpoly_1 %>% as_Spatial()), layer.name = "Freetown, Sierra Leone") %>%
      addTiles(group = "OSM",options = providerTileOptions(minZoom = 15, maxZoom = 18)) %>%
      addProviderTiles(provider = "Hydda.Full", group = "Color",options = providerTileOptions(minZoom = 15, maxZoom = 18)) %>%
      addProviderTiles(provider = "Esri.WorldTopoMap", group = "Topography", options = providerTileOptions(minZoom = 15, maxZoom = 18)) %>%
      addProviderTiles(provider ='Esri.WorldImagery', group = "Satellite", options = providerTileOptions(minZoom = 15, maxZoom = 18)) %>%
      addProviderTiles(provider ="CartoDB.PositronNoLabels", group = "Positron",options = providerTileOptions(minZoom = 15, maxZoom = 18)) %>%
      addLayersControl(baseGroups = c("Positron","Color","Satellite","Topography","OSM"),
                       options = layersControlOptions(collapsed = TRUE),
                       overlayGroups = c("Barriers", "Streets", "Buildings", "Amenities", "Hazard: Public Infrastructure", "Hazard: Physical")) %>%
      addPolylines(data = sle_water, color = "red", group = "Barriers") %>%
      addPolylines(data = sle_coast, color = "red", group = "Barriers") %>%
      addPolygons(data = sle_landuse, label = ~landuse, weight = 1, color = "red", group = "Barriers") %>% 
      addPolylines(data = sle_roads, label = ~name, color = 'purple', highlight = highlightOptions(color = "green"), group = "Streets") %>%
      addPolygons(data = sle_landamenity, label = ~landuse, weight = 1, color = "blue", group = "Amenities") %>%
      addCircleMarkers(data = sle_amenity, label = ~amenity, radius = 5, weight =1 , fillOpacity = 0.5, stroke = FALSE, group = "Amenities") %>%
      #Services data added with this
       addCircleMarkers(data = sle_ona, label = ~label, radius = 5, weight =1 , fillOpacity = 0.5, stroke = FALSE, group = "Amenities") %>%
      #Profile Data begins here
      #Hazards
      addPolygons(data = sle_publicgoods, label = ~settlement, fillColor= ~pal_publicgood(as.integer(publicgood)), fillOpacity = 0.7, weight = 2, group = "Hazard: Public Infrastructure") %>%
      addPolygons(data = sle_physical, label = ~settlement, fillColor= ~pal_physical(as.integer(physical)), fillOpacity = 0.7, weight = 2, group = "Hazard: Physical") %>%
      addPolygons(data = sle_garbagedump, label = ~settlement, fillColor= ~pal_garbagedump(as.integer(garbagedum)), fillOpacity = 0.7, weight = 2, group = "Hazard: Garbage Dump") %>%
      addPolygons(data = sle_industrial, label = ~settlement, fillColor= ~pal_industrial(as.integer(industrial)), fillOpacity = 0.7, weight = 2, group = "Hazard: Industrial") %>%
      addPolygons(data = sle_firestruct, label = ~settlement, fillColor= ~pal_firestruct(as.integer(firestruct)), fillOpacity = 0.7, weight = 2, group = "Structures destroyed by fire") %>%
      addPolygons(data = sle_floodstruct, label = ~settlement, fillColor= ~pal_floodstruct(as.integer(floodstruc)), fillOpacity = 0.7, weight = 2, group = "Structures destroyed by flood") %>%
      addDrawToolbar(targetGroup = "drawnPoly", rectangleOptions = F, markerOptions = F, circleOptions=F,
                     editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()), 
                     circleMarkerOptions = drawCircleMarkerOptions(color = "green"),
                     polylineOptions = drawPolylineOptions(shapeOptions = drawShapeOptions(color = 'green', fillOpacity = 0, weight = 4, opacity = .6)),
                     polygonOptions = drawPolygonOptions(showArea=TRUE, repeatMode=F, shapeOptions=drawShapeOptions(color = 'green', fillColor="green", clickable = TRUE)))
      })

shinyApp(ui, server)
