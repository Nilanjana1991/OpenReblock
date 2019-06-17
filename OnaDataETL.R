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
library(htmltools)

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
sle_shp <- st_read("sleprofile.shp")

#SETTLEMENT CHARACTERISTICS ----------------------------------
sle_outline <- sle_shp %>%
  select(geometry, settlement, areaacres, status, roadtype)

#ENVIRONMENT --------------------------------------------------------
#Hazards
sle_publicgoods <- sle_shp %>%
  select(geometry, publicgood, settlement)
sle_publicgoods <- sle_publicgoods[sle_publicgoods %>% st_is_valid()== TRUE, ]
sle_physical<- sle_shp %>%
  select(geometry, physical, settlement)
sle_physical <- sle_physical[sle_physical %>% st_is_valid()== TRUE, ]
sle_solidwaste<- sle_shp %>%
  select(geometry, solidwaste, settlement)
sle_solidwaste <- sle_solidwaste[sle_solidwaste %>% st_is_valid()== TRUE, ]
#Disasters
sle_firefreq <- sle_shp %>%
  select(geometry, firefreque, settlement)
sle_floodfreq <- sle_shp %>%
  select(geometry, floodfrequ, settlement)

#STRUCTURE DETAILS AND TENURE SECURITY ----------------------------------
sle_permstruct <- sle_shp %>%
  select(geometry, permstruct, settlement)
sle_tempstruct <- sle_shp %>%
  select(geometry, tempstruct, settlement)
sle_evictthreatcount <- sle_shp %>%
  select(geometry, EvictThrea, settlement)
sle_perceivedrisk <- sle_shp %>%
  select(geometry, PerceivedR, settlement)
#Land ownership
sle_ownerprivate <- sle_shp %>%
  select(geometry, ownerpriva, settlement)
sle_ownergovt <- sle_shp %>%
  select(geometry, ownergovt, settlement)
sle_ownerreserve <- sle_shp %>%
  select(geometry, ownerpubli, settlement)
sle_ownercustomary <- sle_shp %>%
  select(geometry, ownercusto, settlement)

#ROAD NETWORK, ACCESS TIME AND COST ---------------------------
#Are roads planned?
sle_plannedroads <- sle_shp %>%
  select(geometry, roadsplann, settlement)
#What type of roads inside the settlement?
sle_roadtype <- sle_shp %>%
  select(geometry, roadtype, settlement)
 
#Time to reach emergency services 
sle_fireengine <- sle_shp %>%
  select(geometry, timefireen, settlement) #Time for fire engine to enter
sle_ambulance <- sle_shp %>%
  select(geometry, timeambula, settlement) #Time for ambulance to enter

#DOES THE CITY PROVIDE ANY SERVICES? -------------------------------------------
sle_servwater <- sle_shp %>%
  select(geometry, servwater, settlement)
sle_servsani <- sle_shp %>%
  select(geometry, servsani, settlement)
sle_servgarbage <- sle_shp %>%
  select(geometry, servgarbag, settlement)
sle_servelec <- sle_shp %>%
  select(geometry, servelectr, settlement)

#ECONOMIC ACTIVITY ----------------------------------------------------
#Male job categories
sle_malejobs_con <- sle_shp %>%
  select(geometry, malejobcon, settlement)
sle_malejobs_petty <- sle_shp %>%
  select(geometry, malejobpet, settlement)
sle_malejobs_agri <- sle_shp %>%
  select(geometry, malejobagr, settlement)
sle_malejobs_serv <- sle_shp %>%
  select(geometry, malejobser, settlement)
sle_malejobs_food <- sle_shp %>%
  select(geometry, malejobfoo, settlement)

#Female job categories
sle_femalejobs_con <- sle_shp %>%
  select(geometry, femalejobc, settlement)
sle_femalejobs_petty <- sle_shp %>%
  select(geometry, femalejobp, settlement)
sle_femalejobs_agri <- sle_shp %>%
  select(geometry, femalejoba, settlement)
sle_femalejobs_serv <- sle_shp %>%
  select(geometry, femalejobs, settlement)
sle_femalejobs_food <- sle_shp %>%
  select(geometry, femalejobf, settlement)


#PRIORITIES ---------------------------------------------------------
sle_waterdrainage <- sle_shp %>%
  select(geometry, water_drai, settlement) 
sle_sanitation <- sle_shp %>%
  select(geometry, sanitation, settlement)
sle_tenuresecurity <- sle_shp %>%
  select(geometry, tenure_sec, settlement)
sle_electric <- sle_shp %>%
  select(geometry, electricit, settlement)
sle_housing <- sle_shp %>%
  select(geometry, housing, settlement)

#User Interface:Freetown ---------------------------------------------------
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
pal_physical <- colorBin("Reds", domain = as.integer(sle_shp$physical), bins = 3)
pal_solidwaste <- colorBin("Reds", domain = as.integer(sle_shp$solidwaste), bins = 3)
pal_firefreq <- colorBin("Reds", domain = as.integer(sle_shp$firefreque), bins = 3)
pal_floodfreq <- colorBin("Reds", domain = as.integer(sle_shp$floodfrequ), bins = 3)
pal_evictthreatcount <- colorBin(c("green", "yellow", "red"),domain = as.integer(sle_shp$EvictThrea), bins = 3)
pal_perceivedrisk <- colorFactor(c("green", "yellow", "red"),domain = as.factor(sle_shp$PerceivedR))
pal_percent <- colorBin(c("red","yellow","green"), domain = c(0,100), bins = 9)
pal_roadsplanned <- colorFactor(c("red", "green"), domain = as.factor(sle_shp$roadsplann))
pal_timefire <- colorBin("Reds", domain = as.integer(sle_shp$timefireen), bins = 6)
pal_timeambu <- colorBin("Reds", domain = as.integer(sle_shp$timeambula), bins = 6)
pal_binary <- colorFactor(c("white","green"), domain = c(0,1))

server <- function(input, output, session) 
  #Leaflet Map - Freetown 
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
                       overlayGroups = c("Barriers", "Streets", "Buildings", "Amenities", "Settlement Outline",
                                         "Hazard: Public Infrastructure", "Hazard: Physical", "Hazard: Solid Waste",
                                         "Fire Frequency", "Flood Frequency", 
                                         "% Permanent Structures","% Temporary Structures", "Evict Threat Count", "Perceived Risk",
                                         "% Private Ownership", "% Municipal Ownership","% Customary Ownership","% Public Goods Ownership",
                                         "Planned Roads", "Fire Engine Response Time (mins)","Ambulance Response Time (mins)", 
                                         "City Service: Water", "City Service: Sanitation", "City Service: Electricity","City Service: Garbage",  
                                         "Male Jobs: Construction", "Male Jobs: Petty Trading", "Male Jobs: Agriculture", "Male Jobs: Services", "Male Jobs: Food", 
                                         "Female Jobs: Construction", "Female Jobs: Petty Trading", "Female Jobs: Agriculture", "Female Jobs: Services","Female Jobs: Food", 
                                          "Priority: Water",  "Priority: Sanitation","Priority: Housing", "Priority: Electricity", "Priority: Tenure security")) %>%
      #addPopups(data = sle_pop, as.double(~lat), as.double(~long), popup = ~htmlEscape(settlement))%>% 
      addPolylines(data = sle_water, color = "red", group = "Barriers") %>%
      addPolylines(data = sle_coast, color = "red", group = "Barriers") %>%
      addPolygons(data = sle_landuse, label = ~landuse, weight = 1, color = "red", group = "Barriers") %>% 
      addPolylines(data = sle_roads, label = ~name, color = 'purple', highlight = highlightOptions(color = "green"), group = "Streets") %>%
      addPolygons(data = sle_landamenity, label = ~landuse, weight = 1, color = "blue", group = "Amenities") %>%
      addCircleMarkers(data = sle_amenity, label = ~amenity, radius = 5, weight =1 , fillOpacity = 0.5, stroke = FALSE, group = "Amenities") %>%
      #Services data added with this
      addCircleMarkers(data = sle_ona, label = ~label, radius = 5, weight =1 , fillOpacity = 0.5, stroke = FALSE, group = "Amenities") %>%
      #Profile Data begins here
      addPolygons(data = sle_outline, stroke = TRUE, color = "black", dashArray = "3", 
                  label = ~paste0('Name:',settlement, '\n Area: ',areaacres, 'acres \n Status: ',status, '\n',roadtype), 
                  fillOpacity = 0, weight = 2, group = "Settlement Outline") %>%
      #Hazards
      addPolygons(data = sle_publicgoods, label = ~paste0(settlement,", Proximity to publi",publicgood), fillColor= ~pal_publicgood(as.integer(publicgood)), fillOpacity = 0.7, weight = 2, group = "Hazard: Public Infrastructure") %>%
      addPolygons(data = sle_physical, fillColor= ~pal_physical(as.integer(physical)), fillOpacity = 0.7, weight = 2, group = "Hazard: Physical") %>%
      addPolygons(data = sle_solidwaste, label = ~settlement, fillColor= ~pal_solidwaste(as.integer(solidwaste)), fillOpacity = 0.7, weight = 2, group = "Hazard: Garbage Dump") %>%
      #Disasters: How many structures destroyed in the last year
      addPolygons(data = sle_firefreq, label = ~settlement, fillColor= ~pal_firefreq(as.integer(firefreque)), fillOpacity = 0.7, weight = 2, group = "Structures destroyed by fire") %>%
      addPolygons(data = sle_floodfreq, label = ~settlement, fillColor= ~pal_floodfreq(as.integer(floodfrequ)), fillOpacity = 0.7, weight = 2, group = "Structures destroyed by flood") %>%
      #Structure Details: More permanent or temporary structures exist?
      addPolygons(data = sle_permstruct, label = ~settlement, fillColor= ~pal_percent(as.integer(permstruct)), fillOpacity = 0.7, weight = 2, group = "% Permanent Structures") %>%
      addPolygons(data = sle_tempstruct, label = ~settlement, fillColor= ~pal_percent(as.integer(tempstruct)), fillOpacity = 0.7, weight = 2, group = "% Temporary Structures") %>%
      #Tenure Security 
      addPolygons(data = sle_evictthreatcount, label = ~settlement, fillColor= ~pal_evictthreatcount(as.integer(EvictThrea)), fillOpacity = 0.7, weight = 2, group = "Evict Threat Count") %>%
      addPolygons(data = sle_perceivedrisk, label = ~settlement, fillColor= ~pal_perceivedrisk(as.factor(PerceivedR)), fillOpacity = 0.7, weight = 2, group = "Perceived Risk") %>%
      #Ownership of land
      addPolygons(data = sle_ownerprivate, label = ~settlement, fillColor= ~pal_percent(as.integer(ownerpriva)), fillOpacity = 0.7, weight = 2, group = "% Private Ownership") %>%
      addPolygons(data = sle_ownergovt, label = ~settlement, fillColor= ~pal_percent(as.integer(ownergovt)), fillOpacity = 0.7, weight = 2, group = "% Municipal Ownership") %>%
      addPolygons(data = sle_ownercustomary, label = ~settlement, fillColor= ~pal_percent(as.integer(ownercusto)), fillOpacity = 0.7, weight = 2, group = "% Customary Ownership") %>%
      addPolygons(data = sle_ownerreserve, label = ~settlement, fillColor= ~pal_percent(as.integer(ownerpubli)), fillOpacity = 0.7, weight = 2, group = "% Public Goods Ownership") %>%
      #Planned Roads
      addPolygons(data = sle_plannedroads, label = ~settlement, fillColor= ~pal_roadsplanned(as.factor(roadsplann)), fillOpacity = 0.7, weight = 2, group = "Planned Roads") %>%
      #Time to access and emergency response
      addPolygons(data = sle_fireengine, label = ~settlement, fillColor= ~pal_timefire(as.integer(timefireen)), fillOpacity = 0.7, weight = 2, group = "Fire Engine Response Time (mins)") %>%
      addPolygons(data = sle_ambulance, label = ~settlement, fillColor= ~pal_timeambu(as.integer(timeambula)), fillOpacity = 0.7, weight = 2, group = "Ambulance Response Time (mins)") %>%
      #Access to infrastructure
      addPolygons(data = sle_servwater, label = ~settlement, fillColor= ~pal_binary(as.integer(servwater)), fillOpacity = 0.7, weight = 2, group = "City Service: Water") %>%
      addPolygons(data = sle_servsani, label = ~settlement, fillColor= ~pal_binary(as.integer(servsani)), fillOpacity = 0.7, weight = 2, group = "City Service: Sanitation") %>%
      addPolygons(data = sle_servelec, label = ~settlement, fillColor= ~pal_binary(as.integer(servelectr)), fillOpacity = 0.7, weight = 2, group = "City Service: Electricity") %>%
      addPolygons(data = sle_servgarbage, label = ~settlement, fillColor= ~pal_binary(as.integer(servgarbag)), fillOpacity = 0.7, weight = 2, group = "City Service: Garbage") %>%
      #Male Jobs
      addPolygons(data = sle_malejobs_con, label = ~settlement, fillColor= ~pal_binary(as.factor(malejobcon)), fillOpacity = 0.6, weight = 2, group = "Male Jobs: Construction") %>%
      addPolygons(data = sle_malejobs_petty, label = ~settlement, fillColor= ~pal_binary(as.factor(malejobpet)), fillOpacity = 0.6, weight = 2, group = "Male Jobs: Petty Trading") %>%
      addPolygons(data = sle_malejobs_agri, label = ~settlement, fillColor= ~pal_binary(as.factor(malejobagr)), fillOpacity = 0.6, weight = 2, group = "Male Jobs: Agriculture") %>%
      addPolygons(data = sle_malejobs_serv, label = ~settlement, fillColor= ~pal_binary(as.factor(malejobser)), fillOpacity = 0.6, weight = 2, group = "Male Jobs: Services") %>%
      addPolygons(data = sle_malejobs_food, label = ~settlement, fillColor= ~pal_binary(as.factor(malejobfoo)), fillOpacity = 0.6, weight = 2, group = "Male Jobs: Food") %>%
      #Female Jobs
      addPolygons(data = sle_femalejobs_con, label = ~settlement, fillColor= ~pal_binary(as.factor(femalejobc)), fillOpacity = 0.6, weight = 2, group = "Female Jobs: Construction") %>%
      addPolygons(data = sle_femalejobs_petty, label = ~settlement, fillColor= ~pal_binary(as.factor(femalejobp)), fillOpacity = 0.6, weight = 2, group = "Female Jobs: Petty Trading") %>%
      addPolygons(data = sle_femalejobs_agri, label = ~settlement, fillColor= ~pal_binary(as.factor(femalejoba)), fillOpacity = 0.6, weight = 2, group = "Female Jobs: Agriculture") %>%
      addPolygons(data = sle_femalejobs_serv, label = ~settlement, fillColor= ~pal_binary(as.factor(femalejobs)), fillOpacity = 0.6, weight = 2, group = "Female Jobs: Services") %>%
      addPolygons(data = sle_femalejobs_food, label = ~settlement, fillColor= ~pal_binary(as.factor(femalejobf)), fillOpacity = 0.6, weight = 2, group = "Female Jobs: Food") %>%
      #Priorities for infrastructure access
      addPolygons(data = sle_waterdrainage, label = ~settlement, fillColor= ~pal_percent(as.integer(water_drai)), fillOpacity = 0.7, weight = 2, group = "Priority: Water") %>%
      addPolygons(data = sle_sanitation, label = ~settlement, fillColor= ~pal_percent(as.integer(sanitation)), fillOpacity = 0.7, weight = 2, group = "Priority: Sanitation") %>%
      addPolygons(data = sle_electric, label = ~settlement, fillColor= ~pal_percent(as.integer(electricit)), fillOpacity = 0.7, weight = 2, group = "Priority: Electricity") %>%
      addPolygons(data = sle_housing, label = ~settlement, fillColor= ~pal_percent(as.integer(housing)), fillOpacity = 0.7, weight = 2, group = "Priority: Housing") %>%
      addPolygons(data = sle_tenuresecurity, label = ~settlement, fillColor= ~pal_percent(as.integer(tenure_sec)), fillOpacity = 0.7, weight = 2, group = "Priority: Tenure security") %>%
      addDrawToolbar(targetGroup = "drawnPoly", rectangleOptions = F, markerOptions = F, circleOptions=F,
                     editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()), 
                     circleMarkerOptions = drawCircleMarkerOptions(color = "green"),
                     polylineOptions = drawPolylineOptions(shapeOptions = drawShapeOptions(color = 'green', fillOpacity = 0, weight = 4, opacity = .6)),
                     polygonOptions = drawPolygonOptions(showArea=TRUE, repeatMode=F, shapeOptions=drawShapeOptions(color = 'green', fillColor="green", clickable = TRUE)))
      })

shinyApp(ui, server)



#-----Lagos Map Information ---------------------------
ng_ona <- read.csv('E:\\Open Reblock\\Datasets\\ng_sdi_services.csv')
colnames(ng_ona) <- tolower(gsub("[^[:alnum:]| ]", "", colnames(ng_ona )))

# See columns names
names(ng_ona)

# See column values
unique(ng_ona$sectioncc1servicetype)
# df_ona %>% mutate(count = 1) %>% group_by(sectioncc1servicetype, sectiondd1toilettype) %>% mutate(total = sum(count)) %>% summarize_at(vars(count), funs(sum), na.rm=TRUE)

ng_ona <- ng_ona %>% 
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

ng_shp <- st_read("Lagos settlements.shp") %>%
  select(geometry, HI, settlement)
ng_shp <- ng_shp[ng_shp %>% st_is_valid()== TRUE, ]

xmin_2 = 3.3480919543
xmax_2 = 3.388862342
ymin_2 = 6.4074727767
ymax_2 = 6.4353738808
bbpoly_2 <- st_bbox(c(xmin = xmin_2, xmax = xmax_2, ymax = ymax_2, ymin = ymin_2), crs = st_crs(4326)) %>% st_as_sfc()

ng_roads <- opq(bbox = c(xmin_2, ymin_2, xmax_2, ymax_2)) %>%
  add_osm_feature(key = 'highway') %>%
  add_osm_feature(key = 'name') %>%
  osmdata_sf() %>%
  pluck("osm_lines") %>% 
  select(name) %>%
  mutate(name = fct_drop(name)) #%>%
#st_set_geometry(NULL) 
names(ng_roads$geometry) <- NULL 

#Natural water boundaries from OSM
ng_water <- opq(bbox = c(xmin_2, ymin_2, xmax_2, ymax_2)) %>%
  add_osm_feature(key = 'waterway') %>%
  add_osm_feature(key = 'name') %>%
  osmdata_sf() %>%
  pluck("osm_lines") %>% 
  select(name) %>%
  mutate(name = fct_drop(name))# %>%
#st_set_geometry(NULL)
names(ng_water$geometry) <- NULL 

#User Interface
ui <- fluidPage(
  leafletOutput("leafmap", height = "90vh"),
  br(),
  #column(3, ('runReblock', 'Run Reblock')),
  column(12,downloadButton('downloadData', 'Download .kml'))#,
  #column(8,fileInput("importData", multiple = FALSE,accept = c(".shp", ".kml"))) #"Import .shp or .kml",
)

#Reference for color coding: https://rstudio.github.io/leaflet/choropleths.html
#AServer
ng_shp$HI <- as.integer(ng_shp$HI)
bins <- c(quantile(ng_shp$HI, na.rm = TRUE, 0.00001), 
          quantile(ng_shp$HI, na.rm = TRUE, 0.125), 
          quantile(ng_shp$HI, na.rm = TRUE, 0.25), 
          quantile(ng_shp$HI, na.rm = TRUE, 0.375), 
          quantile(ng_shp$HI, na.rm = TRUE, 0.5), 
          quantile(ng_shp$HI, na.rm = TRUE, 0.625),  
          quantile(ng_shp$HI, na.rm = TRUE, 0.75), 
          quantile(ng_shp$HI, na.rm = TRUE, 1))
bins <- unique(bins)
pal <- colorBin("YlOrRd", domain = ng_shp$HI, bins = bins)
server <- function(input, output, session) 
  # Leaflet Map - Freetown 
  output$leafmap <- renderLeaflet({
    leaflet() %>%
      setView(lng = 3.34809 , lat = 6.40747, zoom = 5) %>% 
      addHomeButton(ext = raster::extent(bbpoly_2 %>% as_Spatial()), layer.name = "Lagos, Nigeria") %>%
      addTiles(group = "OSM",options = providerTileOptions(minZoom = 15, maxZoom = 18)) %>%
      addProviderTiles(provider = "Hydda.Full", group = "Color",options = providerTileOptions(minZoom = 15, maxZoom = 18)) %>%
      addProviderTiles(provider = "Esri.WorldTopoMap", group = "Topography", options = providerTileOptions(minZoom = 15, maxZoom = 18)) %>%
      addProviderTiles(provider ='Esri.WorldImagery', group = "Satellite", options = providerTileOptions(minZoom = 15, maxZoom = 18)) %>%
      addProviderTiles(provider ="CartoDB.PositronNoLabels", group = "Positron",options = providerTileOptions(minZoom = 15, maxZoom = 18)) %>%
      addLayersControl(baseGroups = c("Positron","Color","Satellite","Topography","OSM"),
                       options = layersControlOptions(collapsed = TRUE),
                       overlayGroups = c("Barriers", "Streets", "Buildings", "Amenities")) %>%
      addPolylines(data = ng_water, color = "red", group = "Barriers") %>%
      addPolygons(data = ng_shp, label = ~settlement, fillColor= ~pal(HI), fillOpacity = 0.5, weight = 2, group = "Access") %>%
      addPolylines(data = ng_roads, label = ~name, color = 'purple', highlight = highlightOptions(color = "green"), group = "Streets") %>%
      addCircleMarkers(data = ng_ona, label = ~label, radius = 5, weight =1 , fillOpacity = 0.5, stroke = FALSE, group = "Amenities") %>%
      addDrawToolbar(targetGroup = "drawnPoly", rectangleOptions = F, markerOptions = F, circleOptions=F,
                     editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()), 
                     circleMarkerOptions = drawCircleMarkerOptions(color = "green"),
                     polylineOptions = drawPolylineOptions(shapeOptions = drawShapeOptions(color = 'green', fillOpacity = 0, weight = 4, opacity = .6)),
                     polygonOptions = drawPolygonOptions(showArea=TRUE, repeatMode=F, shapeOptions=drawShapeOptions(color = 'green', fillColor="green", clickable = TRUE))) 
  })

shinyApp(ui, server)

#----- Monrovia Map Information ---------------------------
lib_ona <- read.csv('E:\\Open Reblock\\Datasets\\lib_sdi_services.csv')
colnames(lib_ona ) <- tolower(gsub("[^[:alnum:]| ]", "", colnames(lib_ona)))

# See columns names
names(lib_ona)

# See column values
unique(lib_ona$sectioncc1servicetype)
# df_ona %>% mutate(count = 1) %>% group_by(sectioncc1servicetype, sectiondd1toilettype) %>% mutate(total = sum(count)) %>% summarize_at(vars(count), funs(sum), na.rm=TRUE)

lib_ona <- lib_ona %>% 
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


lib_shp <- st_read("Monrovia settlements.shp") %>%
  select(geometry, HI, settlement)
lib_shp <- lib_shp[lib_shp %>% st_is_valid()== TRUE, ]

xmin_3 = -10.8115950032
xmax_3 = -10.7805516589
ymin_3 = 6.3192609536
ymax_3 = 6.3409750256
bbpoly_3 <- st_bbox(c(xmin = xmin_3, xmax = xmax_3, ymax = ymax_3, ymin = ymin_3), crs = st_crs(4326)) %>% st_as_sfc()

lib_roads <- opq(bbox = c(xmin_3, ymin_3, xmax_3, ymax_3)) %>%
  add_osm_feature(key = 'highway') %>%
  add_osm_feature(key = 'name') %>%
  osmdata_sf() %>%
  pluck("osm_lines") %>% 
  select(name) %>%
  mutate(name = fct_drop(name)) #%>%
#st_set_geometry(NULL) 
names(lib_roads$geometry) <- NULL 

# Natural water boundaries from OSM
lib_water <- opq(bbox = c(xmin_3, ymin_3, xmax_3, ymax_3)) %>%
  add_osm_feature(key = 'waterway') %>%
  add_osm_feature(key = 'name') %>%
  osmdata_sf() %>%
  pluck("osm_lines") %>% 
  select(name) %>%
  mutate(name = fct_drop(name))# %>%
#st_set_geometry(NULL)
names(lib_water$geometry) <- NULL 

# Land use and amenities from OSM
lib_landamenity <- opq(bbox = c(xmin_3, ymin_3, xmax_3, ymax_3)) %>%
  add_osm_feature(key = 'amenity') %>% 
  osmdata_sf() %>%
  pluck("osm_polygons") %>%
  select(amenity) %>%
  mutate(amenity = ifelse(!is.na(amenity), paste(paste(toupper(substr(gsub("_", " ", amenity), 1, 1)), substr(gsub("_", " ", amenity), 2, nchar(gsub("_", " ", amenity))), sep=""), "", sep=""), amenity)) %>%
  rename(landuse = amenity) %>%
  st_intersection(x=.,y= bbpoly_3)

lib_amenity <- opq(bbox = c(xmin_3, ymin_3, xmax_3, ymax_3)) %>%
  add_osm_feature(key = 'amenity') %>% 
  osmdata_sf() %>%
  pluck("osm_points") %>%
  select(amenity) %>%
  mutate(amenity = ifelse(!is.na(amenity), paste(paste(toupper(substr(gsub("_", " ", amenity), 1, 1)), substr(gsub("_", " ", amenity), 2, nchar(gsub("_", " ", amenity))), sep=""), "", sep=""), amenity)) %>%
  filter(!is.na(amenity))

lib_toilets <- opq(bbox = c(xmin_3, ymin_3, xmax_3, ymax_3)) %>%
  add_osm_feature(key = 'amenity', value = 'toilets') %>%
  osmdata_sf() %>%
  pluck("osm_points")

lib_power <- opq(bbox = c(xmin_3, ymin_3, xmax_3, ymax_3)) %>%
  add_osm_feature(key = 'power') %>% 
  osmdata_sf() %>%
  pluck("osm_points")

lib_shop <- opq(bbox = c(xmin_3, ymin_3, xmax_3, ymax_3)) %>%
  add_osm_feature(key = 'shop') %>% 
  osmdata_sf() %>%
  pluck("osm_points")

#User Interface
ui <- fluidPage(
  leafletOutput("leafmap", height = "90vh"),
  br(),
  #column(3, ('runReblock', 'Run Reblock')),
  column(12,downloadButton('downloadData', 'Download .kml'))#,
  #column(8,fileInput("importData", multiple = FALSE,accept = c(".shp", ".kml"))) #"Import .shp or .kml",
)

#AServer
lib_shp$HI <- as.integer(lib_shp$HI)
bins <- c(quantile(lib_shp$HI, na.rm = TRUE, 0.00001), 
          quantile(lib_shp$HI, na.rm = TRUE, 0.125), 
          quantile(lib_shp$HI, na.rm = TRUE, 0.25), 
          quantile(lib_shp$HI, na.rm = TRUE, 0.375), 
          quantile(lib_shp$HI, na.rm = TRUE, 0.5), 
          quantile(lib_shp$HI, na.rm = TRUE, 0.625),  
          quantile(lib_shp$HI, na.rm = TRUE, 0.75), 
          quantile(lib_shp$HI, na.rm = TRUE, 1))
bins <- unique(bins)
pal <- colorBin("YlOrRd", domain = lib_shp$HI, bins = bins)
server <- function(input, output, session) 
  # Leaflet Map - Freetown 
  output$leafmap <- renderLeaflet({
    leaflet() %>%
      setView(lng = -10.8116 , lat = 6.319261, zoom = 5) %>% 
      addHomeButton(ext = raster::extent(bbpoly_3 %>% as_Spatial()), layer.name = "Monrovia, Liberia") %>%
      addTiles(group = "OSM",options = providerTileOptions(minZoom = 15, maxZoom = 18)) %>%
      addProviderTiles(provider = "Hydda.Full", group = "Color",options = providerTileOptions(minZoom = 15, maxZoom = 18)) %>%
      addProviderTiles(provider = "Esri.WorldTopoMap", group = "Topography", options = providerTileOptions(minZoom = 15, maxZoom = 18)) %>%
      addProviderTiles(provider ='Esri.WorldImagery', group = "Satellite", options = providerTileOptions(minZoom = 15, maxZoom = 18)) %>%
      addProviderTiles(provider ="CartoDB.PositronNoLabels", group = "Positron",options = providerTileOptions(minZoom = 15, maxZoom = 18)) %>%
      addLayersControl(baseGroups = c("Positron","Color","Satellite","Topography","OSM"),
                       options = layersControlOptions(collapsed = TRUE),
                       overlayGroups = c("Barriers", "Streets", "Buildings", "Amenities")) %>%
      addPolylines(data = lib_water, color = "red", group = "Barriers") %>%
      addPolygons(data = lib_shp, label = ~settlement, fillColor= ~pal(HI), fillOpacity = 0.5, weight = 2, group = "Access") %>%
      addPolylines(data = lib_roads, label = ~name, color = 'purple', highlight = highlightOptions(color = "green"), group = "Streets") %>%
      addPolygons(data = lib_landamenity, label = ~landuse, weight = 1, color = "blue", group = "Amenities") %>%
      addCircleMarkers(data = lib_amenity, label = ~amenity, radius = 5, weight =1 , fillOpacity = 0.5, stroke = FALSE, group = "Amenities") %>%
      addCircleMarkers(data = lib_ona, label = ~label, radius = 5, weight =1 , fillOpacity = 0.5, stroke = FALSE, group = "Amenities") %>%
      addDrawToolbar(targetGroup = "drawnPoly", rectangleOptions = F, markerOptions = F, circleOptions=F,
                     editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()), 
                     circleMarkerOptions = drawCircleMarkerOptions(color = "green"),
                     polylineOptions = drawPolylineOptions(shapeOptions = drawShapeOptions(color = 'green', fillOpacity = 0, weight = 4, opacity = .6)),
                     polygonOptions = drawPolygonOptions(showArea=TRUE, repeatMode=F, shapeOptions=drawShapeOptions(color = 'green', fillColor="green", clickable = TRUE))) 
  })

shinyApp(ui, server)

