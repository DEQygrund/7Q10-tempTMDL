library(dplyr)

sf::sf_use_s2(F) # GEOS regime as a baseline

# map title
tag.map.title <- htmltools::tags$style(htmltools::HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 40%;
    text-align: left;
    padding-left: 10px; 
    padding-right: 10px; 
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 28px;
  }
"))

data.dir <- "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/"

# Project area ----
pro_areas <- sf::st_read(dsn = paste0(data.dir,"gis/project_areas.shp"),layer = "project_areas") %>% sf::st_transform(4326)
## lookup HUCs ----
lookup.huc <- readxl::read_xlsx(paste0(data.dir, "Lookup_QAPPProjectArea.xlsx"), sheet = "Lookup_QAPPProjectArea") %>% 
  dplyr::mutate(HUC_6 = as.character(HUC_6),
                HUC_8 = as.character(HUC_8),
                HUC10 = as.character(HUC10),
                HUC12 = as.character(HUC12))

# IR2018/20 Cat 4 & 5 ----
cat.45.rivers <- sf::st_read(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/GIS/2018_2020_IR_Cat4_5_Temp_Rivers_FINAL.shp",
                             layer = "2018_2020_IR_Cat4_5_Temp_Rivers_FINAL") %>% sf::st_zm() %>% sf::st_transform(4326)
cat.45.waterbodies <- sf::st_read(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/GIS/2018_2020_IR_Cat4_5_Temp_Waterbodies_FINAL.shp",
                                  layer = "2018_2020_IR_Cat4_5_Temp_Waterbodies_FINAL") %>% sf::st_zm() %>% sf::st_transform(4326)
cat.45.watershed <- sf::st_read(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/GIS/2018_2020_IR_Cat4_5_Temp_Watershed_FINAL.shp",
                                layer = "2018_2020_IR_Cat4_5_Temp_Watershed_FINAL") %>% 
  dplyr::mutate(AU_Name = gsub(pattern = "HUC12 Name: ","",AU_Name)) %>% 
  dplyr::mutate(AU_Name = paste0(AU_Name, " Watershed")) %>% 
  sf::st_zm() %>% sf::st_transform(4326)

colum_auid <- sf::st_read(dsn = "//deqhq1/TMDL/Planning statewide/2020_2024",
                          layer="Columbia_River_AU_IDs",
                          stringsAsFactors=FALSE) %>% 
  sf::st_drop_geometry() %>%
  dplyr::pull(AU_ID) 

cat.45 <- rbind(cat.45.rivers[,c("IR_categor","AU_Name","AU_ID","Year_liste","Period","HUC12")]#,
                # cat.45.waterbodies[,c("IR_categor","AU_Name","AU_ID","Year_liste","Period","HUC12")],
                # cat.45.watershed[,c("IR_categor","AU_Name","AU_ID","Year_liste","Period","HUC12")]
                ) %>% 
  dplyr::left_join(lookup.huc, by = "HUC12") %>% 
  dplyr::filter(!AU_ID %in% c(colum_auid)) %>% 
  dplyr::mutate_at("AU_Name", stringr::str_replace_all, "Cre\\*", "Creek") %>%
  dplyr::mutate_at("AU_Name", stringr::str_replace_all, "For\\*", "Fork Willamette River") %>%
  dplyr::mutate_at("AU_Name", stringr::str_replace_all, "Joh\\*", "John Day River") %>% 
  dplyr::mutate_at("AU_Name", stringr::str_replace_all, "John\\*", "John Day River") %>% 
  dplyr::mutate_at("AU_Name", stringr::str_replace_all, "McKenzie \\*", "McKenzie River") %>% 
  dplyr::mutate_at("AU_Name", stringr::str_replace_all, "Mill\\*", "Mill Creek") %>%
  dplyr::mutate_at("AU_Name", stringr::str_replace_all, "R\\*", "River") %>%
  dplyr::mutate_at("AU_Name", stringr::str_replace_all, "Ri\\*", "River") %>%
  dplyr::mutate_at("AU_Name", stringr::str_replace_all, "Riv\\*", "River") %>% 
  dplyr::mutate_at("AU_Name", stringr::str_replace_all, "Thunder Creek-North Unpqua River", "Thunder Creek-North Umpqua River")  %>%
  dplyr::mutate_at("AU_Name", stringr::str_replace_all, "W\\*", "Willamette River") %>%
  dplyr::mutate_at("AU_Name", stringr::str_replace_all, "Willamet\\*", "Willamette River") %>%
  dplyr::mutate_at("AU_Name", stringr::str_replace_all, "Willamett\\*", "Willamette River") %>% 
  dplyr::mutate_at("AU_Name", stringr::str_replace_all, "Willamette \\*", "Willamette River")

# USGS flow data ----
load(paste0(data.dir,"/download/usgs_fl.RData")) # usgs.fl.stations & usgs.fl.data
usgs.flow.stations <- usgs.fl.stations %>% # Discharge [ft3/s]
  dplyr::filter(site_no %in% usgs.fl.data$site_no) %>% # filter out the stations that have data beyond the period of 1990-2020
  dplyr::filter(!(site_tp_cd %in% c("SP","GW"))) %>% # ST = Stream
  dplyr::filter(data_type_cd %in% c("dv", "id", "iv")) %>% # dv=daily values; id=historical instantaneous values; iv=instantaneous values
  dplyr::mutate(lat = dec_lat_va,
                long = dec_long_va) %>%
  sf::st_as_sf(coords = c("long", "lat"), crs = sf::st_crs("+init=EPSG:4326"))

# Clip to project areas ----
cat45_stations_join_table <- NULL

for(i in 1:nrow(pro_areas)){
  
  # test: i=1
  
  print(paste0(pro_areas$Project_Na[i],"......"))
  
  cat.45_i <- sf::st_intersection(cat.45,pro_areas$geometry[i], sparse = FALSE) %>% 
    dplyr::filter(QAPP_Project_Area == pro_areas[i,]$Project_Na) %>% 
    dplyr::distinct(AU_ID, .keep_all = TRUE)
  
  cat.45_i_buffer <- sf::st_buffer(cat.45_i,0.02)
  
  usgs.flow.stations_i <- sf::st_intersection(usgs.flow.stations,pro_areas$geometry[i], sparse = FALSE)
  
  cat45_stations_join_i <- sf::st_join(cat.45_i_buffer,usgs.flow.stations_i)
  
  # Save the shapefile ----
  sf::st_write(cat45_stations_join_i,paste0("./maps/map_7Q10_",pro_areas$Project_Na[i],".shp"),update = TRUE)
  
  # generate the table ----
  cat45_stations_join_i_table <- sf::st_drop_geometry(cat45_stations_join_i)
  
  cat45_stations_join_table <- rbind(cat45_stations_join_table,cat45_stations_join_i_table)
  
  usgs.flow.stations_i_intersects <- usgs.flow.stations_i %>% 
    dplyr::filter(site_no %in% cat45_stations_join_i$site_no)
  
  usgs.flow.stations_i_notUse <- usgs.flow.stations_i %>% 
    dplyr::filter(!site_no %in% cat45_stations_join_i$site_no)
  
  # maps ----
  map.title <- htmltools::tags$div(tag.map.title, htmltools::HTML(paste0(pro_areas$Project_Na[i])))
  map_i <- leaflet::leaflet() %>% 
    leaflet::addControl(map.title, position = "topleft", className="map-title") %>% 
    leaflet::addMiniMap(tiles = providers$OpenStreetMap,
                        position = "bottomright",
                        width = 200,
                        height = 150,
                        zoomLevelFixed = 5,
                        toggleDisplay = TRUE,
                        minimized = TRUE) %>% 
    leaflet.extras::addResetMapButton() %>% 
    leaflet::addMapPane("OpenStreetMap", zIndex = -2000) %>% 
    leaflet::addMapPane("area", zIndex = -1000) %>%
    leaflet::addMapPane("ir", zIndex = -40) %>%
    leaflet::addMapPane("stations", zIndex = 100) %>%
    leaflet::addProviderTiles(providers$OpenStreetMap, #names(providers) to see a list of the layers
                              options = pathOptions(pane = "OpenStreetMap")) %>% 
    # __ Project area outline ----
  leaflet::addPolygons(data = pro_areas$geometry[i],
                       options = leaflet::leafletOptions(pane="area"),
                       fillColor = "transparent",
                       fillOpacity = 0,
                       weight = 3,
                       color = "black",
                       opacity = 1) %>% 
    # __ IR Cat45 streams ----
   leaflet::addPolylines(data = cat.45_i,
                         group = "2018/2020 303(d) Temperature Listed - Streams",
                         options = leaflet::leafletOptions(pane="ir"),
                         label = ~`AU_Name`,
                         labelOptions = labelOptions(style = list("color" = "blue",
                                                                  "font-size" = "20px")),
                         color = "blue",
                         opacity = 1,
                         weight = 2,
                         fill=FALSE) %>% 
    # __ IR Cat45 streams buffer ----
  leaflet::addPolylines(data = cat.45_i_buffer,
                        group = "2018/2020 303(d) Temperature Listed - Streams (buffer)",
                        options = leaflet::leafletOptions(pane="ir"),
                        label = ~`AU_Name`,
                        labelOptions = labelOptions(style = list("color" = "red",
                                                                 "font-size" = "20px")),
                        color = "red",
                        opacity = 1,
                        weight = 2,
                        fill=FALSE) %>% 
    # __ USGS flow stations intersect with Cat45 stream AUs ----
  leaflet::addMarkers(data = usgs.flow.stations_i_intersects,
                      group = "USGS flow stations intersect with Cat45 stream AUs",
                      options = leaflet::leafletOptions(pane="stations"),
                      label = ~`site_no`) %>% 
    # __ USGS flow stations don't intersect with Cat45 stream AUs ----
  leaflet::addMarkers(data = usgs.flow.stations_i_notUse,
                      group = "USGS flow stations don't intersect with Cat45 stream AUs",
                      options = leaflet::leafletOptions(pane="stations"),
                      label = ~`site_no`) %>% 
    # Layer control ----
    leaflet::addLayersControl(overlayGroups = c("2018/2020 303(d) Temperature Listed - Streams",
                                                "2018/2020 303(d) Temperature Listed - Streams (buffer)",
                                                "USGS flow stations intersect with Cat45 stream AUs",
                                                "USGS flow stations don't intersect with Cat45 stream AUs"),
                              options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)) %>% 
    leaflet::hideGroup(c("2018/2020 303(d) Temperature Listed - Streams (buffer)",
                         "USGS flow stations intersect with Cat45 stream AUs",
                         "USGS flow stations don't intersect with Cat45 stream AUs"))
  
  # Save the map ----
  print(paste0(pro_areas$Project_Na[i],"...Save the map"))
  htmlwidgets::saveWidget(map_i,paste0("map_7Q10_",i,".html"),selfcontained = TRUE) #selfcontained needs to be in the current working directory
  file.rename(paste0("map_7Q10_",i,".html"), paste0("./maps/map_7Q10_",pro_areas$Project_Na[i],".html"))
  
}

# Save the table ----
write.csv(cat45_stations_join_table, file = "./data/cat45_stations_join_table.csv")























# _ Flow data ----
## _ (1) USGS ----
station.usgs.flow <- usgs.flow.stations %>%  # Discharge [ft3/s]
  dplyr::filter(!(site_tp_cd %in% c("SP","GW"))) %>% # ST = Stream
  dplyr::filter(data_type_cd %in% c("dv", "id", "iv")) %>% # dv=daily values; id=historical instantaneous values; iv=instantaneous values
  dplyr::mutate(lat = dec_lat_va,
                long = dec_long_va) %>%
  sf::st_as_sf(coords = c("long", "lat"), crs = sf::st_crs("+init=EPSG:4269")) #%>%

station.usgs.flow <- sf::st_intersection(station.usgs.flow,pro_area_huc12_union, sparse = FALSE)
station.usgs.flow <- station.usgs.flow %>% sf::st_drop_geometry() %>% 
  dplyr::filter(!is.na(dec_lat_va)) %>% 
  dplyr::filter(!site_no %in% station.owrd$`Station ID`) %>% 
  dplyr::distinct(site_no,.keep_all=TRUE)

## In the 3 Willamette subbasin QAPPs, filter out the reachcodes covered in the Willamette mainstem QAPP
if(qapp_project_area %in% c("Lower Willamette and Clackamas Subbasins",
                            "Middle Willamette Subbasins",
                            "Southern Willamette Subbasins")) {
  
  station.usgs.flow <- station.usgs.flow %>% 
    dplyr::left_join(df.stations[,c("MLocID","Reachcode")], by = c("site_no" = "MLocID")) %>% 
    dplyr::filter(!Reachcode %in% will_reachcodes) 
  
} 

## In the Malheur and Grande Ronde QAPPs, filter out the reachcodes covered in the Willamette mainstem QAPP
if(qapp_project_area %in% c("Lower Grande Ronde, Imnaha, and Wallowa Subbasins",
                            "Malheur River Subbasins")) {
  
  station.usgs.flow <- station.usgs.flow %>% 
    dplyr::left_join(df.stations[,c("MLocID","Reachcode")], by = c("site_no" = "MLocID")) %>% 
    dplyr::filter(!Reachcode %in% will_reachcodes)
  
}

station.usgs.flow <- station.usgs.flow %>% 
  dplyr::select(`Data Source` = agency_cd, 
                `Station ID` = site_no, 
                `Station` = station_nm, 
                `Lat` = dec_lat_va, 
                `Long` = dec_long_va)

usgs.data.flow <- usgs.fl.data %>% 
  dplyr::filter(site_no %in% station.usgs.flow$`Station ID`) %>% 
  dplyr::select(`Data Source` = agency_cd,
                `Station ID` = site_no,
                dateTime,
                Result = X_00060_00003)