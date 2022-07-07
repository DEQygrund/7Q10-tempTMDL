# devtools::install_github('DEQrmichie/dflowR', host = 'https://api.github.com', dependencies= TRUE, force = TRUE, upgrade='never')
library(dflowR)
library(dplyr)
library(leaflet)

data.sharedrive <- "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/"

# Project areas ----
## Not include Willamette MS ----
pro_areas <- sf::st_read(dsn = paste0(data.sharedrive,"gis/project_areas.shp"),layer = "project_areas") %>% sf::st_transform(4326)
## Willamette MS ----
willamette_huc12 <- sf::read_sf(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/GIS/willa_snake/TempTMDL_QAPP_Reaches_data_query_HUC12s.shp",
                                layer = "TempTMDL_QAPP_Reaches_data_query_HUC12s") %>%
  dplyr::filter(Project_Na == "Willamette River Mainstem and Major Tributaries") %>%
  sf::st_transform(4326)
willamette_huc12_union <- sf::st_union(willamette_huc12)

# Lookup HUCs ----
# Include Willamette MS
lookup.huc <- readxl::read_xlsx(paste0(data.sharedrive, "Lookup_QAPPProjectArea.xlsx"), sheet = "Lookup_QAPPProjectArea") %>%
  dplyr::mutate(HUC_6 = as.character(HUC_6),
                HUC_8 = as.character(HUC_8),
                HUC10 = as.character(HUC10),
                HUC12 = as.character(HUC12))

# IR 2020-2022 CAT 4&5 Temperature AUs ----
colum_auid <- sf::st_read(dsn = "//deqhq1/TMDL/Planning statewide/2020_2024",
                          layer="Columbia_River_AU_IDs",
                          stringsAsFactors=FALSE) %>%
  sf::st_drop_geometry() %>%
  dplyr::pull(AU_ID)

cat.45.rivers <- sf::st_read(dsn = "./data/gis/2020-2022_IR_Cat45_Temperature_Rivers_project_areas.shp",
                             layer = "2020-2022_IR_Cat45_Temperature_Rivers_project_areas") %>% sf::st_zm() %>% sf::st_transform(4326) %>%
  dplyr::select(-c(AU_WBType, AU_UseCode, AQWMS_NUM, AQWMS_TXT, Shape_Leng, wqstd_code, AU_delist, Rationale, previous_r, year_asses, recordID, action_ID, action_TMD, TMDL_Prior)) %>%
  dplyr::rename(AU_Description = AU_Descrip,
                AU_parameter_category = AU_paramet,
                assessed_2022 = assessed_2,
                Year_listed = Year_liste,
                TMDL_Project = TMDL_Proje,
                QAPP_Project_Area = QAPP_Proje) %>%
  dplyr::filter(!AU_ID %in% colum_auid)

cat.45.waterbodies <- sf::st_read(dsn = "./data/gis/2020-2022_IR_Cat45_Temperature_Waterbodies_project_areas.shp",
                                  layer = "2020-2022_IR_Cat45_Temperature_Waterbodies_project_areas") %>% sf::st_zm() %>% sf::st_transform(4326) %>%
  dplyr::select(-c(AU_WBType, AU_UseCode, HUC12,AQWMS_NUM, AQWMS_TXT, Shape_Leng, Shape_Area, wqstd_code, AU_delist, Rationale, previous_r, year_asses, recordID, action_ID, action_TMD, TMDL_Prior)) %>%
  dplyr::select(-c(OBJECTID,AU_ID_1,AU_Name_1,AU_Descr_1,DO_Class,ObjectID_1,HUC_67,HUC_89,HUC8_NAME,HUC10,HUC10_NAME,HUC12_1,HUC12_Name,HUC12_Name,Adjusted,ObjectID_2)) %>%
  dplyr::rename(AU_Description = AU_Descrip,
                AU_parameter_category = AU_paramet,
                assessed_2022 = assessed_2,
                Year_listed = Year_liste,
                TMDL_Project = TMDL_Proje,
                QAPP_Project_Area = QAPP_Proje) %>%
  dplyr::filter(!AU_ID %in% colum_auid)

cat.45.watershed <- sf::st_read(dsn = "./data/gis/2020-2022_IR_Cat45_Temperature_watershed_project_areas.shp",
                                layer = "2020-2022_IR_Cat45_Temperature_watershed_project_areas") %>% sf::st_zm() %>% sf::st_transform(4326) %>%
  dplyr::select(-c(AU_WBType, AU_UseCode, HUC12,AQWMS_NUM, AQWMS_TXT, Shape_Leng, Shape_Area, wqstd_code, AU_delist, Rationale, previous_r, year_asses, recordID, action_ID, action_TMD, TMDL_Prior)) %>%
  dplyr::select(-c(OBJECTID,AU_ID_1,AU_Name_1,AU_Descr_1,DO_Class,ObjectID_1,HUC_67,HUC_89,HUC8_NAME,HUC10,HUC10_NAME,HUC12_1,HUC12_Name,HUC12_Name,Adjusted,ObjectID_2)) %>%
  dplyr::rename(AU_Description = AU_Descrip,
                AU_parameter_category = AU_paramet,
                assessed_2022 = assessed_2,
                Year_listed = Year_liste,
                TMDL_Project = TMDL_Proje,
                QAPP_Project_Area = QAPP_Proje) %>%
  dplyr::filter(!AU_ID %in% colum_auid)

cat.45.watershed.gnis <- sf::st_read(dsn = "./data/gis/2020-2022_IR_Cat45_Temperature_Watershed_GNIS_project_area.shp",
                                     layer = "2020-2022_IR_Cat45_Temperature_Watershed_GNIS_project_area") %>% sf::st_zm() %>% sf::st_transform(4326) %>%
  dplyr::rename(QAPP_Project_Area = QAPP_Proje,
                AU_GNIS_Name = AU_GNIS_Na)

cat.45 <- rbind(cat.45.rivers[,c("AU_ID","QAPP_Project_Area")],cat.45.waterbodies[,c("AU_ID","QAPP_Project_Area")],cat.45.watershed[,c("AU_ID","QAPP_Project_Area")])

cat.45.willamette <- cat.45 %>% dplyr::filter(QAPP_Project_Area == "Willamette River Mainstem and Major Tributaries")
  
# leaflet::leaflet() %>%
#   leaflet::addPolygons(data = pro_areas) %>%
#   leaflet::addPolylines(data = cat.45.watershed.gnis)

# AWQMS station database ----
## See AWQMSdata_fix.R to install AWQMSdata package
library(AWQMSdata)
huc8 <- lookup.huc %>% dplyr::select(HUC_8) %>% dplyr::distinct() %>% dplyr::pull()
awqms.stations <- AWQMSdata::query_stations(state="OR",stations_odbc="STATIONS", huc8=huc8)
save(awqms.stations, file=paste0("./data/awqms_stations.RData")) # download date: 7/1/2022

# USGS stations and data ----
# install.packages("dataRetrieval")
library(dataRetrieval)
## Github: https://github.com/USGS-R/dataRetrieval
## Stations:
### OR stations:
usgs.flow.stations.or <- dataRetrieval::whatNWISdata(stateCd="OR", parameterCd = "00060") # 00060	= Discharge [ft3/s]
### WA stations for Willamette Mainstem QAPP:
usgs.flow.stations.wa <- dataRetrieval::whatNWISdata(stateCd="WA", parameterCd = "00060")
usgs.flow.stations.wa.willamette <- usgs.flow.stations.wa %>%
  dplyr::mutate(lat = dec_lat_va,
                long = dec_long_va) %>%
  sf::st_as_sf(coords = c("long", "lat"), crs = sf::st_crs("+init=EPSG:4326"))
sf::st_as_s2(FALSE)
usgs.flow.stations.wa.willamette <- sf::st_intersection(usgs.flow.stations.wa.willamette,willamette_huc12_union) %>%
  sf::st_drop_geometry()
usgs.flow.stations <- rbind(usgs.flow.stations.or,usgs.flow.stations.wa.willamette)
## Data:
usgs.fl.data <- NULL
for (id in unique(sort(usgs.flow.stations$site_no))) {
  print(id)
  usgs.fl.data.i <- dataRetrieval::readNWISdata(siteNumber = id,
                                                  parameterCd = "00060",    # and statCd = "00003" for daily mean which is default
                                                  startDate = "1878-06-01", # min(usgs.flow.stations$begin_date)
                                                  endDate = "2022-06-14")   # max(usgs.flow.stations$end_date)
  usgs.fl.data <- dplyr::bind_rows(usgs.fl.data,usgs.fl.data.i)
}

usgs.flow.data <- usgs.fl.data %>% dplyr::filter(!is.na(X_00060_00003)) # negative flows are kept

save(usgs.flow.stations,usgs.flow.data, file=paste0("./data/usgs_flow.RData")) # download date: 6/15/2022

au.usgs.stations <- usgs.flow.stations %>%
  dplyr::left_join(awqms.stations,by=c("site_no"="MLocID")) %>% # Add AU ID to the USGS station table
  dplyr::left_join(cat.45,by="AU_ID") %>% # Join IR 2020-2022 CAT 4&5, which add QAPP_project_areas to the USGS station table
  dplyr::filter(HUC8 %in% huc8) # Filter HUC within the project areas

writexl::write_xlsx(au.usgs.stations,"./data/au_usgs_stations.xlsx")
writexl::write_xlsx(usgs.flow.stations,"./data/usgs_fl_stations.xlsx")

# OWRD stations and data ----
devtools::source_gist("https://gist.github.com/DEQrmichie/835c7c8b3f373ed80e4b9e34c656951d")
owrd.stations.or <- readxl::read_xlsx(paste0(data.sharedrive,"download/wrd_nonUSGS_stations.xlsx"), sheet = "Non-USGS_stations") %>%
  dplyr::mutate(Period_Start = as.character(as.Date(as.numeric(First), origin = "1899-12-30")),
                Period_End = as.character(as.Date(as.numeric(Last), origin = "1899-12-30"))) %>%
  dplyr::mutate(Period_Start = if_else(Date_ID %in% "Date",Period_Start,First),
                Period_End = if_else(Date_ID %in% "Date",Period_End,Last))

owrd.stations.nbr <- owrd.stations.or %>%
  dplyr::distinct(station_nbr) %>%
  dplyr::pull(station_nbr)

owrd.fl.data <- NULL
for(station in sort(unique(owrd.stations.nbr))) {
  print(station)
  owrd.data.ind <- owrd_data(station = station,
                             startdate = "1/1/1905", # sort(unique(owrd.stations.or$Period_Start))[1]: "1905-06-16"
                             enddate = "6/30/2022",  # Data download date
                             char = "MDF") # MDF - Mean Daily Flow
  owrd.fl.data <- rbind(owrd.fl.data,owrd.data.ind)
}

owrd.flow.data <- owrd.fl.data %>% dplyr::filter(!is.na(Result.Value)) # negative flows are kept

save(owrd.stations.or, owrd.flow.data, file=paste0("./data/owrd.RData")) # updated date: 6/30/2022

au.owrd.stations <- owrd.stations.or %>%
  dplyr::mutate(station_nbr = as.character(station_nbr)) %>%
  dplyr::left_join(awqms.stations,by=c("station_nbr"="MLocID")) %>% # Add AU ID to the OWRD station table
  dplyr::left_join(cat.45,by="AU_ID") %>% # Join IR 2020-2022 CAT 4&5, which add QAPP_project_areas to the OWRD station table
  dplyr::filter(HUC8.x %in% huc8) %>% # Filter HUC within the project areas
  dplyr::filter(!station_nbr %in% usgs.flow.stations$site_no) # Filter out USGS stations

writexl::write_xlsx(au.owrd.stations,"./data/au_owrd_stations.xlsx")
writexl::write_xlsx(owrd.stations.or,"./data/owrd_fl_stations.xlsx")

save(data.sharedrive,
     pro_areas,
     willamette_huc12,
     willamette_huc12_union,
     lookup.huc,
     colum_auid,
     cat.45.rivers,
     cat.45.waterbodies,
     cat.45.watershed,
     cat.45.watershed.gnis,
     cat.45,
     cat.45.willamette,
     huc8,
     awqms.stations,
     usgs.flow.stations.or,
     usgs.flow.stations.wa,
     usgs.flow.stations.wa.willamette,
     usgs.fl.data,
     usgs.flow.stations,
     usgs.flow.data,
     au.usgs.stations,
     owrd.stations.or,
     owrd.fl.data,
     owrd.flow.data,
     au.owrd.stations,
     file = "data.RData")

load("data.RData")

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

# Each project area ----
## for test:
# project_area = "John Day River Basin"
# project_area = "Lower Grande Ronde, Imnaha, and Wallowa Subbasins"
# project_area = "Lower Willamette and Clackamas Subbasins"
# project_area = "Malheur River Subbasins"
# project_area = "Middle Willamette Subbasins"
# project_area = "Middle Columbia-Hood, Miles Creeks"
# project_area = "North Umpqua Subbasin"
# project_area = "Rogue River Basin"
# project_area = "Sandy Subbasin"
# project_area = "South Umpqua and Umpqua Subbasins"
# project_area = "Southern Willamette Subbasins"
# project_area = "Walla Walla Subbasin"
# project_area = "Willow Creek Subbasin"
# project_area = "Willow Creek Subbasin"
# project_area = "Willamette River Mainstem and Major Tributaries"

for(project_area in pro_areas$Project_Na){
  
# __ AUs ----
print(paste0(project_area,"......AUs"))
cat.45.rivers.pro.area <- #sf::st_intersection(cat.45.rivers,pro_areas[which(pro_areas$Project_Na == project_area),]$geometry) %>% 
  cat.45.rivers %>%
  dplyr::distinct(AU_ID, .keep_all = TRUE) %>% 
  dplyr::filter(QAPP_Project_Area %in% project_area) %>% 
  dplyr::mutate(AU_GNIS_Name = NA)

cat.45.waterbodies.pro.area <- # sf::st_intersection(cat.45.waterbodies,pro_areas[which(pro_areas$Project_Na == project_area),]$geometry) %>% 
  cat.45.waterbodies %>%
  dplyr::distinct(AU_ID, .keep_all = TRUE) %>% 
  dplyr::filter(QAPP_Project_Area %in% project_area) %>% 
  dplyr::mutate(AU_GNIS_Name = NA)

cat.45.watershed.pro.area <- # sf::st_intersection(cat.45.watershed,pro_areas[which(pro_areas$Project_Na == project_area),]$geometry) %>% 
  cat.45.watershed %>% 
  dplyr::distinct(AU_ID, .keep_all = TRUE) %>% 
  dplyr::filter(QAPP_Project_Area %in% project_area)

cat.45.watershed.gnis.pro.area <- sf::st_intersection(cat.45.watershed.gnis,pro_areas[which(pro_areas$Project_Na == project_area),]$geometry) %>% 
  dplyr::filter(QAPP_Project_Area %in% project_area)

cat.45.watershed.gnis.pro.area.tbl <- cat.45.watershed.gnis.pro.area %>% sf::st_drop_geometry() %>% 
  dplyr::distinct(AU_ID,AU_GNIS_Name)

cat.45.watershed.au.gnis.pro.area <- cat.45.watershed.pro.area %>% 
  dplyr::left_join(cat.45.watershed.gnis.pro.area.tbl[,c("AU_ID","AU_GNIS_Name")],by="AU_ID")

cat.45.pro.area.count <- data.frame(dtf = c("cat.45.rivers.pro.area",
                                            "cat.45.waterbodies.pro.area",
                                            "cat.45.watershed.au.gnis.pro.area"),
                                    nrow = c(nrow(cat.45.rivers.pro.area),
                                             nrow(cat.45.waterbodies.pro.area),
                                             nrow(cat.45.watershed.au.gnis.pro.area))) %>% 
  dplyr::filter(!nrow == 0)

cat.45.pro.area.count

if(project_area %in% c("Lower Willamette and Clackamas Subbasins",
                       "Middle Columbia-Hood, Miles Creeks",
                       "North Umpqua Subbasin",
                       "Walla Walla Subbasin",
                       "Willow Creek Subbasin")){
  cat.45.pro.area <- rbind(cat.45.rivers.pro.area,cat.45.watershed.au.gnis.pro.area)
} else {
  cat.45.pro.area <- rbind(cat.45.rivers.pro.area,cat.45.waterbodies.pro.area,cat.45.watershed.au.gnis.pro.area)
}

if(project_area %in% "Willamette River Mainstem and Major Tributaries") {
  cat.45.pro.area <- cat.45.rivers.pro.area
}

# leaflet::leaflet() %>%
#   leaflet::addPolygons(data = cat.45.watershed.pro.area) %>%
#   leaflet::addPolylines(data = cat.45.watershed.gnis.pro.area)

# __ USGS and OWRD stations ----
print(paste0(project_area,"......USGS and OWRD stations"))
au.usgs.stations.pro.area <- au.usgs.stations %>% 
  dplyr::filter(QAPP_Project_Area == project_area) %>% 
  dplyr::distinct(site_no, .keep_all = TRUE) %>% 
  dplyr::select(AU_ID,GNIS_Name,
                Data_Source = agency_cd,
                Station_ID = site_no,
                Station_Name = station_nm,
                Latitude = dec_lat_va,
                Longitude = dec_long_va,
                Period_Start = begin_date,
                Period_End = end_date) %>% 
  dplyr::mutate(Period_Start = as.character(Period_Start),
                Period_End = as.character(Period_End))

au.owrd.stations.pro.area <- au.owrd.stations %>% 
  dplyr::filter(QAPP_Project_Area == project_area) %>% 
  dplyr::distinct(station_nbr, .keep_all = TRUE) %>% 
  dplyr::select(AU_ID,GNIS_Name,
                Data_Source = Operator,
                Station_ID = station_nbr,
                Station_Name = station_name,
                Latitude = Lat,
                Longitude = Long,
                Period_Start,Period_End) %>% 
  dplyr::mutate(Period_Start = as.character(Period_Start),
                Period_End = as.character(Period_End))

au.usgs.owrd.stations.pro.area <- rbind(au.usgs.stations.pro.area,au.owrd.stations.pro.area)

# __ Map ----
print(paste0(project_area,"......map"))
au.usgs.stations.pro.area.shp <- au.usgs.stations.pro.area %>% 
  dplyr::filter(!is.na(Latitude)) %>% 
  dplyr::mutate(lat = Latitude,
                long = Longitude) %>%
  sf::st_as_sf(coords = c("long", "lat"), crs = sf::st_crs("+init=EPSG:4326"))

au.owrd.stations.pro.area.shp <- au.owrd.stations.pro.area %>% 
  dplyr::filter(!is.na(Latitude)) %>% 
  dplyr::mutate(lat = Latitude,
                long = Longitude) %>%
  sf::st_as_sf(coords = c("long", "lat"), crs = sf::st_crs("+init=EPSG:4326"))

pal.rivers <- leaflet::colorFactor(palette = 'Set1',domain = cat.45.rivers.pro.area$AU_ID)
pal.waterbodies <- leaflet::colorFactor(palette = 'Set1',domain = cat.45.waterbodies.pro.area$AU_ID)
pal.watershed <- leaflet::colorFactor(palette = 'Set1',domain = cat.45.watershed.pro.area$AU_ID)

map.title <- htmltools::tags$div(tag.map.title, htmltools::HTML(paste0(project_area)))
map <- leaflet::leaflet() %>%
  leaflet::addControl(map.title, position = "topleft", className="map-title") %>% 
  leaflet::addMapPane("OpenStreetMap", zIndex = -2000) %>% 
  leaflet::addMapPane("pro_areas", zIndex = -1000) %>%
  leaflet::addMapPane("hydrotiles", zIndex = -800) %>%
  leaflet::addMapPane("ws_au", zIndex = -500) %>%
  leaflet::addMapPane("ws_au_gnis", zIndex = -300) %>%
  leaflet::addMapPane("sr_au", zIndex = -300) %>%
  leaflet::addMapPane("wb_au", zIndex = -300) %>%
  leaflet::addMapPane("stations", zIndex = 100) %>%
  leaflet::addProviderTiles(providers$OpenStreetMap,
                            options = pathOptions(pane = "OpenStreetMap")) %>% 
  leaflet::addWMSTiles(baseUrl="https://basemap.nationalmap.gov/arcgis/services/USGSHydroCached/MapServer/WmsServer",
                       group = "Stream Names (USGS NHD)",
                       options = leaflet::WMSTileOptions(format = "image/png",
                                                         transparent = TRUE,
                                                         pane= "hydrotiles"),
                       attribution = '<a href="https://basemap.nationalmap.gov/arcgis/rest/services/USGSHydroCached/MapServer">USGS The National Map: National Hydrography Dataset.</a>',
                       layers = "0") %>%
  leaflet::addPolygons(data = pro_areas[which(pro_areas$Project_Na == project_area),]$geometry,fillColor = "transparent",
                       options = leaflet::leafletOptions(pane="pro_areas"),) %>%
  leaflet::addPolygons(data = cat.45.watershed.pro.area,
                       color = ~pal.watershed(AU_ID),
                       label = ~AU_Name,
                       group = "2020/2022 303(d) Temperature Listed - Watersheds",
                       options = leaflet::leafletOptions(pane="ws_au")) %>% 
  leaflet::addPolylines(data = cat.45.watershed.gnis.pro.area,
                       color = "blue",
                       label = ~GNIS_Name,
                       fill=FALSE,
                       group = "2020/2022 303(d) Temperature Listed - Watershed AU GNIS",
                       options = leaflet::leafletOptions(pane="ws_au_gnis")) %>% 
  leaflet::addPolylines(data = cat.45.rivers.pro.area, 
                        color = ~pal.rivers(AU_ID),
                        label = ~AU_Name,
                        opacity = 1,
                        weight = 5,
                        fill=FALSE,
                        group = "2020/2022 303(d) Temperature Listed - Streams",
                        options = leaflet::leafletOptions(pane="sr_au")) %>% 
  leaflet::addMarkers(data = au.usgs.stations.pro.area.shp,
                      label = ~paste0("USGS:",Station_ID),
                      group = "USGS Flow Stations",
                      options = leaflet::leafletOptions(pane="stations")) %>% 
  leaflet::addMarkers(data = au.owrd.stations.pro.area.shp,
                      label = ~paste0("OWRD:",Station_ID),
                      group = "OWRD Flow Stations",
                      options = leaflet::leafletOptions(pane="stations")) %>% 
  leaflet::addLayersControl(overlayGroups = c("USGS Flow Stations",
                                              "OWRD Flow Stations",
                                              "2020/2022 303(d) Temperature Listed - Streams",
                                              "2020/2022 303(d) Temperature Listed - Waterbodies",
                                              "2020/2022 303(d) Temperature Listed - Watersheds",
                                              "2020/2022 303(d) Temperature Listed - Watershed AU GNIS",
                                              "Stream Names (USGS NHD)"),
                            options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = TRUE))
  
  if(!project_area %in% c("Lower Willamette and Clackamas Subbasins",
                          "Middle Columbia-Hood, Miles Creeks",
                          "North Umpqua Subbasin",
                          "Walla Walla Subbasin",
                          "Willow Creek Subbasin")){
    map <- map %>% 
      leaflet::addPolygons(data = cat.45.waterbodies.pro.area,
                           color = ~pal.waterbodies(AU_ID),
                           label = ~AU_Name,
                           group = "2020/2022 303(d) Temperature Listed - Waterbodies",
                           options = leaflet::leafletOptions(pane="wb_au"))
    }

print(paste0(project_area,"......save the map"))
htmlwidgets::saveWidget(map,paste0(project_area,".html"),selfcontained = TRUE) #selfcontained needs to be in the current working directory
file.rename(paste0(project_area,".html"), paste0("./maps/",project_area,".html"))

# __ Result table ----
print(paste0(project_area,"......result table"))
tbl.pro.area <- cat.45.pro.area %>% 
  sf::st_drop_geometry() %>% 
  dplyr::left_join(au.usgs.owrd.stations.pro.area,by="AU_ID") %>% 
  dplyr::select(Project_Area = QAPP_Project_Area,
                AU_ID,AU_Name,
                IR_Category = AU_parameter_category,
                GNIS_Name,AU_GNIS_Name,Data_Source,Station_ID,Station_Name,Latitude,Longitude,Period_Start,Period_End) %>% 
  dplyr::mutate(`7Q10_cfs` = NA,
                Years_of_Record_Used = NA)

# __ USGS and OWRD flow data ----
print(paste0(project_area,"......USGS and OWRD flow data"))
usgs.station.flow.data <- usgs.flow.data %>% 
  dplyr::filter(site_no %in% sort(unique(tbl.pro.area$Station_ID))) %>% 
  dplyr::filter(!is.na(site_no)) %>% 
  dplyr::mutate(POSIXct_date = as.Date(dateTime),
                daily_mean_flow = as.numeric(X_00060_00003)) %>% 
  dplyr::select(POSIXct_date,daily_mean_flow,
                Station_ID = site_no,
                Data_Source = agency_cd)

owrd.station.flow.data <- owrd.flow.data %>% 
  dplyr::filter(station_nbr %in% sort(unique(tbl.pro.area$Station_ID))) %>% 
  dplyr::filter(!is.na(station_nbr)) %>% 
  dplyr::mutate(POSIXct_date = as.Date(record_date,format="%m-%d-%Y"),
                daily_mean_flow = as.numeric(Result.Value),
                Data_Source = "OWRD") %>% 
  dplyr::select(POSIXct_date,daily_mean_flow,
                Station_ID = station_nbr,
                Data_Source)

usgs.owrd.flow.data <- rbind(usgs.station.flow.data,owrd.station.flow.data) 

# __ Sandy Flow Chart ----
# for(site_no in sort(unique(usgs.owrd.flow.data$Station_ID))){
#   # test: site_no <- "14130000"
#   flow_chart <- ggplot2::ggplot(data = usgs.owrd.flow.data[which(usgs.owrd.flow.data$Station_ID == site_no),],
#                                 ggplot2::aes(x = as.Date(POSIXct_date), y = daily_mean_flow)) +
#     ggplot2::geom_point() +
#     ggplot2::ggtitle(paste0("USGS Station ID:",
#                             usgs.owrd.flow.data[which(usgs.owrd.flow.data$Station_ID == site_no),]$Station_ID,
#                             "\n Data Start Date:",min(as.Date(usgs.owrd.flow.data[which(usgs.owrd.flow.data$Station_ID == site_no),]$POSIXct_date)),
#                             "\n Data End Date:",max(as.Date(usgs.owrd.flow.data[which(usgs.owrd.flow.data$Station_ID == site_no),]$POSIXct_date)))) +
#     ggplot2::xlab("Date") +
#     ggplot2::ylab("Discharge, cubic feet per second")
# }
# flow_chart

# __ 7Q10 ----
print(paste0(project_area,"......7Q10"))
for(site_no in sort(unique(usgs.owrd.flow.data$Station_ID))){
  # test: site_no <- "14037500"
  print(site_no)
  first_column <- usgs.owrd.flow.data[which(usgs.owrd.flow.data$Station_ID == site_no),]$POSIXct_date
  second_column <- usgs.owrd.flow.data[which(usgs.owrd.flow.data$Station_ID == site_no),]$daily_mean_flow
  x <- data.frame(first_column, second_column)
  m <- 7
  r <- 10
  `7Q10_cfs` <- dflowR::dflow(x,m,r)
  tbl.pro.area[which(tbl.pro.area$Station_ID == site_no),]$`7Q10_cfs` <- `7Q10_cfs`
  subset <- usgs.owrd.flow.data %>% 
    filter(Station_ID == site_no) %>% 
    mutate(year = lubridate::year(POSIXct_date))
  tbl.pro.area[which(tbl.pro.area$Station_ID == site_no),]$Years_of_Record_Used <- length(unique(sort(subset[which(subset$Station_ID == site_no),]$year)))-1
  tbl.pro.area <- tbl.pro.area %>% 
    dplyr::select(Project_Area,AU_ID,AU_Name,IR_Category,GNIS_Name,AU_GNIS_Name,`7Q10_cfs`,Data_Source,Station_ID,Station_Name,Latitude,Longitude,Years_of_Record_Used,Period_Start,Period_End)
}

writexl::write_xlsx(tbl.pro.area,paste0("./data/tbl_",project_area,".xlsx"))

}

save.image()
load(".RData")

# Task: summarize AUs ----
## for test:
# project_area = "John Day River Basin"
# project_area = "Lower Grande Ronde, Imnaha, and Wallowa Subbasins"
# project_area = "Lower Willamette and Clackamas Subbasins"
# project_area = "Malheur River Subbasins"
# project_area = "Middle Willamette Subbasins"
# project_area = "Middle Columbia-Hood, Miles Creeks"
# project_area = "North Umpqua Subbasin"
# project_area = "Rogue River Basin"
# project_area = "Sandy Subbasin"
# project_area = "South Umpqua and Umpqua Subbasins"
# project_area = "Southern Willamette Subbasins"
# project_area = "Walla Walla Subbasin"
# project_area = "Willow Creek Subbasin"
tbl <- NULL
for(project_area in pro_areas$Project_Na){
  print(project_area)
  tbl.i <- readxl::read_xlsx(paste0("./data/tbl_",project_area,".xlsx"), sheet = "Sheet1") %>% 
    dplyr::mutate(Period_Start = as.character(Period_Start),
                  Period_End = as.character(Period_End))
  tbl <- rbind(tbl,tbl.i)
}

tbl <- tbl %>% dplyr::mutate(`7Q10_cfs_Count` = ifelse(is.na(`7Q10_cfs`),0,1))
write.csv(tbl,file=paste0("./data/tbl_allAreas.csv"))

library(pivottabler)
pt <- PivotTable$new()
pt$addData(tbl)
pt$addRowDataGroups("Project_Area")
pt$addRowDataGroups("AU_ID")
pt$defineCalculation(calculationName="AU_ID_Count", summariseExpression="n()")
pt$defineCalculation(calculationName="7Q10_cfs_Count", summariseExpression="sum(`7Q10_cfs_Count`, na.rm=TRUE)")
pt$evaluatePivot()

library(openxlsx)
wb <- createWorkbook(creator = Sys.getenv("USERNAME"))
addWorksheet(wb, "Data")
pt$writeToExcelWorksheet(wb=wb, wsName="Data", 
                         topRowNumber=1, leftMostColumnNumber=1, applyStyles=FALSE)
saveWorkbook(wb, file=paste0("./data/tbl_counts.xlsx"), overwrite = TRUE)


