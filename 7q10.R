# devtools::install_github('DEQrmichie/dflowR', host = 'https://api.github.com', dependencies= TRUE, force = TRUE, upgrade='never')
library(dflowR)
library(dplyr)
library(leaflet)

# data.sharedrive <- "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/model_QAPPs/R/data/"
# 
# # Project area ----
# ## Not include Willamette MS ----
# pro_areas <- sf::st_read(dsn = paste0(data.sharedrive,"gis/project_areas.shp"),layer = "project_areas") %>% sf::st_transform(4326)
# ## Willamette MS ----
# willamette_huc12 <- sf::read_sf(dsn = "//deqhq1/TMDL/Planning statewide/Temperature_TMDL_Revisions/GIS/willa_snake/TempTMDL_QAPP_Reaches_data_query_HUC12s.shp",
#                                 layer = "TempTMDL_QAPP_Reaches_data_query_HUC12s") %>% 
#   dplyr::filter(Project_Na == "Willamette River Mainstem and Major Tributaries") %>% 
#   sf::st_transform(4326)
# willamette_huc12_union <- sf::st_union(willamette_huc12)
# 
# # Lookup HUCs ----
# # Include Willamette MS
# lookup.huc <- readxl::read_xlsx(paste0(data.sharedrive, "Lookup_QAPPProjectArea.xlsx"), sheet = "Lookup_QAPPProjectArea") %>%
#   dplyr::mutate(HUC_6 = as.character(HUC_6),
#                 HUC_8 = as.character(HUC_8),
#                 HUC10 = as.character(HUC10),
#                 HUC12 = as.character(HUC12))
# 
# # IR 2020-2022 CAT 4&5 ----
# cat.45.rivers <- sf::st_read(dsn = "./data/gis/2020-2022_IR_Cat45_Temperature_Rivers_project_areas.shp",
#                              layer = "2020-2022_IR_Cat45_Temperature_Rivers_project_areas") %>% sf::st_zm() %>% sf::st_transform(4326) %>%
#   dplyr::select(-c(AU_WBType, AU_UseCode, AQWMS_NUM, AQWMS_TXT, wqstd_code, AU_delist, Rationale, previous_r, year_asses, recordID, action_ID, action_TMD, TMDL_Prior
# )) %>%
#   dplyr::rename(AU_Description = AU_Descrip,
#                 AU_parameter_category = AU_paramet,
#                 assessed_2022 = assessed_2,
#                 Year_listed = Year_liste,
#                 TMDL_Project = TMDL_Proje,
#                 QAPP_Project_Area = QAPP_Proje)
# 
# leaflet::leaflet() %>%
#   leaflet::addPolygons(data = pro_areas) %>%
#   leaflet::addPolylines(data = cat.45.rivers)
# 
# # AWQMS station database ----
# ## See AWQMSdata_fix.R to install AWQMSdata package
# library(AWQMSdata)
# huc8 <- lookup.huc %>% dplyr::select(HUC_8) %>% dplyr::distinct() %>% dplyr::pull()
# awqms.stations <- AWQMSdata::query_stations(state="OR",stations_odbc="STATIONS", huc8=huc8)
# save(awqms.stations, file=paste0("./data/awqms_stations.RData")) # download date: 6/15/2022
# 
# # USGS stations and data ----
# # install.packages("dataRetrieval")
# library(dataRetrieval)
# ## Github: https://github.com/USGS-R/dataRetrieval
# ## Stations:
# ### OR stations:
# usgs.fl.stations.or <- dataRetrieval::whatNWISdata(stateCd="OR", parameterCd = "00060") # 00060	= Discharge [ft3/s]
# ### WA stations for Willamette Mainstem QAPP: 
# usgs.fl.stations.wa <- dataRetrieval::whatNWISdata(stateCd="WA", parameterCd = "00060")
# usgs.fl.stations.wa.will <- usgs.fl.stations.wa %>% 
#   dplyr::mutate(lat = dec_lat_va,
#                 long = dec_long_va) %>%
#   sf::st_as_sf(coords = c("long", "lat"), crs = sf::st_crs("+init=EPSG:4326"))
# st_as_s2(FALSE)
# usgs.fl.stations.wa.will <- sf::st_intersection(usgs.fl.stations.wa.will,willamette_huc12_union) %>% 
#   sf::st_drop_geometry()
# usgs.fl.stations <- rbind(usgs.fl.stations.or,usgs.fl.stations.wa.will)
# ## Data:
# usgs.fl.data <- NULL
# for (id in unique(sort(usgs.fl.stations$site_no))) {
#   print(id)
#   usgs.fl.data.i <- dataRetrieval::readNWISdata(siteNumber = id,
#                                                 parameterCd = "00060",    # and statCd = "00003" for daily mean which is default
#                                                 startDate = "1878-06-01", # min(usgs.fl.stations$begin_date)
#                                                 endDate = "2022-06-14")   # max(usgs.fl.stations$end_date)
#   usgs.fl.data <- dplyr::bind_rows(usgs.fl.data,usgs.fl.data.i)
# }
# save(usgs.fl.stations,usgs.fl.data, file=paste0("./data/usgs_flow.RData")) # download date: 6/15/2022
# 
# au.usgs.stations <- usgs.fl.stations %>% 
#   dplyr::left_join(awqms.stations,by=c("site_no"="MLocID")) %>% # Add AU ID to the USGS station table
#   dplyr::left_join(cat.45.rivers,by="AU_ID") %>% # Join IR 2020-2022 CAT 4&5
#   dplyr::filter(HUC8 %in% huc8) # Filter HUC within the project areas
# 
# writexl::write_xlsx(au.usgs.stations,"./data/au_usgs_stations.xlsx")
# writexl::write_xlsx(usgs.fl.stations,"./data/usgs_fl_stations.xlsx")
# 
# save(data.sharedrive,
#      pro_areas,
#      willamette_huc12,
#      willamette_huc12_union,
#      lookup.huc,
#      cat.45.rivers,
#      huc8,
#      awqms.stations,
#      usgs.fl.stations.or,
#      usgs.fl.stations.wa,
#      usgs.fl.stations.wa.will,
#      usgs.fl.stations,
#      usgs.fl.data,
#      au.usgs.stations,
#      file = "data.RData")
load("data.RData")

# Sandy Subbasin ----
project_area <- "Sandy Subbasin"

# __ Sandy AUs ----
cat.45.rivers.pro.area <- sf::st_intersection(cat.45.rivers,pro_areas[which(pro_areas$Project_Na == project_area),]$geometry) %>% 
  dplyr::distinct(AU_ID, .keep_all = TRUE)

# __ Sandy USGS stations ----
au.usgs.stations.pro.area <- au.usgs.stations %>% 
  dplyr::filter(QAPP_Project_Area == project_area) %>% 
  dplyr::distinct(site_no, .keep_all = TRUE) %>% 
  dplyr::select(AU_ID,GNIS_Name,agency_cd,site_no,station_nm,dec_lat_va,dec_long_va,begin_date,end_date) 

# __ map ----
au.usgs.stations.pro.area.shp <- au.usgs.stations.pro.area %>% 
  dplyr::mutate(lat = dec_lat_va,
                long = dec_long_va) %>%
  sf::st_as_sf(coords = c("long", "lat"), crs = sf::st_crs("+init=EPSG:4326"))

pal <- leaflet::colorFactor(palette = 'Set1',domain = au.usgs.stations.pro.area.shp$AU_ID)

leaflet::leaflet() %>%
  leaflet::addProviderTiles(providers$OpenStreetMap) %>% 
  leaflet::addPolygons(data = pro_areas[which(pro_areas$Project_Na == project_area),]$geometry,fillColor = "transparent") %>%
  leaflet::addPolylines(data = cat.45.rivers.pro.area, color = ~pal(AU_ID)) %>% 
  leaflet::addMarkers(data = au.usgs.stations.pro.area.shp)

# __ Sandy result table ----
tbl.pro.area <- cat.45.rivers.pro.area %>% 
  sf::st_drop_geometry() %>% 
  dplyr::left_join(au.usgs.stations.pro.area,by="AU_ID") %>% 
  dplyr::select(QAPP_Project_Area,AU_ID,AU_Name,AU_parameter_category,GNIS_Name,agency_cd,site_no,station_nm,dec_lat_va,dec_long_va,begin_date,end_date) %>% 
  dplyr::mutate(`7Q10_cfs` = NA)

writexl::write_xlsx(tbl.pro.area,"./data/tbl_pro_area.xlsx")

# __ Sandy USGS station data ----
usgs.station.flow.data <- usgs.fl.data %>% 
  dplyr::filter(site_no %in% sort(unique(tbl.pro.area$site_no))) %>% 
  dplyr::filter(!is.na(site_no)) %>% 
  dplyr::mutate(POSIXct_date = as.Date(dateTime),
                daily_mean_flow = as.numeric(X_00060_00003)) %>% 
  dplyr::select(POSIXct_date,daily_mean_flow,site_no,agency_cd)

# __ Flow Chart ----
# for(site_no in sort(unique(usgs.station.flow.data$site_no))){
#   # test: site_no <- "14130000"
#   flow_chart <- ggplot2::ggplot(data = usgs.station.flow.data[which(usgs.station.flow.data$site_no == site_no),],
#                                 ggplot2::aes(x = as.Date(dateTime), y = X_00060_00003)) +
#     ggplot2::geom_point() +
#     ggplot2::ggtitle(paste0("USGS Station ID:",
#                             usgs.station.flow.data[which(usgs.station.flow.data$site_no == site_no),]$site_no,
#                             "\n Data Start Date:",min(as.Date(usgs.station.flow.data[which(usgs.station.flow.data$site_no == site_no),]$dateTime)),
#                             "\n Data End Date:",max(as.Date(usgs.station.flow.data[which(usgs.station.flow.data$site_no == site_no),]$dateTime)))) +
#     ggplot2::xlab("Date") +
#     ggplot2::ylab("Discharge, cubic feet per second")
# }

# __ 7Q10 ----
for(site_no in sort(unique(usgs.station.flow.data$site_no))){
  # test: site_no <- "14130000"
  print(site_no)
  first_column <- usgs.station.flow.data[which(usgs.station.flow.data$site_no == site_no),]$POSIXct_date
  second_column <- usgs.station.flow.data[which(usgs.station.flow.data$site_no == site_no),]$daily_mean_flow
  x <- data.frame(first_column, second_column)
  m <- 7
  r <- 10
  `7Q10_cfs` <- dflowR::dflow(x,m,r)
  tbl.pro.area[which(tbl.pro.area$site_no == site_no),]$`7Q10_cfs` <- `7Q10_cfs`
}

writexl::write_xlsx(tbl.pro.area,"./data/tbl_pro_area.xlsx")

save.image()
