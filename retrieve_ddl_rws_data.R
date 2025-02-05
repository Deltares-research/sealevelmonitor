  ## Get DDL sea level data for main stations. 
  ## 
  ## 
  ## to do: connect station code to station name (ijmuiden is not correct) and gtsm_id
  
  source("analysis/sealevelmonitor/_common/functions.R")
  
  require(tidyverse)
  require(rwsapi)
  
  datayear = 2022:2023
  
  # for all RWS North Sea stations
  stationlist <- read_delim(
    "data/rijkswaterstaat/stationlist.csv", 
    locale = readr::locale(encoding = 'WINDOWS-1252'), 
    delim = ",", 
    escape_double = FALSE, 
    trim_ws = TRUE) %>%
    distinct(stationname) %>% unlist() %>% unname()
  
  # for 6 "hoofdstations"
  # stationlist = readMainStationInfo(filepath = "") %>%
  #   distinct(ddl_id) %>% unlist %>% unname
  
  
  ddlrawdir <- "P:/11202493--systeemrap-grevelingen/1_data/Noordzee/ddl/raw/wathte"
  mainstations_df <- readMainStationInfo(filepath = "")
  mainstationcodes <- mainstations_df$ddl_id
  
  mijnmetadata <- get_selected_metadata(grootheid = "WATHTE", locatie = stationlist)
  
  

# mijnmetadata %>% 
#   distinct(coordinatenstelsel, x, y, locatie.naam, locatie.code) %>%
#   sf::st_as_sf(coords = c("x","y"), crs = unique(.$coordinatenstelsel)) %>%
#   sf::st_write("data/rijkswaterstaat/waterhoogtestations.geojson")
# mijnmetadata %>% 
#   distinct(coordinatenstelsel, x, y, locatie.naam, locatie.code) %>%
#   write_csv2("data/rijkswaterstaat/waterhoogtestations.csv")

# newStationList <- mijnmetadata %>% 
#   distinct(grootheid.code,
#            hoedanigheid.code,
#            locatie.naam,
#            locatie.code)
# write_csv(newStationList, "data/rijkswaterstaat/newStationList_0.csv")

#  missing in earlier retrievals. 
mijnselectie <- mijnmetadata %>% filter(grepl("plat", locatie.naam, ignore.case = T)) #%>%
  # mutate(
  #   x = str_pad(as.character(x), 16, "right", "0"),
  #   y = str_pad(as.character(y), 16, "right", "0")
  # )

readDDLwaterhoogte2(
  ddlmetadata = mijnselectie, 
  startyear = min(datayear), 
  endyear = max(datayear), 
  outDir = ddlrawdir
)
