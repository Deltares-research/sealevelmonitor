## Get DDL sea level data for main stations. 
## 
## 
## to do: connect station code to station name (ijmuiden is not correct) and gtsm_id

source("analysis/sealevelmonitor/_common/functions.R")

require(tidyverse)
require(rwsapi)

datayear = 2023:2023

# for all RWS North Sea stations
stationlist <- read_delim("data/rijkswaterstaat/stationlist.csv", 
                          delim = ",", escape_double = FALSE, trim_ws = TRUE) %>%
  distinct(stationname) %>% unlist() %>% unname()

# for 6 "hoofdstations"
# stationlist = readMainStationInfo(filepath = "") %>%
#   distinct(ddl_id) %>% unlist %>% unname


ddlrawdir <- "P:/11202493--systeemrap-grevelingen/1_data/Noordzee/ddl/raw/wathte"
mainstations_df <- readMainStationInfo(filepath = "")
mainstationcodes <- mainstations_df$ddl_id

mijnmetadata <- get_selected_metadata(compartiment = "OW", grootheid = "WATHTE", locatie = stationlist)
mijnmetadata <- tail(mijnmetadata, )

readDDLwaterhoogte2(
  ddlmetadata = mijnmetadata, 
  startyear = min(datayear), 
  endyear = max(datayear), 
  outDir = ddlrawdir
  )
