require(tidyverse)
require(modelr)
require(ggfortify)
require(jsonlite)

# config <- RcppTOML::parseToml("analysis/sealevelmonitor/_common/configuration.TOML")





#== Read and prepare data ===================================================


readDDLwaterhoogte <- function(station, startyear, endyear, grootheid = "Waterhoogte", outDir = "data/rijkswaterstaat/ddl/raw"){
  
  require(rwsapi)
  require(tidyverse)
  
  waterhoogteparameters <- c("Waterhoogte berekend", "Waterhoogte", "Waterhoogte astronomisch", "Waterhoogte verwacht")    

  # make warning if grootheid is not in list of waterhoogteparameters.  
  
  md <- rwsapi::rws_metadata()
  thisCatalogue <- md$content$AquoMetadataLijst %>% 
    unnest(
      names_sep = ".", 
      c(Compartiment, Eenheid, Grootheid, Hoedanigheid, Parameter)) %>% 
    filter(Grootheid.Omschrijving == grootheid) %>%
    left_join(
      md$content$AquoMetadataLocatieLijst, 
      by = c(AquoMetadata_MessageID = "AquoMetaData_MessageID")
    ) %>%
    left_join(md$content$LocatieLijst) %>% 
    filter(Code %in% station) %>%
    rename_with(tolower) %>%
    rename(
      locatie.naam = naam,
      locatie.code = code
    )
  
  for(iyear in startyear:endyear){
    rwsapi::getDDLdata( 
      startyear = iyear,
      endyear = iyear,
      myCatalogue = thisCatalogue,
      outDir = outDir
    )
  }
}




addPreviousYearHeight <- function(df){
  df %>%
    dplyr::group_by(station) %>%
    dplyr::mutate(previousYearHeight = height[match(year - 1, year)]) %>%
    dplyr::filter(year > min(year)) %>%
    dplyr::ungroup()
}

addSurgeAnomaly = function(df){
  df %>%
    dplyr::mutate(surge_anomaly = - (`height - surge anomaly` - height))
}

addBreakPoints = function(df){
  df %>%
    dplyr::mutate(from1993 = (year >= 1993) * (year - 1993)) %>%
    dplyr::mutate(from1960_square = (year >= 1960) * (year - 1960) * (year - 1960))
}

selectCols <- function(df){
  df %>%
    tidyr::drop_na(station) %>%
    dplyr::select(
      year, 
      from1960_square,
      from1993,
      epoch,
      height,
      station = name_rws,
      surge_anomaly
    ) %>%
    dplyr::mutate(station = factor(station, levels = config$runparameters$station))
}


readSeaLevelData <- function(url){
  readr::read_csv(url, comment = "#")
}  %>% mutate(epoch = epoch) %>%
  addSurgeAnomaly() %>%
  addBreakPoints() %>%
  selectCols()

readMainStationInfo <- function(filepath = "") {
  jsonlite::read_json(
    path = ifelse(
      filepath == "", 
      "data/deltares/main_stations.json",
      file.path(filepath, "data/deltares/main_stations.json")
    )
    
  ) %>%
  purrr::map_df(~ unlist(.[1:15]))
}

readMainStationLocations <- function(path = ""){
  read_delim(
    file = ifelse(
      path == "",
      "data/psmsl/NLstations.csv",
      file.path(path, "data/psmsl/NLstations.csv")
    ),
    delim = ";", escape_double = FALSE, trim_ws = TRUE)
}

read_gtsm_nc <- function(nc = "c:\\Temp\\era5_reanalysis_surge_2023_v1_monthly_mean.nc", stations_selected){
  require(RNetCDF)
  ncf <- RNetCDF::open.nc(nc)
  
  data <- RNetCDF::read.nc(ncf)

  stations <- tibble::tibble(
    gtsmid = data$stations, 
    stationname = data$station_name,
    station_x_coordinate = data$station_x_coordinate,
    station_y_coordinate = data$station_y_coordinate
  )
  df <- reshape2::melt(data$surge, value.name = "surge_m") %>%
    dplyr::mutate(
      gtsmid = data$stations[Var2],
      month      = data$month[Var1]
    ) %>%
    dplyr::left_join(stations) %>%
    dplyr::select(-Var1, -Var2) %>%
    dplyr::filter(gtsmid %in% stations_selected)
  
  return(df)
}

read_yearly_gtsm <- function(filename = "data/deltares/gtsm/gtsm_surge_annual_mean_main_stations_2024.csv") {
  
  gtsm_surge_annual_mean_main_stations_2024 <- read.csv(filename, comment = "#", )
  
}
  
  read_yearly_psmsl_csv  <- function(station_nr, filepath){
    
    base_rlr_url = "https://psmsl.org/data/obtaining/rlr.annual.data/"
    base_rlr_ext = ".rlrdata"
    
    rlr_df <- lapply(station_nr, 
                     function(x) {
                       read_delim(
                         file = paste0(base_rlr_url, x, base_rlr_ext), 
                         col_names = c("year", "rlr_height_mm", "interpolated", "flag"),
                         col_types = c("nncc"),
                         na = "-99999",
                         delim = ";"
                       ) |>
                         mutate(psmsl_id = as.character(x))
                     }
    ) |>
      bind_rows()
    
    mainStationInfo <- readMainStationInfo(filepath) |>
      select(psmsl_id, name, `nap-rlr`, gtsm_id)
    
    rlr_df <- rlr_df %>% left_join(mainStationInfo, by = c(psmsl_id = "psmsl_id"))
    
    return(rlr_df)
        
  }
  

  
  
  # hieronder is nog niet helemaal af, zie yearly psmsl function
  read_monthly_psmsl_csv  <- function(station_nr){
    
    base_rlr_url = "https://psmsl.org/data/obtaining/rlr.monthly.data/"
    base_rlr_ext = ".rlrdata"

    rlr_df <- lapply(station_nr,
                     function(x) {
                       rlr_df <- read_delim(
                         file = paste0(base_rlr_url, x, base_rlr_ext), 
                         col_names = c("decimal_year", "rlr_height_mm", "interpolated", "flag"),
                         col_types = "niic",
                         delim = ";",
                         trim_ws = T, 
                         locale = locale(decimal_mark = "."
                         )
                       ) |>
                         mutate(psmsl_id = as.character(x))
                     }
    ) |>
      bind_rows()
    
    
    return(rlr_df)
    
  }
  



read_tidal_components_csv <- function(filesdir = "p:/11202493--systeemrap-grevelingen/1_data/Wadden/ddl/calculated/TA_filtersurge") {
  
  filelist <- list.files(filesdir, pattern = "csv", full.names = T)
  # get names of stations and year from filenames in filelistShort
  filelistShort <- list.files(filesdir, pattern = "csv", full.names = F)
  
  df <- lapply(filelist, function(x) read_csv(x, col_types = cols(), progress = FALSE))
  dfs <- dplyr::bind_rows(df)
  
  names <- tibble(name = str_replace(filelistShort, pattern = "_UTC\\+1.csv", replacement = "")) %>%
    tidyr::separate(name, c("station", "jaar", "component"), sep = "_") %>%
    dplyr::select(-component) %>%
    dplyr::left_join(mainstations_df[,c("ddl_id", "name")], by = c(station = "ddl_id"))
  
  names %>% 
    dplyr::mutate(jaar = as.integer(jaar)) %>%
    dplyr::mutate(data = df)
}


#==== NOT TESTED AT THE MOMENT, THIS IS A CONCEPT ==============

use_gtsm <- function(){
    params$wind_or_surge_type == "GTSM"
}


check_workflow_wind <- function(wind_or_surge_type){
  if(!wind_or_surge_type %in% config$runparameters$wind_or_surge_types) {
    cat("incorrect wind or surge specification")
  } else
  {
    if(use_gtsm()) {
      cat("Surge correction by GTSM is used")
    } else{
      cat("Wind correction ", wind_or_surge_type, " is used")
    }
  }
}

#==== Model functions ============================================

# linear_model <- function(df){
#     glm(
#       reformulate(
#         c(
#           ifelse(use_gtsm(), config$model_terms$surge_anomaly, config$model_terms$wind_anomaly),
#           config$model_terms$linear_time_term,
#           # config$model_terms$autocorrelation_term,
#           config$model_terms$nodal_term
#         ),
#         config$model_terms$response_term
#       ),
#       family = gaussian(link = "identity"),
#       data = df
#     )
# }

# linear_model <- function(df){
#   glm(
#     height ~ offset(surge_anomaly) + I(year - epoch) + I(cos(2 * pi * (year - epoch)/(18.613))) + I(sin(2 * pi * (year - epoch)/(18.613))),
#     data = df
#   )
# }


linear_model <- function(df){
  if(use_gtsm()){
    lm(
      height ~ offset(surge_anomaly) + 
        I(year - epoch) + 
        I(cos(2 * pi * (year - epoch)/(18.613))) + 
        I(sin(2 * pi * (year - epoch)/(18.613))),
      data = df
    )
  } else {
    lm(
      height ~ wind_anomaly + 
        I(year - epoch) + 
        I(cos(2 * pi * (year - epoch)/(18.613))) + 
        I(sin(2 * pi * (year - epoch)/(18.613))),
      data = df
    )
  }
}


broken_linear_model <- function(df){
  
  if(use_gtsm()){
    lm(
      height ~ offset(surge_anomaly) + 
        I(year - epoch) + 
        from1993 + 
        I(cos(2 * pi * (year - epoch)/(18.613))) + 
        I(sin(2 * pi * (year - epoch)/(18.613))),
      data = df
    )
    
  } else {
    lm(
      height ~ wind_anomaly +
        I(year - epoch) +
        from1993 +
        I(cos(2 * pi * (year - epoch)/(18.613))) +
        I(sin(2 * pi * (year - epoch)/(18.613))),
      data = df
    )
  }
}

## nog aanpassen, zoals hierboven

squared_model <- function(df){
  glm(
    reformulate(
      c(
        ifelse(use_gtsm(), config$model_terms$surge_anomaly, config$model_terms$wind_anomaly),
        config$model_terms$squared_time_term,
        config$model_terms$autocorrelation_term,
        config$model_terms$nodal_term
      ),
      config$model_terms$response_term
    ),
    family = gaussian(link = "identity"),
    data = df
  )
}

broken_squared_model <- function(df){
  
  if(use_gtsm()){
    lm(
      height ~ offset(surge_anomaly) + 
        I(year - epoch) + 
        from1960_square + 
        I(cos(2 * pi * (year - epoch)/(18.613))) + 
        I(sin(2 * pi * (year - epoch)/(18.613))),
      data = df
    )
    
  } else {
    lm(
      height ~ wind_anomaly +
        I(year - epoch) +
        from1960_square + 
        I(cos(2 * pi * (year - epoch)/(18.613))) +
        I(sin(2 * pi * (year - epoch)/(18.613))),
      data = df
    )
  }
}

