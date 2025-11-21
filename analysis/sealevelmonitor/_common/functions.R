require(tidyverse)
require(modelr)
require(ggfortify)
require(jsonlite)
require(magrittr)

# config <- RcppTOML::parseToml("analysis/sealevelmonitor/_common/configuration.TOML")

waterhoogteparameters <- c("Waterhoogte berekend", "Waterhoogte", "Waterhoogte astronomisch", "Waterhoogte verwacht")    
grootheid = "Waterhoogte"

# make warning if grootheid is not in list of waterhoogteparameters.  

# get_selected_metadata <- function(
#     compartiment = NULL, 
#     grootheid = NULL, 
#     parameter = NULL, 
#     locatie = NULL
# ) {
#   
#   require(rwsapi)
#   require(tidyverse)
#   
#   md <- rwsapi::rws_metadata()
#   
#   md$content$AquoMetadataLijst %>% 
#     unnest(
#       names_sep = ".", 
#       c(Compartiment, Eenheid, Grootheid, Hoedanigheid, Parameter)) %>% 
#     filter(
#       if(is.null(grootheid)) TRUE else Grootheid.Omschrijving %in% grootheid | Grootheid.Code %in% grootheid,
#       if(is.null(parameter)) TRUE else Parameter.Omschrijving %in% parameter | Parameter.Code %in% parameter,
#       if(is.null(compartiment)) TRUE else Compartiment.Code %in% compartiment | Compartiment.Code %in% compartiment
#     ) %>%
#     left_join(
#       md$content$AquoMetadataLocatieLijst, 
#       by = c(AquoMetadata_MessageID = "AquoMetaData_MessageID")
#     ) %>%
#     left_join(md$content$LocatieLijst) %>%
#     filter(if(is.null(locatie)) TRUE else Naam %in% locatie |  Code %in% locatie) %>% 
#     rename_with(tolower) %>%
#     rename(
#       locatie.naam = naam,
#       locatie.code = code
#     )
# }
# 


#== Read and prepare data ===================================================


# readDDLwaterhoogte <- function(station, startyear, endyear, grootheid = "Waterhoogte", outDir = "data/rijkswaterstaat/ddl/raw"){
#   
#   require(rwsapi)
#   require(tidyverse)
#   
#   waterhoogteparameters <- c("Waterhoogte berekend", "Waterhoogte", "Waterhoogte astronomisch", "Waterhoogte verwacht")    
#   
#   # make warning if grootheid is not in list of waterhoogteparameters.  
#   
#   md <- rwsapi::rws_metadata()
#   thisCatalogue <- md$content$AquoMetadataLijst %>% 
#     unnest(
#       names_sep = ".", 
#       c(Compartiment, Eenheid, Grootheid, Hoedanigheid, Parameter)) %>% 
#     filter(Grootheid.Omschrijving == grootheid) %>%
#     left_join(
#       md$content$AquoMetadataLocatieLijst, 
#       by = c(AquoMetadata_MessageID = "AquoMetaData_MessageID")
#     ) %>%
#     left_join(md$content$LocatieLijst) %>% 
#     filter(Code %in% station) %>%
#     rename_with(tolower) %>%
#     rename(
#       locatie.naam = naam,
#       locatie.code = code
#     )
#   
#   for(iyear in startyear:endyear){
#     rwsapi::getDDLdata( 
#       startyear = iyear,
#       endyear = iyear,
#       myCatalogue = thisCatalogue,
#       outDir = outDir
#     )
#   }
# }
# 
# readDDLwaterhoogte2 <- function(ddlmetadata, startyear, endyear, outDir = "data/rijkswaterstaat/ddl/raw"){
#   
#   require(rwsapi)
#   require(tidyverse)
#   
#   waterhoogteparameters <- c("Waterhoogte berekend", "Waterhoogte", "Waterhoogte astronomisch", "Waterhoogte verwacht")    
#   
#   # make warning if grootheid is not in list of waterhoogteparameters.  
#   
#   for(iyear in startyear:endyear){
#     rwsapi::getDDLdata( 
#       startyear = iyear,
#       endyear = iyear,
#       myCatalogue = ddlmetadata,
#       outDir = outDir
#     )
#   }
# }

readYrAvgWaterhoogteDDL <- function(dir = "data\\rijkswaterstaat\\ddl\\annual_means"){
  files = list.files(dir)  
  lapply(files, \(x) read_delim(file.path(dir, x), delim = ";")) %>%
    bind_rows()
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

addBreakPoints2 = function(df, broken_line_breakpoint = 1993, broken_squared_breakpoint = 1960){
  
  stopifnot(
    any(names(df)=="year"),
    is.data.frame(df), 
    broken_line_breakpoint    >  1900, 
    broken_squared_breakpoint >  1900,
    broken_line_breakpoint     < 2000, 
    broken_squared_breakpoint  < 2000
  )
  
  blb_name = paste0("from", broken_line_breakpoint)
  bsb_name = paste0("from", broken_squared_breakpoint, "_square")
  
  df %>%
    dplyr::mutate(!!sym(blb_name) := (year >= broken_line_breakpoint) * (year - broken_line_breakpoint)) %>%
    dplyr::mutate(!!sym(bsb_name) := (year >= broken_squared_breakpoint) * (year - broken_squared_breakpoint) * (year - broken_squared_breakpoint))

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


addHACterms <- function(models) {
  
  modelstemp <- models %>%
    mutate(
      tidy.HAC = map(
        model, 
        function(x) broom::tidy(
          sqrt(
            diag(
              NeweyWest(
                x, 
                lag = 1, 
                prewhite = F, 
                adjust = T
              )
            )
          )
        )
      )
    )
  
  modelstemp$tidy.HAC <- lapply(modelstemp$tidy.HAC,
                                function(x) {
                                  x %>%
                                    rename(
                                      term.HAC = names,
                                      st.err.HAC = x
                                    )
                                }
  )
  
  return(modelstemp)
}

makePrettyAnovaTable <- function(output, digits) {
  rm.cols <- NULL
  for (i in 1:(dim(output)[2])) {
    if (all(is.na(output[,i]))) { rm.cols <- c(rm.cols,i) }
  }
  if (!is.null(rm.cols)) { output <- output[,-rm.cols] }
  # Reformat column names
  names(output) <- sub("Rsq","R^2^",names(output))
  names(output) <- sub("Pr\\(>F\\)","p",names(output))
  names(output) <- sub("^P$","p",names(output))
  # Rounding
  output <- apply(output, 2, signif, digits = digits)
  # Generate the kable
  options(knitr.kable.NA = '')
  knitr::kable(output)
}

makePredictionTable <- function(models, lookup = lookup) {
  
  all_predictions <- models %>%
    mutate(
      preds = map2(data, model, add_predictions)
    ) %>%
    dplyr::select(
      station,
      modeltype, 
      data, 
      tidy, 
      preds) %>%
    tidyr::unnest(c(data, preds), names_sep = "_") %>% 
    tidyr::unnest(tidy) %>%
    # str(max.level = 2)
    
    dplyr::select(-std.error, -statistic, -p.value) %>% # clean up
    tidyr::pivot_wider(
      names_from = term, 
      values_from = estimate
    ) %>%
    mutate(`data_height-surge_anomaly` = data_height - `preds_surge_anomaly`) %>%
    mutate(`preds_height-surge_anomaly` = preds_pred - `preds_surge_anomaly`) %>%
    rename(any_of(lookup)) %>%
    # str(max.level = 2)
    mutate(
      nodal_tide = 
        u_nodal * cos(2*pi*(data_year-epoch)/18.613) + 
        v_nodal * sin(2*pi*(data_year-epoch)/18.613),
      prediction_recalc = case_when(
        if("linear" %in% params$modeltype){
          modeltype == "linear" ~ 
            Constant + 
            Trend * (data_year - epoch)
        },
        if("broken_linear" %in% params$modeltype){
          modeltype == "broken_linear" ~ 
            Constant + 
            Trend * (data_year - epoch) +
            (data_year >= 1993) * `+ trend 1993` * (data_year - 1993)
        },
        if("broken_squared" %in% params$modeltype){
          modeltype == "broken_squared" ~ Constant + 
            Trend * (data_year - epoch) +
            (data_year >= 1960) * `+ square_trend 1960` * (data_year - 1960) * (data_year - 1960)
        }
      ),
      pre_accelleration = case_when(
        if("linear" %in% params$modeltype){
          modeltype == "linear" ~ 
            Constant + 
            Trend * (data_year - epoch)
        },
        if("broken_linear" %in% params$modeltype){
          modeltype == "broken_linear" ~ 
            Constant + 
            Trend * (data_year - epoch)
        },
        if("broken_squared" %in% params$modeltype){
          modeltype == "broken_squared" ~ Constant + 
            Trend * (data_year - epoch)
        }
      ),
      post_accelleration = case_when(
        if("linear" %in% params$modeltype){
          modeltype == "linear" ~ 
            Constant + 
            Trend * (data_year - epoch)
        },
        if("broken_linear" %in% params$modeltype){
          modeltype == "broken_linear" ~ 
            Constant + 
            Trend * (data_year - epoch) +
            `+ trend 1993` * (data_year - 1993)
        },
        if("broken_squared" %in% params$modeltype){
          modeltype == "broken_squared" ~ Constant + 
            Trend * (data_year - epoch) +
            `+ square_trend 1960` * (data_year - 1960) * (data_year - 1960)
        }
      )
    ) %>%
    select(
      station,
      modeltype,
      data_year,
      data_height,
      preds_year,
      prediction_recalc,
      `data_height-surge_anomaly`,
      `preds_height-surge_anomaly`,
      nodal_tide,
      pre_accelleration,
      post_accelleration
    )
  
  return(all_predictions)
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
    purrr::map_df(~ unlist(.[1:16]))
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

# read_gtsm_nc <- function(nc = "c:\\Temp\\era5_reanalysis_surge_2023_v1_monthly_mean.nc", stations_selected){
#   require(RNetCDF)
#   ncf <- RNetCDF::open.nc(nc)
#   
#   data <- RNetCDF::read.nc(ncf)
#   
#   stations <- tibble::tibble(
#     gtsmid = data$stations, 
#     stationname = data$station_name,
#     station_x_coordinate = data$station_x_coordinate,
#     station_y_coordinate = data$station_y_coordinate
#   )
#   df <- reshape2::melt(data$surge, value.name = "surge_m") %>%
#     dplyr::mutate(
#       gtsmid = data$stations[Var2],
#       month      = data$month[Var1]
#     ) %>%
#     dplyr::left_join(stations) %>%
#     dplyr::select(-Var1, -Var2) %>%
#     dplyr::filter(gtsmid %in% stations_selected)
#   
#   return(df)
# }
# 
read_yearly_gtsm <- function(filename = "data/deltares/gtsm/gtsm_surge_annual_mean_main_stations_2024.csv") {

  gtsm_surge_annual_mean_main_stations_2024 <- read.csv(filename, comment = "#")

}

read_yearly_psmsl_csv  <- function(station_nr, mainstations.df = NULL, filepath){

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

  if(!is.null(mainstations.df)){
    mainstationInfo = mainstations.df |>
      select(psmsl_id, name, `nap-rlr`, gtsm_id)
  } else{
    mainStationInfo <- readMainStationInfo(filepath) |>
      select(psmsl_id, name, `nap-rlr`, gtsm_id)
  }

  rlr_df <- rlr_df %>% left_join(mainStationInfo, by = c(psmsl_id = "psmsl_id"))

  return(rlr_df)

}

# 
# 
# 
# # hieronder is nog niet helemaal af, zie yearly psmsl function
# read_monthly_psmsl_csv  <- function(station_nr){
#   
#   base_rlr_url = "https://psmsl.org/data/obtaining/rlr.monthly.data/"
#   base_rlr_ext = ".rlrdata"
#   
#   rlr_df <- lapply(station_nr,
#                    function(x) {
#                      rlr_df <- read_delim(
#                        file = paste0(base_rlr_url, x, base_rlr_ext), 
#                        col_names = c("decimal_year", "rlr_height_mm", "interpolated", "flag"),
#                        col_types = "niic",
#                        delim = ";",
#                        trim_ws = T, 
#                        locale = locale(decimal_mark = "."
#                        )
#                      ) |>
#                        mutate(psmsl_id = as.character(x))
#                    }
#   ) |>
#     bind_rows()
#   
#   
#   return(rlr_df)
#   
# }
# 


read_tidal_components_csv <- function(files = NA, filesdir = "https://watersysteemdata.deltares.nl/thredds/fileServer/watersysteemdata/Wadden/ddl/calculated/TA_filtersurge") {
  
  if(any(is.na(files))){
    filelist <- list.files(filesdir, pattern = "component", full.names = T)
    # get names of stations and year from filenames in filelistShort
    filelistShort <- list.files(filesdir, pattern = "component", full.names = F)
  } else{
    filelist <- file.path(files)
    filelistShort <- basename(files)
  }  
  
  df <- lapply(filelist, 
               function(x) {
                 read_csv(x, col_types = cols(), progress = FALSE) %>%
                   mutate(verticalreference = case_when(
                     grepl("MSL", x) ~ "MSL",
                     !grepl("MSL", x) ~ "NAP"
                   ))
               }
  )
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

# experimental 

broken_jerk_model <- function(df){
  
  if(use_gtsm()){
    lm(
      height ~ offset(surge_anomaly) + 
        I(year - epoch) + 
        from1960_square + 
        from1960_third +
        I(cos(2 * pi * (year - epoch)/(18.613))) + 
        I(sin(2 * pi * (year - epoch)/(18.613))),
      data = df
    )
    
  } else {
    lm(
      height ~ wind_anomaly +
        I(year - epoch) +
        from1960_square + 
        from1960_third +
        I(cos(2 * pi * (year - epoch)/(18.613))) +
        I(sin(2 * pi * (year - epoch)/(18.613))),
      data = df
    )
  }
}

