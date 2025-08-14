

## goal: update sea level monitor database with
# annual averages for 6 main stations
# GTSM corrections


### !!!!! nakijken of gemiddelden over nederland afgerond worden opgeslagen 


require(tidyverse)
require(lubridate)

configDir <- "analysis/sealevelmonitor"

config <- RcppTOML::parseToml(file.path(configDir, "_common/configuration.TOML"))
config_flat <- list_flatten(config)
source(file.path(configDir, "_common/functions.R"))
epoch = config$constants$epoch
mainstations_df <- readMainStationInfo(filepath = "")
mainstations_locs <- readMainStationLocations(path = "")

current_df <-   read_csv2("data/deltares/results/dutch-sea-level-monitor-export-stations-latest.csv")

rlr_df <- read_yearly_psmsl_csv(mainstations_df$psmsl_id, filepath = "")

rlr_df <- rlr_df %>% 
  mutate(
    source = "psmsl",
    ophaaldatum = today()
  )

# compare years.. 
range(current_df$year)
range(current_df[current_df$source == "psmsl",]$year)
range(rlr_df$year)

if(config_flat$runparameters_monitoryear - 1 == max(rlr_df$year)){
  cat("Mean annual sea level downloaded from PSMSL are availabale up to ", max(rlr_df$year), ", the time series is up to date. ")
} else {
  cat("Mean annual sea level downloaded from PSMSL are only available up to ", max(rlr_df$year), " and thus incomplete for the current analysis. In order to do a preliminary analysis, measurements from Rijkswatersataat Data Distribution Layer will be used for missing year(s). ")
}


if(max(rlr_df$year) == config$runparameters$monitoryear-1){
  print("PSMSL time series is up-to-date and is used for analysis")
} else {
  print("The PSMSL time series is not complete. An attempt is made to complete the data using RWS DDL.")
  
  # find missing years
  # look if ddl data are available
  # glue ddl data in dataframe and label them with source "rws_ddl"
  
  if(config$runparameters$monitoryear - max(rlr_df$year) > 2){
    print("There is more than 1 year of data missing. This is more than expected. Analysis abandonned.")
    stop()
  } else
    required_file <- file.path("data/rijkswaterstaat/ddl/annual_means/", paste0(config$runparameters$monitoryear-1, ".csv"))
  if(file.exists(required_file)){
    ddl_datayear <- read_csv2(required_file) %>%
      filter(
        station %in% mainstations_df$name
      )
  } else{
    cat("DDL data for year", params$monitoryear-1, "is not available. Retrieve DDL data first. ", sep = " ")
  }
}


# Get GTSM data from local file
gtsm <- read_yearly_gtsm(filename = "../../data/deltares/gtsm/gtsm_surge_annual_mean_main_stations.csv") |>
  mutate(year = year(ymd(t)))

# convert rlr tot nap2005
refreshed_df <- rlr_df |> 
  mutate(
    height = rlr_height_mm - as.numeric(`nap-rlr`),
  ) |> 
  rename(station = name)

# check if psmsl data need to be completed with ddl data
try(
  if(
    exists("ddl_datayear") & 
    !unique(ddl_datayear$year) %in% unique(refreshed_df$year)
  ){
    refreshed_df <- refreshed_df |> bind_rows(ddl_datayear)
  },
  silent = T
)

refreshed_df <- refreshed_df |>
  left_join(gtsm, by = c(station = "name", year = "year")) |>
  mutate(
    surge_anomaly = case_when(
      year >= 1950 ~ (1000 * surge - mean(1000 * surge, na.rm = T)), # meters to millimeters
      year < 1950 ~ 0
    )
  ) |>
  select(
    year,
    height,
    station,
    surge_anomaly
  ) %>%
  bind_rows(
    . |>
      group_by(year) |>
      summarise(
        height = mean(height, na.rm = T),
        surge_anomaly = mean(surge_anomaly, na.rm = T)
      ) |>
      mutate(
        station = "Netherlands"
      )
  ) %>%
  bind_rows(
    . |>
      filter(station %in% c("Vlissingen", "Hoek van Holland", "Den Helder", "Harlingen", "IJmuiden")) |>
      group_by(year) |>
      summarise(
        height = mean(height, na.rm = T),
        surge_anomaly = mean(surge_anomaly, na.rm = T)
      ) |>
      mutate(
        station = "Netherlands (without Delfzijl)"
      )
  ) |>
  addBreakPoints() %T>%
  # write_csv2("../../data/deltares/results/dutch-sea-level-monitor-export-stations-latest.csv") %>%
  filter(year >= params$startyear)

