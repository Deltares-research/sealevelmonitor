

## goal: update sea level monitor database with

# annual averages for 6 main stations --> to /data/slm-input.csv
# year
# station
# stationID
# height_rlr
# height_nap2005

# monthly averages for 6 main stations 
# decimal year
# station
# stationID
# height_rlr
# height_nap2005

# GTSM corrections
# year
# station
# stationID
# surge

# jaargemidddelden uit 10-minutenreeksen uit Wadar?
# 

### !!!!! nakijken of gemiddelden over nederland afgerond worden opgeslagen 


require(tidyverse)
require(lubridate)
require(rsealevel)
select <- dplyr::select

configDir <- "analysis/sealevelmonitor"

config <- RcppTOML::parseToml(file.path(configDir, "_common/configuration.TOML"))
config_flat <- list_flatten(config)
source(file.path(configDir, "_common/functions.R"))
epoch = config$constants$epoch
mainstations_df <- rsealevel::readMainStationInfo(filepath = "")
mainstations_locs <- rsealevel::readMainStationLocations(path = "")

# current_df <-   read_delim("data/deltares/results/dutch-sea-level-monitor-export-stations-latest.csv", delim = ";")

monitoryear <- config$runparameters$monitoryear

#== annual averages for 6 main stations --> to /data/slm-input.csv ===================================


#== get psmsl data and join with station info
psmsl_yr <- rsealevel::read_yearly_psmsl_csv(mainstations_df$psmsl_id) %>%
  left_join(
    mainstations_df %>%
      select(
        psmsl_id,
        `nap-rlr`, 
        gtsm_id,
        name
      ), 
    by = c(psmsl_id = "psmsl_id")) %>% 
  mutate(
    source = "psmsl",
    fetch_date = today()
  ) |> 
  mutate(
    height = rlr_height_mm - as.numeric(`nap-rlr`),
  )

#== Get RWS ddl for last monitoryear - 1 for early (preliminary) value

ddl_year <- monitoryear - 1
ddl_annual_current_year <- read_delim(file.path("data/rijkswaterstaat/ddl/annual_means/", paste0(ddl_year, ".csv")), delim = ";")
# make same format as df
# joing with mainstations for ids
# join with psmsl_yr
# integrate in df, keep or make source column
# integrate in further workflow, keeping source column



#== get GTSM yearly average surge corrections ============================================

# Get GTSM data from local file
gtsm_yr <- read_yearly_gtsm(filename = "data/deltares/gtsm/gtsm_surge_annual_mean_main_stations.csv") |>
  mutate(year = year(ymd(t)))

## Complete sea level height with RWS DDL height for latest year
## Check years height and gtsm
## 
boolMissing <- function(psmsl, gtsm) {
  max(gtsm$year) != max(psmsl$year)
}

missingYears <- function(psmsl, gtsm) {
  gtsm %>% 
    anti_join(psmsl %>% filter(year>= 1950), by = c(year = "year")) %>%
    distinct(year) %>% unlist %>% unname
}

missingYear <- if(boolMissing(psmsl_yr, gtsm_yr)){
  missingYears(psmsl_yr, gtsm_yr)
}

## Fetch ddl$height for missing year

ddl_annual_dir <- "data\\rijkswaterstaat\\ddl\\annual_means"
ddl_ann_means <- read_delim(file.path(ddl_annual_dir, paste0(missingYear, ".csv"))) %>%
  mutate(
    fetch_date = today()
  ) %>%
  select(
    year,
    name = station,
    source, 
    fetch_date,
    height
  )

## Include in dataframe indicating source = "rws-ddl


#== combine PSMSL, DDL and GTSM surge data ===========================

psmsl_gtsm_yr <- psmsl_yr |>
  bind_rows(ddl_ann_means) |>
  left_join(gtsm_yr, by = c(name = "name", year = "year")) |>
  mutate(
    surge_anomaly = case_when(
      year >= 1950 ~ (1000 * surge - mean(1000 * surge, na.rm = T)), # meters to millimeters
      year < 1950 ~ 0
    )
  ) |>
  select(
    year,
    psmsl_id,
    name,
    height,
    surge_anomaly,
    surge,
    source
  ) %>%
  bind_rows(
    . |>
      group_by(year, source) |>
      summarise(
        height = mean(height, na.rm = T),
        surge = mean(surge, na.rm = T),
        surge_anomaly = mean(surge_anomaly, na.rm = T),
        source = unique(source)
      ) |>
      ungroup() %>%
      mutate(
        name = "Netherlands",
        method = "calculated"
      )
  ) %>%
  bind_rows(
    . |>
      filter(name %in% c("Vlissingen", "Hoek van Holland", "Den Helder", "Harlingen", "IJmuiden")) |>
      group_by(year, source) |>
      summarise(
        height = mean(height, na.rm = T),
        surge = mean(surge, na.rm = T),
        surge_anomaly = mean(surge_anomaly, na.rm = T),
        source = unique(source)
      ) |>
      ungroup() %>%
      mutate(
        name = "Netherlands (without Delfzijl)",
        method = "calculated"
      )
  ) %>%
  mutate(station = name)  # for backwards compatibility

  # add metadata like e.g. 
  # writeLines("##gff-version 3", "output.gff")
# write_delim(your_data, "output.gff", append = TRUE, delim = ";")
  # 
  write_delim(psmsl_gtsm_yr, "data/deltares/input/psmsl_gtsm_yr-latest.csv", delim = ";")
  write_delim(psmsl_gtsm_yr, paste0("data/deltares/input/psmsl_gtsm_yr-", today(), ".csv"), delim = ";")


  