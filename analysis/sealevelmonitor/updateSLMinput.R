

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

configDir <- "analysis/sealevelmonitor"

config <- RcppTOML::parseToml(file.path(configDir, "_common/configuration.TOML"))
config_flat <- list_flatten(config)
source(file.path(configDir, "_common/functions.R"))
epoch = config$constants$epoch
mainstations_df <- rsealevel::readMainStationInfo(filepath = "")
mainstations_locs <- rsealevel::readMainStationLocations(path = "")

current_df <-   read_delim("data/deltares/results/dutch-sea-level-monitor-export-stations-latest.csv", delim = ";")
# watch out, change to read_delim when possible

last_reported_year <- max(current_df$year)

#== annual averages for 6 main stations --> to /data/slm-input.csv ===================================

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

#== get GTSM yearly average surge corrections ============================================

# Get GTSM data from local file
gtsm_yr <- read_yearly_gtsm(filename = "data/deltares/gtsm/gtsm_surge_annual_mean_main_stations.csv") |>
  mutate(year = year(ymd(t)))

#== combine PSMSL and GTSM surge data ===========================

psmsl_gtsm_yr <- psmsl_yr |>
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
    surge
  ) %>%
  bind_rows(
    . |>
      group_by(year) |>
      summarise(
        height = mean(height, na.rm = T),
        surge_anomaly = mean(surge_anomaly, na.rm = T)
      ) |>
      mutate(
        name = "Netherlands"
      )
  ) %>%
  bind_rows(
    . |>
      filter(name %in% c("Vlissingen", "Hoek van Holland", "Den Helder", "Harlingen", "IJmuiden")) |>
      group_by(year) |>
      summarise(
        height = mean(height, na.rm = T),
        surge_anomaly = mean(surge_anomaly, na.rm = T)
      ) |>
      mutate(
        name = "Netherlands (without Delfzijl)"
      )
  ) %>%
  mutate(station = name)  # for backwards compatibility
#|>
  # addBreakPoints() 
  # 
  # add metadata like e.g. 
  # writeLines("##gff-version 3", "output.gff")
# write_tsv(your_data, "output.gff", append = TRUE, col_names = TRUE)
  # 
  write_delim(psmsl_gtsm_yr, "data/deltares/input/psmsl_gtsm_yr-latest.csv", delim = ";")
  write_delim(psmsl_gtsm_yr, paste0("data/deltares/input/psmsl_gtsm_yr-", today(), ".csv"), delim = ";")

