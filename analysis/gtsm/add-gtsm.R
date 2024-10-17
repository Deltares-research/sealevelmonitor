require(jsonlite)
require(ncdf4)
require(tidyverse)
require(scales)

replacement_year = 2023

gtsm_dir <- "data/deltares/gtsm"

#== read station information =================================

mainstations <- jsonlite::read_json("data\\deltares\\main_stations.json")
mainstations_df <- map_df(mainstations, ~ unlist(.[1:15])) #%>%
  # mutate(gtsm_id_2023 = as.numeric(gtsm_id_2023))

source("analysis/sealevelmonitor/_common/functions.R")

selectedstations = mainstations_df$gtsm_id_2023

#===== read test NetCDF for certain year =============================

gtsm_replacementyear <- read_gtsm_nc(
  nc = file.path(
    "data", 
    "deltares", 
    "gtsm", 
    "gtsm_nc", 
    paste0("era5_reanalysis_surge_", replacement_year, "_v1_monthly_mean.nc")
  ), 
  stations_selected = selectedstations
  ) %>%
  mutate(
    gtsmid = as.character(gtsmid),
    ) %>%
  arrange(gtsmid, month) %>%
  left_join(mainstations_df, by = c(gtsmid = "gtsm_id_2023"))

gtsm_replacementyear %>% distinct(id, gtsm_id, gtsmid, stationname, name)
range(gtsm_replacementyear$month)
range(gtsm_replacementyear$surge_m)

#========== visual checks ===============================

ggplot(gtsm_replacementyear, aes(month, surge_m)) +
  geom_path(aes(color = name, group = name), size = 1, alpha = 1) +
  theme_minimal()
  
require(leaflet)
leaflet(gtsm_replacementyear) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~station_x_coordinate, 
    lat = ~station_y_coordinate, 
    label = ~name, 
    labelOptions = labelOptions(noHide = TRUE)
  )

#======== calculate yearly averages (weigthed) ===============================

gtsm_replacementyear_monthly_add <- gtsm_replacementyear %>%
  mutate(
    t = as.Date(paste(replacement_year, str_pad(as.character(month), 2, "left", "0"), "01", sep = "-")),
    psmsl_id = as.numeric(psmsl_id)
  ) %>% 
  select(
    t,
    name,
    psmsl_id,
    ddl_id,
    latitude = station_y_coordinate,
    longitude = station_x_coordinate,
    station_name = stationname,
    surge = surge_m
  )  %>%
  bind_rows(
    group_by(., t) %>%
      summarise(
        surge = mean(surge),
        latitude = 52.1551995,
        longitude = 5.3850577,
        station_name = "Dutch mean"
      ) %>%
      mutate(
        name = "NL",
        ddl_id = "NL"
      )
  )


gtsm_replacementyear_annual_add <- gtsm_replacementyear %>% 
  mutate(
    date = as.Date(paste(replacement_year, str_pad(as.character(month), 2, "left", "0"), "01", sep = "-")),
    year = year(date),
    daysInMonth = lubridate::days_in_month(date)
    ) %>% 
  group_by(name, ddl_id, gtsmid, year) %>%
  summarise(
    yearly_weighted_mean_surge_m = weighted.mean(surge_m, daysInMonth)
  ) %>%
  ungroup() %>%
  mutate(
    t = as.Date(paste(year, "01", "01", sep = "-"))
  ) %>%
  select(
    t,
    name,
    ddl_id,
    surge = yearly_weighted_mean_surge_m
  ) %>% 
  bind_rows(
      group_by(., t) %>%
      summarise(
        surge = mean(surge)
        ) %>%
      mutate(
        name = "NL",
        ddl_id = "NL"
      )
  )

#==== add to existing gtsm file =============================================

list.files(gtsm_dir)
gtsm_annual <- read_yearly_gtsm("data/deltares/gtsm/gtsm_surge_annual_mean_main_stations.csv") %>%
  mutate(t = as.Date(t))
gtsm_monthly <- read_yearly_gtsm("data/deltares/gtsm/gtsm_surge_monthly_mean_main_stations.csv") %>%
  mutate(t = as.Date(t))

#333 check replacement values ==============================================

gtsm_annual %>%
  ggplot(aes(t, surge, color = name)) +
  geom_line(aes()) +
  geom_point(data = gtsm_replacementyear_annual_add, size = 2) +
  coord_cartesian(xlim = c(min(unique(gtsm_replacementyear_annual_add$t)), max(unique(gtsm_replacementyear_annual_add$t))))

gtsm_monthly %>%
  ggplot(aes(t, surge, color = name)) +
  geom_line(aes()) +
  geom_point(data = gtsm_replacementyear_monthly_add, size = 2) +
  coord_cartesian(xlim = c(min(unique(gtsm_replacementyear_monthly_add$t)), max(unique(gtsm_replacementyear_monthly_add$t))))

#=== add new values to existing table ======================================

gtsm_allyears_annual <- gtsm_annual %>%
  filter(!t %in% unique(gtsm_replacementyear_annual_add$t)) %>%
  bind_rows(gtsm_replacementyear_annual_add) %>%
  select(
    t,
    name,
    ddl_id,
    surge
  ) %>%
  arrange(name, t)

gtsm_allyears_monthly <- gtsm_monthly %>%
  filter(!t %in% unique(gtsm_replacementyear_annual_add$t)) %>%
  bind_rows(gtsm_replacementyear_monthly_add) %>%
  select(
    t,
    name,
    ddl_id,
    surge
  ) %>%
  arrange(name, t)

#==== check =================================================================

gtsm_allyears_annual %>%
  filter(name !="NL") %>%
  ggplot(aes(t, surge)) +
  geom_point(aes(color = name), linewidth = 1) +
  scale_x_date(breaks = scales::breaks_pretty(10), date_labels = "%Y") +
  geom_smooth(span = 0.3) +
  scale_x_date(breaks = scales::breaks_pretty(20), minor_breaks = "1 year", date_labels = "%Y") +
  facet_wrap("name")


save_file_name_annual <- "data/deltares/gtsm/gtsm_surge_annual_mean_main_stations.csv"
metadata_gtsm_replacementyear_annual <- c("# generated with r-analyse/gtsm/add-gtsm.R", 
                                "# contains gtsm surge output per year")
# not implemented, but metadata can be added above the csv table
write_lines(
  metadata_gtsm_replacementyear_annual, 
  save_file_name_annual, 
)
write.table(
  gtsm_allyears_annual,
  save_file_name_annual, 
  append = T,
  row.names = FALSE,
  sep = ","
)


gtsm_allyears_monthly %>%
  filter(name !="NL") %>%
  ggplot(aes(t, surge)) +
  geom_line(aes(color = name), linewidth = 1) +
  geom_smooth(span = 0.1) +
  scale_x_date(breaks = scales::breaks_pretty(20), minor_breaks = "1 year", date_labels = "%Y") +
  facet_wrap("name")

save_file_name_monthly <- "data/deltares/gtsm/gtsm_surge_monthly_mean_main_stations.csv"
metadata_gtsm_replacementyear_monthly <- c("# generated with r-analyse/gtsm/add-gtsm.R", 
                               "# contains gtsm surge output per month")
# not implemented, but metadata can be added above the csv table
write_lines(
  metadata_gtsm_replacementyear_monthly, 
  save_file_name_monthly, 
)
write.table(
  gtsm_allyears_monthly,
  save_file_name_monthly, 
  append = T,
  sep = ",", 
  row.names = F
)






