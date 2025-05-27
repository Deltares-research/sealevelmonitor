
require(tidyverse)
require(stringr)
source("analysis/sealevelmonitor/_common/functions.R")

mainstations_df <- readMainStationInfo()
stationLocations <- sf::st_read("data/rijkswaterstaat/waterhoogtestations.geojson", crs= 25831)

#=== plot map ==============================================================

stationLocations %>%
  sf::st_transform(4326) %>%
leaflet::leaflet() %>%
  leaflet::addTiles() %>%
  leaflet::addCircleMarkers(
    radius = 2, 
    label = ~paste(locatie.code, locatie.naam), 
    labelOptions = leaflet::labelOptions(noHide = T)
  )

#=== read sea level data - takes time ======================================

df_sealevel <- read_delim("data/deltares/results/dutch-sea-level-monitor-export-stations-2024-09-30_temp.csv", delim = ";")

whdir <- ("data\\rijkswaterstaat\\ddl\\annual_means")
df_ddl_y_avg_height <- readYrAvgWaterhoogteDDL(whdir)
newStationList <- read_csv("data/rijkswaterstaat/newStationList.csv")
name_codes <- df_ddl_y_avg_height %>% distinct(station) %>% arrange(station) %>%
  left_join(newStationList %>% select(locatie.naam, locatie.code, gebied), 
            by = c(station = "locatie.naam"))
# df_ddl_y_avg_height %>% 
#   arrange(locatie.code, year) %>%
#   distinct(year, locatie.code, locatie.naam ) %>%
#   write_csv("data/rijkswaterstaat/station_year_list.csv")

#=== read tidal components data =================================================

dir = "p:/11202493--systeemrap-grevelingen/1_data/noordzee/ddl/calculated/TA_filtersurge"
componentfiles <- list.files(dir, pattern = "component", full.names = T)
componentfiles[grepl("LICHTEL", componentfiles)]
componentfiles <- componentfiles[!(grepl("LICHTEL", componentfiles) & grepl("MSL", componentfiles))]

df_tidal <- read_tidal_components_csv(componentfiles, dir)
df_tidal$name <- name_codes$station[match(df_tidal$station, name_codes$locatie.code)]

#==== Plot time course of components ============================================

Wadden_oost = c("LAUW", "LAUWOG", "SCHIERMNOG", "HUIBGT", "UHW2", "SPE", "DLFZ", "DELFZL", "TERM", "NIEUWSTZL")
Wadden_west = c("DENHDR", "DENH", "OUDSD", "OUDE", "DENOVBTN", "OEBU", "VLIELHVN", "VLIE", "WESTTSLG", "WTER", "KORNWDZBTN", "KOBU", 
                "HARLGN", "HARL", "NES", "HOLWD", "HOLW") 


# make stationlist sorted by geography
sortedStations <- df_ddl_y_avg_height %>% distinct(station) %>%
  left_join(name_codes, by = c(station = "station")) %>%
  left_join(
    read_csv2("data/rijkswaterstaat/waterhoogtestations.csv") %>%
      mutate(locatie.naam = case_when(
        locatie.naam == "IJmuiden buitenhaven" ~ "IJmuiden",
        locatie.naam != "IJmuiden buitenhaven" ~ locatie.naam
      )), 
    by = c(station = "locatie.naam")
  )  %>%
  filter(locatie.code.x == locatie.code.y) %>%
  select(-locatie.code.y) %>% rename(locatie.code = locatie.code.x) %>%
  arrange(-y-x) %>%
  select(station) %>% distinct() %>% unlist %>% unname

# relation M4:M2 with sea level rise
df_tidal_height <- df_tidal %>%
  mutate(station = replace(station, station == "IJMDBTHVN", "IJMDN")) %>% 
  unnest(data) %>%
  filter(comp %in% c("M2", "M4")) %>%
  # filter(station %in% c("DELFZL", "DENHDR", "EEMSHVN", "HARLGN", "HOEKVHLD", "IJMDBTHVN", "HOLWD", "HUIBGT")) %>%
  pivot_longer(
    cols = c(A, phi_deg), 
    names_to = "variable", 
    values_to = "value"
  ) %>%
  group_by(station, name, jaar, comp, variable) %>% # remove duplicates due to multiple vertical references (no effect on M2 and M4 values)
  summarize(value = mean(value), .groups = "drop") %>%
  pivot_wider(
    id_cols = c(name, jaar, variable), 
    names_from = comp, 
    values_from = value
  ) %>%
  mutate(
    M4_M2 = case_when(
      variable == "A" ~ M4 / M2,
      variable == "phi_deg" ~ M4 - M2
    )
  ) %>%
  left_join(
    df_ddl_y_avg_height,
    by = c(name = "station", jaar = "year")
  ) %>%
  drop_na(height) %>%
  mutate(wanneer = ifelse(jaar < 1993, "voor", "na")) %>%
  group_by(name, variable) %>%
  filter(n() > 10) %>% 
  ungroup() %>%
  mutate(name = factor(name, levels = sortedStations))


df_tidal_height %>%
  filter(variable == "A") %>%
  ggplot(aes(x = jaar, y = M4)) +
  geom_line(aes(color = wanneer), linewidth = 1) +
  # geom_boxplot(aes(group = wanneer), fill = "transparent") +
  geom_vline(xintercept = c(1993, 2007, 2016), 
             linewidth = 0.5) +
  coord_cartesian(xlim = c(1932, NA), ylim = c(NA,NA)) +
  facet_wrap("name", scales = "free_y", ncol = 3)

dir.create("results")
dir.create("results/tidal_analysis")
ggsave("results/tidal_analysis/M4_wadden.png", height = 10, width = 14)

df_tidal_height %>%
  filter(variable == "A") %>%
  filter(name %in% mainstations_df$name) %>%
  ggplot(aes(x = jaar, y = M2)) +
  geom_line(aes(color = wanneer), linewidth = 1) +
  # geom_boxplot(aes(group = wanneer), fill = "transparent") +
  geom_vline(xintercept = c(1993, 2007, 2016), 
             linewidth = 0.5) +
  coord_cartesian(xlim = c(1932, NA), ylim = c(NA,NA)) +
  facet_wrap("name", scales = "free_y", ncol = 3)

ggsave("results/tidal_analysis/M2_wadden.png", height = 10, width = 14)

df_tidal_height %>%
  filter(variable == "A") %>%
  mutate(wanneer = ifelse(jaar < 1993, "voor", "na")) %>%
  ggplot(aes(x = jaar, y = M4_M2)) +
  geom_line(aes(color = wanneer), linewidth = 1) +
  # geom_boxplot(aes(group = wanneer), fill = "transparent") +
  geom_vline(xintercept = c(1993, 2007, 2016), 
             linewidth = 0.5) +
  coord_cartesian(xlim = c(1932, NA), ylim = c(NA,NA)) +
  facet_wrap("name", scales = "free_y", ncol = 3)

ggsave("results/tidal_analysis/M4_M2_wadden.png", height = 10, width = 14)


# genormaliseerde componenten in de tijd - M2

p <- df_tidal_height %>%
  left_join(newStationList, by = c(name = "locatie.naam")) %>%
  mutate(
    gebied = case_when(
      locatie.code %in% Wadden_oost ~"Wadden_oost",
      locatie.code %in% Wadden_west ~ "Wadden_west",
      !locatie.code %in% c(Wadden_oost, Wadden_west) ~"buiten Wadden",
    )
  ) %>%
  mutate(gebied = factor(gebied, levels = c("buiten Wadden", "Wadden_west", "Wadden_oost"))) %>%
  filter(variable == "A") %>%
  select(-variable) %>%
  pivot_longer(
    cols = c(M2, M4, M4_M2), 
    names_to = "comp", 
    values_to = "A"
  ) %>%
  group_by(name, comp, gebied) %>%
  filter(
    jaar >= 1900,
    comp == "M2"
    ) %>%
  mutate(Anorm = A/(quantile(A, 0.9,  na.rm = T))) %>%
  ungroup() %>% 
  ggplot(aes(x = jaar, y = Anorm)) +
  geom_point(aes(color = locatie.code), size = 1, alpha = 0.5) +
  geom_line(aes(color = locatie.code), linewidth = 0.8, alpha = 0.6) +
  # geom_smooth(
  #   data = . %>% filter(jaar > 1993), 
  #   method = "lm", 
  #   alpha = 0.1,
  #   aes(color = locatie.code)) +
  # geom_boxplot(aes(group = wanneer), fill = "transparent") +
  geom_vline(xintercept = c(2010, 2016, 1993)) +
  coord_cartesian(xlim = c(1932, NA), ylim = c(0.85,1.1)) +
  facet_grid(comp ~ gebied, scales = "free_y") +
  xlab("year") + ylab("normalized amplitude in m") +
  ggplot2::theme(legend.position = "none")
p
ggsave("results/tidal_analysis/normalized_A_M2_wadden.png", height = 7, width = 10)
plotly::ggplotly(p)

p <- df_tidal_height %>%
  left_join(newStationList, by = c(name = "locatie.naam")) %>%
  mutate(
    gebied = case_when(
      locatie.code %in% Wadden_oost ~"Wadden_oost",
      locatie.code %in% Wadden_west ~ "Wadden_west",
      !locatie.code %in% c(Wadden_oost, Wadden_west) ~"outside Wadden",
    )
  ) %>%
  filter(variable == "A") %>%
  select(-variable) %>%
  pivot_longer(
    cols = c(M2, M4, M4_M2), 
    names_to = "comp", 
    values_to = "A"
  ) %>%
  group_by(name, comp, gebied) %>%
  filter(jaar >= 1980) %>%
  mutate(Anorm = A/mean(A)) %>%
  ungroup() %>% 
  ggplot(aes(x = jaar, y = Anorm)) +
  geom_point(aes(color = locatie.code), size = 1, alpha = 0.5) +
  geom_line(aes(color = locatie.code), linewidth = 0.8, alpha = 0.6) +
  # geom_smooth(
  #   data = . %>% filter(jaar > 1993), 
  #   method = "lm", 
  #   alpha = 0.1,
  #   aes(color = locatie.code)) +
  # geom_boxplot(aes(group = wanneer), fill = "transparent") +
  geom_vline(xintercept = c(2010, 2016, 1993)) +
  coord_cartesian(xlim = c(NA, NA), ylim = c(0.8,1.2)) +
  facet_grid(comp ~ gebied, scales = "free_y") +
  xlab("year") + ylab("normalized amplitude in m") +
  ggplot2::theme(legend.position = "none")
p
ggsave("results/tidal_analysis/normalized_A_wadden_1980-now.png", height = 7, width = 10)
plotly::ggplotly(p)


colors = colorRamps::magenta2green(2)

df_tidal_height %>%
  filter(variable == "A") %>%
  ggplot(aes(x = M4_M2, y = height)) +
  geom_point(aes(color = wanneer), size = 2, alpha = 0.5) +
  geom_path(aes(color = wanneer), size = 0.5) +
  geom_smooth(method = "lm", color = colors[1], alpha = 0.2, size = 1) +
  # geom_boxplot(aes(group = wanneer), fill = "transparent") +
  # geom_vline(xintercept = 1993) +
  coord_cartesian(ylim = c(NA,NA)) +
  coord_flip() +
  facet_wrap(vars(name), scales = "free", nrow = 4) +
  ggtitle("Amplitude M4/M2 vs mean sea level per year")
ggsave("results/tidal_analysis/tide_amplitude_height_wadden.png", height = 8, width = 12)

# relation M4:M2 with sea level rise
df_tidal_height %>%
  filter(variable != "A") %>%
  ggplot(aes(x = M4_M2, y = height)) +
  geom_path(aes(color = wanneer), size = 1) +
  geom_smooth(method = "lm", color = colors[1], alpha = 0.2, size = 1) +
  # geom_boxplot(aes(group = wanneer), fill = "transparent") +
  # geom_vline(xintercept = 1993) +
  coord_cartesian(ylim = c(NA,NA)) +
  coord_flip() +
  facet_wrap(vars(name), scales = "free", nrow = 4) +
  ggtitle("Phase difference M4 - M2 vs mean sea level per year")
ggsave("results/tidal_analysis/tide_phase_height_wadden.png", height = 8, width = 12)


df_tidal_height %>%
  filter(jaar >= 1980) %>% 
  filter(variable == "A") %>%
  ggplot(aes(x = M4_M2, y = height)) +
  geom_point(aes(color = wanneer), size = 2, alpha = 0.5) +
  geom_path(aes(color = wanneer), size = 0.5) +
  geom_smooth(method = "lm", color = colors[1], alpha = 0.2, size = 1) +
  # geom_boxplot(aes(group = wanneer), fill = "transparent") +
  # geom_vline(xintercept = 1993) +
  facet_wrap(vars(name), scales = "free", nrow = 4) +
  coord_cartesian(ylim = c(NA,NA)) +
  coord_flip() +
  ggtitle("Amplitude M4/M2 vs mean sea level per year (>1980)")
ggsave("results/tidal_analysis/tide_amplitude_height_wadden_1980_now.png", height = 8, width = 12)

# relation M4:M2 with sea level rise
df_tidal_height %>%
  filter(jaar >= 1980) %>% 
  filter(variable != "A") %>%
  ggplot(aes(x = M4_M2, y = height)) +
  geom_path(aes(color = wanneer), size = 1) +
  geom_smooth(method = "lm", color = colors[1], alpha = 0.2, size = 1) +
  # geom_boxplot(aes(group = wanneer), fill = "transparent") +
  # geom_vline(xintercept = 1993) +
  coord_cartesian(ylim = c(NA,NA)) +
  coord_flip() +
  facet_wrap(vars(name), scales = "free", nrow = 4) +
  ggtitle("Phase difference M4 - M2 vs mean sea level per year (>1980)")
ggsave("results/tidal_analysis/tide_phase_height_wadden_1980_now.png", height = 8, width = 12)


df_tidal_height %>%
  filter(variable == "A") %>%
  filter(!grepl("platform", name)) %>%
  ggplot(aes(x = M2, y = height)) +
  geom_point(aes(color = jaar), size = 2, alpha = 0.5) +
  # geom_path(aes(color = wanneer), size = 0.5) +
  # geom_smooth(
  #   aes(color = wanneer),
  #   method = "lm", 
  #   # color = colors[1], 
  #   alpha = 0.2, 
  #   size = 1
  #   ) +
  # geom_boxplot(aes(group = wanneer), fill = "transparent") +
  # geom_vline(xintercept = 1993) +
  coord_cartesian(ylim = c(NA,NA)) +
  coord_flip() +
  facet_wrap(vars(name), scales = "fixed", nrow = 3) +
  scale_color_viridis_c(option = "turbo") +
  ggtitle("Amplitude M2 vs mean sea level per year") +
  theme_dark()
ggsave("results/tidal_analysis/M2_amplitude_height_wadden.png", height = 7, width = 14)

#==== sea level for all stations ===========================================================

# lm(
#   height ~ offset(surge_anomaly) + 
#     I(year - epoch) + 
#     from1993 + 
#     I(cos(2 * pi * (year - epoch)/(18.613))) + 
#     I(sin(2 * pi * (year - epoch)/(18.613))),
#   data = df
# )

p <- df_ddl_y_avg_height %>%
  left_join(newStationList, by = c(station = "locatie.naam")) %>%
  filter(
    year >= 1993,
    # groepering.code == "NVT",
    height < 500,
    height > -500
  ) %>% 
  group_by(station) %>%
  filter(n() > 20) %>% ungroup() %>%
  mutate(
    gebied = case_when(
      locatie.code %in% Wadden_oost ~"Wadden_oost",
      locatie.code %in% Wadden_west ~ "Wadden_west",
      !locatie.code %in% c(Wadden_oost, Wadden_west) ~"outside Wadden",
    )
  ) %>%
  mutate(station = factor(station, levels = sortedStations)) %>%
  ggplot(aes(year, height)) +
  geom_point(aes(color = gebied)) +
  # geom_line(aes(color = name)) +
  geom_smooth(
    method = "lm", 
    formula = y ~ I(x - 1970) + I(x > 1993 * x *x),
    aes(color = gebied), 
    alpha = 0) +
  geom_vline(xintercept = 1993, linewidth = 0.5) +
  # facet_wrap("locatie.naam", nrow = 3) +
ggtitle("mean sea level per year")
  
p

plotly::ggplotly(p)

