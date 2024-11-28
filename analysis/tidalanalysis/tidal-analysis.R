
require(tidyverse)
require(stringr)
source("analysis/sealevelmonitor/_common/functions.R")

mainstations_df <- readMainStationInfo()

#=== read sea level data - takes time ======================================

df_sealevel <- read_delim("data/deltares/results/dutch-sea-level-monitor-export-stations-2024-09-30_temp.csv", delim = ",")

#=== read tidal components data =================================================

dir = "p:/11202493--systeemrap-grevelingen/1_data/noordzee/ddl/calculated/TA_filtersurge"
componentfiles <- list.files(dir, pattern = "component", full.names = T)
df_tidal <- read_tidal_components_csv(dir)

#==== Plot time course of components ============================================

df_tidal %>%
  unnest(data) %>%
  filter(comp %in% c("M2", "M4")) %>%
  # filter(!station %in% c("UITHZWD1", "NES")) %>%
  # filter(station %in% c("DELFZL", "DENHDR", "HARLGN", "WESTTSLG", "HUIBGT", "LAUWOG")) %>%
  filter(!station %in% c("K14PFM", "L9PFM")) %>%
  pivot_longer(
    cols = c(A, phi_deg), 
    names_to = "variable", 
    values_to = "value"
  ) %>%
  select(-name) %>%
  pivot_wider(
    id_cols = c(station, jaar, variable), 
    names_from = comp, 
    values_from = value
  ) %>%
  mutate(
    M4_M2 = case_when(
    variable == "A" ~ M4 / M2,
    variable == "phi_deg" ~ M4 - M2
    )
  ) %>%
  filter(variable == "A") %>%
  mutate(wanneer = ifelse(jaar < 1993, "voor", "na")) %>%
  ggplot(aes(x = jaar, y = M4_M2)) +
  geom_line(aes(color = wanneer), size = 1) +
  # geom_boxplot(aes(group = wanneer), fill = "transparent") +
  # geom_vline(xintercept = 1993) +
  coord_cartesian(ylim = c(NA,NA)) +
  facet_wrap(variable ~ station, scales = "free_y")

dir.create("results")
dir.create("results/tidal_analysis")
ggsave("results/tidal_analysis/M4_M2_wadden.png", height = 5, width = 10)


# genormaliseerde componenten in de tijd

p <- df_tidal %>%
  unnest(data) %>%
  filter(comp %in% c("M2", "M4")) %>%
  filter(station %in% c("DELFZL", "DENHDR", "EEMSHVN", "HARLGN", "HOLWD", "HUIBGT")) %>% 
  group_by(station, comp) %>%
  mutate(Anorm = A/mean(A)) %>%
  ungroup() %>%
  mutate(wanneer = ifelse(jaar < 1993, "voor", "na")) %>%
  ggplot(aes(x = jaar, y = Anorm)) +
  geom_line(aes(color = station), size = 1) +
  # geom_boxplot(aes(group = wanneer), fill = "transparent") +
  # geom_vline(xintercept = 1993) +
  coord_cartesian(ylim = c(NA,NA)) +
  facet_grid(comp ~ ., scales = "free_y")
p
ggsave("results/tidal_analysis/normalized_A_wadden.png", height = 5, width = 10)
plotly::ggplotly(p)
colors = colorRamps::magenta2green(2)

# relation M4:M2 with sea level rise
df_tidal %>%
  unnest(data) %>%
  filter(comp %in% c("M2", "M4")) %>%
  filter(station %in% c("DELFZL", "DENHDR", "EEMSHVN", "HARLGN", "HOLWD", "HUIBGT")) %>% 
  pivot_longer(
    cols = c(A, phi_deg), 
    names_to = "variable", 
    values_to = "value"
  ) %>%
  select(-station) %>%
  drop_na(name) %>% 
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
  left_join( df_sealevel, by = c(name = "station", jaar = "year")) %>%
  # filter(variable != "A") %>%
  mutate(wanneer = ifelse(jaar < 1993, "voor", "na")) %>%
  ggplot(aes(x = M4_M2, y = height)) +
  geom_path(aes(color = wanneer), size = 0.5) +
  geom_smooth(method = "lm", color = colors[1], alpha = 0.2, size = 1) +
  # geom_boxplot(aes(group = wanneer), fill = "transparent") +
  # geom_vline(xintercept = 1993) +
  coord_cartesian(ylim = c(NA,NA)) +
  coord_flip() +
  facet_wrap(vars(variable, name), scales = "free", nrow = 2)
ggsave("results/tidal_analysis/tide_height_wadden.png", height = 5, width = 10)

