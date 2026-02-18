

# path <- "p:\\11202493--systeemrap-grevelingen\\1_data\\Noordzee\\ddl\\netcdf\\wathte_2020_2025"
# files <- list.files(path, pattern = ".nc")
# path = "https://watersysteemdata.deltares.nl/thredds/dodsC/watersysteemdata/Noordzee/ddl/netcdf/wathte_2020_2025/"

library(tidync)
library(tidyverse)

path <- "data/rijkswaterstaat/ddl/netcdf/wathte_2020_2025"
files <- list.files(path, pattern = ".nc")
df <- lapply(files, \(x) {
  print(x)
  station = str_replace(x, "_meas_wl.nc", "")
  nc <- tidync::tidync(x = file.path(path, x))
  df_yr <- hyper_tibble(nc) %>%
    mutate(station = station) %>%
    mutate(
      year = year(time)
    ) %>%
    group_by(station, year) %>%
    summarize(
      n_NA = length(which(is.na(Meetwaarde.Waarde_Numeriek))),
      height = mean(Meetwaarde.Waarde_Numeriek, na.rm = T),
      n = n()
    )
}) %>%
  bind_rows() %>% 
  filter(year > 2019) 

df%>%
  ggplot(aes(year, height)) +
  geom_point() +
  geom_line() +
  facet_wrap("station")

currentyear = 2025

df %>%
  filter(year == currentyear) %>%
  mutate(
    verticalreference = "NAP", 
    source = "rws_ddl_wadar"
  ) %>%
  mutate(
    station = recode(
      station,
      "delfzijl" = "Delfzijl",
      "denhelder.marsdiep" = "Den Helder",
      "harlingen.waddenzee" = "Harlingen",
      "hoekvanholland" = "Hoek van Holland",
      "ijmuiden.buitenhaven"  = "IJmuiden",
      "vlissingen"  = "Vlissingen",
    )
  ) %>%
  write_delim(file.path("data/rijkswaterstaat/ddl/annual_means/", paste0(currentyear, ".csv")))
