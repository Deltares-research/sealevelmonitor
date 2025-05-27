
## Get DDL sea level data for main stations. 
## Calculate yearly means per year for all stations and write to file
## 
## to do: connect station code to station name (ijmuiden is not correct) and gtsm_id

source("analysis/sealevelmonitor/_common/functions.R")

## make list of stationcodes

stationlist <- read_csv("data/rijkswaterstaat/stationlist.csv")
mijnmetadata <- get_selected_metadata(compartiment = "OW", grootheid = "WATHTE", locatie = stationlist)

datayear = 2024

ddlrawdir <- "P:/11202493--systeemrap-grevelingen/1_data/Noordzee/ddl/raw/wathte"
ddlmeandir <- "data/rijkswaterstaat/ddl/annual_means"
mainstations_df <- readMainStationInfo(filepath = "")
mainstationnames <- mainstations_df$name
correcties <- read_csv("data/Deltares/coastalstationscorrections2005.csv")
codes <- read_csv("data/rijkswaterstaat/station_year_list.csv") |>
  distinct(locatie.naam, locatie.code) %>% 
  left_join(
    read_csv("data/rijkswaterstaat/station_year_list.csv") %>%
      filter(year < 2023) %>%
      distinct(locatie.naam, locatie.code) %>%
      rename(mwtl_code = locatie.code),
    by = c(locatie.naam = "locatie.naam")
  ) %>%
  drop_na(mwtl_code) %>%
  arrange(locatie.naam) 


# readDDLwaterhoogte(station = mainstationcodes, startyear = min(datayear), endyear = max(datayear), outDir = ddlrawdir)

# calculate annual means

waterhoogtes <- list()

for(myyear in datayear){
  
  # myyear = datayear[length(datayear)]
  
  files <- list.files(ddlrawdir, pattern = as.character(myyear))
  
  waterhoogtes_myyear <- lapply(
    files, 
    function(x){
      read_delim(
        paste0(ddlrawdir, "/", x),
        delim = ";", 
        escape_double = FALSE, 
        locale = locale(), 
        col_types = cols(),
        trim_ws = TRUE, 
        na = c("-999999999", "999999999", "")
      ) %>% 
        select(
          kwaliteitswaarde.code, 
          locatie.naam,
          locatie.code,
          meetapparaat.omschrijving,
          grootheid.omschrijving,
          groepering.code,
          eenheid.code,
          hoedanigheid.code,
          numeriekewaarde
        ) %>% 
        mutate(
          kwaliteitswaarde.code = as.character(kwaliteitswaarde.code)
        ) %>%
        mutate(
          locatie.naam = case_when(
            locatie.naam == "IJmuiden buitenhaven" ~ "IJmuiden",
            locatie.naam != "IJmuiden buitenhaven" ~ locatie.naam
          )
        ) %>%
        filter(
          as.numeric(kwaliteitswaarde.code) < 50,
          groepering.code == "NVT"
        )
    }
  ) %>%
    list_rbind() %>%
    group_by(
      locatie.naam,
      # locatie.code,
          # meetapparaat.omschrijving,
          grootheid.omschrijving,
          eenheid.code,
          # groepering.code,
          hoedanigheid.code
        ) %>%
        summarise(
          annual_mean_mm = mean(numeriekewaarde, na.rm = T) %/% 0.1,
          n = n(),
          .groups = "drop"
        ) %>%
        mutate(
          year = myyear,
          source = "rws_ddl"
        ) %>%
    left_join(
      correcties %>% 
        select(Station, `verschil [mm]`),
      by = c(locatie.naam = "Station")
    ) %>%
    mutate(
      annual_mean_mm_corrected = case_when(
        year >= 2005 ~ annual_mean_mm,
        year < 2005 & !is.na(`verschil [mm]`) ~ annual_mean_mm + `verschil [mm]`,
        year < 2005 & is.na(`verschil [mm]`) ~ NA_real_
      )
    )
  
  if(nrow(waterhoogtes_myyear) > 0){
    write_delim(
      waterhoogtes_myyear, delim = ";",
      file = file.path(paste(ddlmeandir, "all", sep = "_"), paste0(myyear, ".csv"))
    )
  }
  
  if(nrow(waterhoogtes_myyear) > 0){
    waterhoogtes_myyear %>%
      filter() %>% # filter out unwanted combinations of hoedanigheid, groepering
      select(
        year,
        height = annual_mean_mm_corrected,
        station = locatie.naam,
        n_per_year = n,
        verticalreference = hoedanigheid.code,
        source
      ) %>%
      write_delim(
        delim = ";",
        file = file.path(ddlmeandir, paste0(myyear, ".csv"))
      )
  }
  
}



# checks on raw data, should move to retrieval script

waterhoogtes_myyear %>%
  filter() %>% # filter out unwanted combinations of hoedanigheid, groepering
  select(
    year,
    height = annual_mean_mm_corrected,
    station = locatie.naam,
    n_per_year = n,
    source
  )
  


# 25 means "In ruimte en tijd geïnterpoleerde waardeIn ruimte en tijd geïnterpoleerde waarde"
# 99 means "Hiaat waarde"

# Drop 99, keep 25

waterhoogtes_myyear %>%
  filter(kwaliteitswaarde.code > 50) %>%
  count(locatie.code) %>%
  ggplot(aes(locatie.code, n)) +
  geom_col() +
  coord_flip() +
  ggtitle("aantal hiaatwaarden per station")


waterhoogtes_myyear %>%
  filter(kwaliteitswaarde.code < 50) %>%
  count(locatie.code) %>%
  ggplot(aes(locatie.code, n)) +
  geom_col() +
  coord_flip() +
  ggtitle("aantal valide waarden per station")


waterhoogtes_myyear %>%
  sample_n(100000) %>%
  filter(kwaliteitswaarde.code < 50) %>%
  ggplot(aes(tijdstip, numeriekewaarde)) +
  geom_line(alpha = 0.1) +
  # geom_smooth() +
  scale_x_datetime(date_breaks = "2 month", date_labels = "%h") +
  facet_wrap("locatie.code")

# Europlatform heeft dubbel aantal metingen, zowel t.o.v. MSL als NAP
waterhoogtes_myyear %>%
  filter(locatie.code == "EPL2") %>% 
  count(hoedanigheid.code)

annual_means %>% distinct(station) %>% View

# IJmuiden en Harlingen missen van de hoofdstations
# Gek. IJmuiden staat wel (2x) in stationlist. Harlingen nu toegevoegd. 

