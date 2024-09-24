
## Get DDL sea level data for main stations. 
## 
## 
## to do: connect station code to station name (ijmuiden is not correct) and gtsm_id

source("analysis/sealevelmonitor/_common/functions.R")

datayear = 1900:2023

ddlrawdir <- "data/rijkswaterstaat/ddl/raw"
ddlmeandir <- "data/rijkswaterstaat/ddl/annual_means"
mainstations_df <- readMainStationInfo(filepath = "")
mainstationcodes <- mainstations_df$ddl_id

readDDLwaterhoogte(station = mainstationcodes, startyear = min(datayear), endyear = max(datayear), outDir = ddlrawdir)

# calculate annual means

for(datayear in datayear){
  
  files <- list.files(ddlrawdir, pattern = as.character(datayear))
  
  waterhoogtes_datayear <- lapply(
    files, function(x){
      read_delim(paste0(ddlrawdir, "/", x),
                 delim = ";", 
                 escape_double = FALSE, 
                 locale = locale(), 
                 col_types = cols(),
                 trim_ws = TRUE, 
                 na = "-999999999") %>%
        filter(!grepl("HW", waardebepalingsmethode.omschrijving)) %>%
        filter(!grepl("LW", waardebepalingsmethode.omschrijving))
    }
  ) %>%
    list_rbind() %>%
    select(
      locatie.code,
      locatie.naam,
      coordinatenstelsel,
      geometriepunt.x,
      geometriepunt.y,
      tijdstip,
      statuswaarde,
      kwaliteitswaarde.code,
      bemonsteringssoort.omschrijving,
      eenheid.code,
      grootheid.omschrijving,
      hoedanigheid.code,
      groepering.code,
      meetapparaat.omschrijving,
      waardebepalingsmethode.omschrijving,
      numeriekewaarde
    )
  
  annual_means <- waterhoogtes_datayear %>%
    filter(kwaliteitswaarde.code < 50,
           groepering.code == "NVT"
    ) %>%
    group_by(locatie.naam,
             locatie.code,
             grootheid.omschrijving
    ) %>%
    summarise(
      annual_mean_mm = mean(numeriekewaarde, na.rm = T) %/% 0.1,
      n = n(),
      .groups = "drop"
    ) %>%
    mutate(
      year = datayear,
      source = "rws_ddl"
    ) %>%
    select(
      year,
      height = annual_mean_mm,
      station = locatie.naam,
      n_per_year = n,
      source
    )

  if(nrow(annual_means) > 0){
    write_delim(
      annual_means, delim = ";",
      file = file.path(ddlmeandir, paste0(datayear, ".csv")
      )
    )
  }  
  
}

waterhoogtes_datayear %>%
  filter(kwaliteitswaarde.code < 50) %>%
  ggplot(aes(x = numeriekewaarde)) +
  geom_histogram()

waterhoogtes_datayear %>%
  count(statuswaarde, kwaliteitswaarde.code)

# 25 means "In ruimte en tijd geïnterpoleerde waardeIn ruimte en tijd geïnterpoleerde waarde"
# 99 means "Hiaat waarde"

# Drop 99, keep 25

waterhoogtes_datayear %>%
  filter(kwaliteitswaarde.code < 50) %>%
  count(locatie.code) %>%
  ggplot(aes(locatie.code, n)) +
  geom_col()

waterhoogtes_datayear %>%
  filter(kwaliteitswaarde.code < 50) %>%
  ggplot(aes(tijdstip, numeriekewaarde)) +
  # geom_line(alpha = 0.1) +
  geom_smooth() +
  scale_x_datetime(date_breaks = "2 month", date_labels = "%h") +
  facet_wrap("locatie.code")



