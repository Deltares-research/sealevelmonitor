
# add coordinates for mean of stations

  if(nrow(readMainStationLocations() %>% filter(grepl("Netherlands", StationName))) == 0){
    Neth <- readMainStationLocations() %>%
      summarize(
        StationName = "Netherlands",
        Lat = mean(Lat),
        Lon = mean(Lon),
        Station = 10000
      ) 
    
    
    Neth_wo_D <-   bind_rows(
        readMainStationLocations() %>%
          filter(StationName != "DELFZIJL") %>%
          summarize(
            StationName = "Netherlands (without Delfzijl)",
            Lat = mean(Lat),
            Lon = mean(Lon),
            Station = 10001
          )    
      ) 
  
    readMainStationLocations() %>%
      bind_rows(Neth) %>%
      bind_rows(Neth_wo_D) %>%
      mutate(across(everything(), ~ replace(.x, is.na(.x), ""))) %>%
      write_delim("../data/psmsl/NLstations.csv", 
                             delim = ";")
  }

