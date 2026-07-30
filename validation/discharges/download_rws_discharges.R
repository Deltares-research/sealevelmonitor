library(httr)
library(jsonlite)
library(dplyr)
library(stringr)

# ================================
# 1. Nieuwe API: Ophalen Catalogus
# ================================
rwsapi2_get_catalogus <- function() {
  
  url <- "https://ddapi20-waterwebservices.rijkswaterstaat.nl/METADATASERVICES/OphalenCatalogus"
  
  body <- list(
    CatalogusFilter = list(
      Compartimenten = TRUE,
      Grootheden = TRUE,
      Parameters = TRUE,
      Fenomenen = TRUE,
      Locaties = TRUE
    )
  )
  
  res <- httr::POST(url,
                    body = body,
                    encode = "json",
                    httr::accept("application/json"))
  
  if (res$status_code != 200) {
    stop("Fout bij ophalen catalogus: ", res$status_code)
  }
  
  json <- jsonlite::fromJSON(content(res, "text", encoding = "UTF-8"), flatten = TRUE)
  
  return(json)
}

cat <- rwsapi2_get_catalogus()
class(cat$AquoMetadataLijst)
class(cat$AquoMetadataLocatieLijst)
class(cat$LocatieLijst)

cat_0 <- cat$AquoMetadataLijst %>% 
  left_join(cat$AquoMetadataLocatieLijst, by = c(AquoMetadata_MessageID = "AquoMetaData_MessageID")) %>%
  left_join(cat$LocatieLijst, by = c(Locatie_MessageID = "Locatie_MessageID"))

# zoek naar keyword:
# 

# =====================================
# 2. Zoeken in locaties op sleutelwoord
# =====================================

keywords2 <- c("kornwerderzand", "den oever", "kornwerderzand", 
               "haringvliet", "waterweg", "lauwers",
               "eems", "dollard", "ijmuiden", "maassluis")

locaties <- cat_0 %>% 
  filter(Grootheid.Code == "Q") %>%
  filter(grepl( pattern <- paste(keywords2, collapse = "|"), tolower(Naam))) %>%
  select(Naam)



library(httr)
library(jsonlite)
library(dplyr)

# =======================================================
#  rwsapi2_get_measurements()
#  Nieuwe DDAPI20 Rijkswaterstaat Waterwebservices API
#  Ophalen van tijdreeksen (debiet, waterstand, etc.)
# =======================================================

# nog working yet.. a.o. Q (debiet) is not a parameter, but a "grootheid"

rwsapi2_get_measurements <- function(location_id,
                                     parameter_code,
                                     start_date,
                                     end_date) {
  
  url <- "https://ddapi20-waterwebservices.rijkswaterstaat.nl/WAARNEMINGENSERVICES/OphalenWaarnemingen"
  
  # Volgens de nieuwe API moeten we een JSON-body meesturen.
  # (Dit is expliciet onderdeel van DDAPI20, waarbij GET niet meer mag worden gebruikt.) 
  # Bron: RWS migratie naar WaterWebservices DDAPI20. [1](https://waterinfo.rws.nl/publiek/waterhoogte)
  body <- list(
    WaarnemingenFilter = list(
      Locatie = location_id,
      Grootheid = parameter_code,
      Periode = list(
        Begindatum = paste0(start_date, "T00:00:00+01:00"),
        Einddatum  = paste0(end_date,   "T23:59:59+01:00")
      ),
      Metadata = TRUE,
      Waarnemingen = TRUE
    )
  )
  
  res <- httr::POST(
    url,
    body = body,
    encode = "json",
    httr::accept("application/json")
  )
  
  if (res$status_code != 200) {
    stop("API‑fout: HTTP ", res$status_code, "\nResponse:\n", content(res, "text"))
  }
  
  json <- jsonlite::fromJSON(content(res, "text", encoding = "UTF-8"), flatten = TRUE)
  
  if (is.null(json$Waarnemingen)) {
    warning("Geen waarnemingen beschikbaar voor deze combinatie.")
    return(NULL)
  }
  
  df <- as.data.frame(json$Waarnemingen)
  
  # Maak tijdkolom bruikbaar
  if ("Tijdstip" %in% names(df)) {
    df$timestamp <- as.POSIXct(df$Tijdstip, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")
  }
  
  df$locatie   <- location_id
  df$parameter <- parameter_code
  
  return(df)
}


df <- rwsapi2_get_measurements(
  location_id = "HVN003",      # Haringvliet (voorbeeld)
  parameter_code = "WATQ",     # debiet / waterafvoer (voorbeeldcode)
  start_date = "2020-01-01",
  end_date   = "2020-12-31"
)

head(df)


