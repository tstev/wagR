# LOAD LIBRARIES REQUIRED ------------------------------------------------------
library(tidyverse)
library(tmap)
library(sf)
library(data.table)
library(httr)
source("R/str_exclude.R")

# LOAD AND PRE-PROCESS DATA ----------------------------------------------------
# Wageningen Polling Station results
wag_election_res <- fread("data/wagening_results.csv", nrows = 30L,
                          na.strings = "", drop = c(1L, 22L, 23L),
                          encoding = "UTF-8")

# Clean column names
setnames(wag_election_res, "V2", "PARTY")

# Remove election results from 2012
wag_election_res <- wag_election_res[!str_detect(PARTY, "TK 2012"),]

# Save the data
saveRDS(wag_election_res, file = "output/wag_election_results.rds")

# Extract/clean polling station names
poll_station_names <- str_exclude(colnames(wag_election_res), "PARTY")
poll_station_names <- str_replace_all(poll_station_names, "^\\d+(\\.|\\s*)", "")
poll_station_names <- str_trim(poll_station_names)

# Extract/clean political party names
party_names <- str_exclude(wag_election_res[, PARTY], "BLANCO|NIET GELDIG")

wag_election_res <- melt(wag_election_res, id.vars = "PARTY", variable.name = "STATION",
                value.name = "RESULTS", variable.factor = FALSE)

# Wageningen Polling stations CBS file
wag_stations <- read_csv("data/2fc13394-c2fc-4492-843c-cba07e4bf8f5.csv") %>%
  filter(Gemeente == "Wageningen") %>%
  select(`CBS buurtnummer`, Wijknaam, `CBS wijknummer`, Buurtnaam, `Naam stembureau`,
         Straatnaam, Huisnummer, Huisnummertoevoeging, Postcode,
         Longitude, Latitude)
setDT(wag_stations)
setnames(wag_stations, "Naam stembureau", "STATION")

# Get Wageningen gemeente shapefile en data
url <- list(hostname = "geodata.nationaalgeoregister.nl/cbsgebiedsindelingen/wfs",
            scheme = "https",
            query = list(service = "WFS",
                         version = "2.0.0",
                         request = "GetFeature",
                         typename =
                           "cbsgebiedsindelingen:cbs_gemeente_2017_gegeneraliseerd",
                         outputFormat = "application/json")) %>%
  setattr("class","url")
request <- build_url(url)

nl_mun <- st_read(request, stringsAsFactors = FALSE) %>%
            select(statcode, gemeente = statnaam, geometry) %>%
              filter(gemeente == "Wageningen")

# Get Dutch buurten shapefiles en data
url <- list(hostname = "geodata.nationaalgeoregister.nl/cbsgebiedsindelingen/wfs",
            scheme = "https",
            query = list(service = "WFS",
                         version = "2.0.0",
                         request = "GetFeature",
                         typename =
                           "cbsgebiedsindelingen:cbs_buurt_2017_gegeneraliseerd",
                         outputFormat = "application/json")) %>%
  setattr("class","url")
request <- build_url(url)

nl_buurt <- st_read(request, stringsAsFactors = FALSE) %>%
              select(statcode, buurt = statnaam, geometry)

# Get Wageningen Buurt files
wag_buurten <- st_intersection(nl_mun, nl_buurt)


wag_stations <- st_as_sf(wag_stations, coords = c("Longitude", "Latitude"))
st_crs(wag_stations) <- 4326



wag_election_res$STATION <- ifelse(wag_election_res$STATION == "1.Gem.huis", "Gemeente Wageningen",
                          ifelse(wag_election_res$STATION == "2.Nudehof", "Verzorgingshuis De Nudehof",
                                 ifelse(wag_election_res$STATION == "3.Rumah Kita", "Verzorgingshuis Rumah Kita",
                                        ifelse(wag_election_res$STATION == "4.'t Startpunt", "'t Startpunt",
                                               ifelse(wag_election_res$STATION == "5 Gerk.Vrij", "Gereformeerde Kerk Vrijgemaakt",
                                                      ifelse(wag_election_res$STATION == "6. Johan Friso Z", "Johan Frisoschool Zuid",
                                                             ifelse(wag_election_res$STATION == "7. Pomhorst", "Wijkcentrum De Pomhorst",
                                                                    ifelse(wag_election_res$STATION == "8. Tarthorst", "OBS De Tarthorst",
                                                                           ifelse(wag_election_res$STATION == "9. Tuindorp", "Speeltuinvereniging Tuindorp",
                                                                                  ifelse(wag_election_res$STATION == "10.Piekschooll", "H.J. Piekschool",
                                                                                         ifelse(wag_election_res$STATION == "11. Margrietschool", "De Margrietschool",
                                                                                                ifelse(wag_election_res$STATION == "12. Vlinder", "Sporthal De Vlinder",
                                                                                                       ifelse(wag_election_res$STATION == "13 Belmonte", "Serviceflag Belmonte",
                                                                                                              ifelse(wag_election_res$STATION == "14.Bibliotheek", "BBLTHK",
                                                                                                                     ifelse(wag_election_res$STATION == "15 Nol in't Bosch", "Hotel Nol in 't Bosch",
                                                                                                                            ifelse(wag_election_res$STATION == "16.Neijenoord", "OBS De Nijenoord",
                                                                                                                                   ifelse(wag_election_res$STATION == "17. WAVV", "Voetbalvereniging WAVV",
                                                                                                                                          ifelse(wag_election_res$STATION == "18.Brandweer", "De Brandweerkazerne",
                                                                                                                                                 ifelse(wag_election_res$STATION == "19.Campus", "Campus WUR Forumgebouw", "hou op")))))))))))))))))))


