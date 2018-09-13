# LOAD LIBRARIES REQUIRED ------------------------------------------------------
library(tidyverse)
library(tmap)
library(sf)
library(data.table)
library(httr)

# LOAD AND PRE-PROCESS DATA ----------------------------------------------------
# Wageningen Polling Station results
wag_election_res <- fread("data/wagening_results.csv", nrows = 30L,
                          na.strings = "", drop = c(1L, 22L, 23L),
                          encoding = "UTF-8")

# Clean column names
setnames(wag_election_res, "V2", "PARTY")

# Remove election results from 2012
wag_election_res <- wag_election_res[!str_detect(PARTY, "TK 2012"),]

# Extract/clean polling station names
poll_station_names <- str_exclude(colnames(wag_election_res), "PARTY")
poll_station_names <- str_replace_all(poll_station_names, "^\\d+(\\.|\\s*)", "")
poll_station_names <- str_trim(poll_station_names)

# Extract/clean political party names
party_names <- str_exclude(wag_election_res[, PARTY], "BLANCO|NIET GELDIG")
str_to_title(party_names)

wag_res <- melt(wag_res, id.vars = "PARTY", variable.name = "STATION",
                value.name = "RESULTS", variable.factor = FALSE)

# Wageningen Polling stations CBS file
wag_stations <- read_csv("data/2fc13394-c2fc-4492-843c-cba07e4bf8f5.csv") %>%
  filter(Gemeente == "Wageningen") %>%
  select(`CBS buurtnummer`, Wijknaam, `CBS wijknummer`, Buurtnaam, `Naam stembureau`,
         Straatnaam, Huisnummer, Huisnummertoevoeging, Postcode,
         Longitude, Latitude)
setDT(wag_stations)
setnames(wag_stations, "Naam stembureau", "STATION")


tmp <- stringdist_semi_join(wag_res[PARTY == "VVD",],
                            wag_stations, by = "STATION")

# Select only Wagenengen Polling stations
wag_dat <- stations
tmpfile <- tempfile(fileext = ".zip")

# Get Wageningen BUURT shapefile
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

nl_mun <- st_read(request, stringsAsFactors = FALSE)

# -------
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

nl_buurt <- st_read(request, stringsAsFactors = FALSE)

# -------------
wag_sf <- nl_mun %>% filter(statcode == "GM0289")

wag_buurten <- st_intersection(wag_sf, nl_buurt)

wag_polls <- st_as_sf(wag_dat, coords = c("Longitude", "Latitude"))
st_crs(wag_polls) <- 4326
qtm(wag_polls)


qtm(wag_buurten)

qtm(wag_buurten) +
  qtm(wag_polls)

qtm(wag_polls)
