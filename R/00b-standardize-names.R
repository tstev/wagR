# Code to download all political parties in the Netherands
library(rvest)
library(dplyr)
library(fuzzyjoin)
library(stringr)
library(reclin)

# URL to the list of Polical parties
url <- "https://nl.wikipedia.org/wiki/Politieke_partijen_in_Nederland"

# XCode to scrape tables
tabs <- c("/html/body/div[3]/div[3]/div[4]/div/table[2]",
          "/html/body/div[3]/div[3]/div[4]/div/table[3]",
          "/html/body/div[3]/div[3]/div[4]/div/table[5]")

nl_parties <- data.frame()
for (t in tabs) {
  # Read in html table
  tmp <- url %>%
    read_html(encoding = "UTF-8") %>%
    html_nodes(xpath = t) %>%
    html_table(fill = TRUE)
  tmp <- tmp[[1]]

  # Ensure colnames are unique
  colnames(tmp) <- make.unique(colnames(tmp), sep = "_")

  # Select only desired columns
  tmp <- tmp %>%
    select(one_of(c("Partij", "Afkorting")))

  # Remove first row if it is same as header
  if (any(colnames(tmp) == unlist(tmp[1L,], use.names = FALSE))) {
    tmp <- tmp[-c(1L), ]
  }

  # Rename the column names
  tmp <- tmp %>%
    rename(PARTY = Partij,
           ABBR = Afkorting)
  rownames(tmp) <- NULL
  nl_parties <- rbind(nl_parties, tmp)
}

# Replace empty cells with NA
nl_parties <- nl_parties %>% mutate(
  ABBR = str_replace_all(ABBR, "$^", NA_character_))

nl_parties <- nl_parties %>% mutate(
  ABBR = str_replace_na(ABBR, ""))



x <- c("VVD", "P.v.d.A.", "PVV", "SP", "CDA", "D66", "ChristenUnie",
       "GROENLINKS", "SGP", "Partij voor de Dieren", "50PLUS", "Ondernemers Partij",
       "VNL", "DENK", "NIEUWE WEGEN", "Forum voor Democratie", "De Burger Beweging",
       "Vrijzinnige Partij", "GeenPeil", "Piraten partij", "Artikel 1",
       "Niet Stemmers", "Libertarische Partij", "Lokaal in de Kamer",
       "JESUS LEEFT", "StemNL", "MenS en Spirit / ...")
x <- data.frame(PARTY = x, ABBR = "")

pairs <- pair_blocking(x, nl_parties)
tmp <- compare_pairs(pairs, by = c("PARTY", "ABRR"),
              comparators = jaro_winkler(0.9))



stringdist_left_join(x, nl_parties, by = "PARTY", ignore_case = TRUE)
stringdist_left_join(x, nl_parties, by = c(PARTY = "ABBR"), ignore_case = TRUE)


tmp <- stringdistmatrix(str_to_lower(x$PARTY),
                        str_to_lower(nl_parties$PARTY))
colnames(tmp) <- nl_parties$PARTY
rownames(tmp) <- x$PARTY
