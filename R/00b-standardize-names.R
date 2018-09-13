# Code to download all political parties in the Netherands
library(rvest)
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
  nl_parties <- rbind(nl_parties, tmp)
}
