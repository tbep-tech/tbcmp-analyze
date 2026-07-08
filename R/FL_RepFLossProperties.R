library(tidyverse)
library(jsonlite)

# Combine into one dataframe
df <- fromJSON("D:/gis/FEMA/NfipMultipleLossProperties.json") %>%
      as_tibble() %>%
      unnest(cols = everything()) %>%
      dplyr::filter(fipsCountyCode %in% c("12017", "12053", "12101", "12103", "12057", "12081", "12115")) %>%
      mutate(censusTract = substr(censusBlockGroup, 1, 11)) %>%
      distinct(censusTract, id) %>%
      group_by(censusTract) %>%
      summarise( count = n())

write.csv(df, file = "./data/output/hot_spot/rpfloss_by_tract.csv", quote = TRUE)
