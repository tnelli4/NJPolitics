library(tidyverse)
library(maps)
library(sf)
library(ggplot2)
library(dplyr)
library(scales)

nj_2017_results <- read.csv("nj_gov_2017_csv.txt")
nj_counties <- st_read("NJ_Counties_3857.shx")
nj_2017_results_sf <-left_join(nj_counties, nj_2017_results, by = "COUNTY") #left join to keep it as sf

nj_2017_results_sf$total_votes <- nj_2017_results_sf$PHILIP.MURPHY..Democratic. + nj_2017_results_sf$KIM.GUADAGNO..Republican.
nj_2017_results_sf$winner_votes <- pmax(nj_2017_results_sf$PHILIP.MURPHY..Democratic., nj_2017_results_sf$KIM.GUADAGNO..Republican.)
nj_2017_results_sf$loser_votes <- pmin(nj_2017_results_sf$PHILIP.MURPHY..Democratic., nj_2017_results_sf$KIM.GUADAGNO..Republican.)
nj_2017_results_sf$margin_positive <- ((nj_2017_results_sf$winner_votes - nj_2017_results_sf$loser_votes)/nj_2017_results_sf$total_votes)*100
nj_2017_results_sf$margin <- ((nj_2017_results_sf$PHILIP.MURPHY..Democratic. - nj_2017_results_sf$KIM.GUADAGNO..Republican.)/nj_2017_results_sf$total_votes)*100

map_2017 <- nj_2017_results_sf %>%
  mutate(value = margin)


nj_2025_results <- read.csv("nj_gov_2025_csv.txt")
nj_2025_results <- nj_2025_results %>%
  rename_with(~ paste0("2025_", .x), -COUNTY)

nj_2025_results$margin_2025 <- (nj_2025_results$`2025_SHERRILL_DEM`- nj_2025_results$`2025_CIATTARELLI_REP`)/(nj_2025_results$`2025_SHERRILL_DEM`+ nj_2025_results$`2025_CIATTARELLI_REP`)*100

nj_counties <- st_read("NJ_Counties_3857.shx")

nj_2025_results_sf <- left_join(nj_counties, nj_2025_results, by = "COUNTY")
