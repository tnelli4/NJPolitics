library(tidyverse)
library(maps)
library(sf)
library(ggplot2)
library(dplyr)

nj_2017_results <- read.csv("nj_gov_2017_csv.txt")
nj_counties <- st_read("NJ_Counties_3857.shx")
nj_2017_results_sf <-left_join(nj_counties, nj_2017_results, by = "COUNTY") #left join to keep it as sf

nj_2017_results_sf$total_votes <- nj_2017_results_sf$PHILIP.MURPHY..Democratic. + nj_2017_results_sf$KIM.GUADAGNO..Republican.
nj_2017_results_sf$winner_votes <- pmax(nj_2017_results_sf$PHILIP.MURPHY..Democratic., nj_2017_results_sf$KIM.GUADAGNO..Republican.)
nj_2017_results_sf$loser_votes <- pmin(nj_2017_results_sf$PHILIP.MURPHY..Democratic., nj_2017_results_sf$KIM.GUADAGNO..Republican.)
nj_2017_results_sf$margin_positive <- ((nj_2017_results_sf$winner_votes - nj_2017_results_sf$loser_votes)/nj_2017_results_sf$total_votes)*100
nj_2017_results_sf$margin <- ((nj_2017_results_sf$PHILIP.MURPHY..Democratic. - nj_2017_results_sf$KIM.GUADAGNO..Republican.)/nj_2017_results_sf$total_votes)*100


ggplot(nj_2017_results_sf) +
  geom_sf(aes(fill = margin)) +
  scale_fill_gradient2(
    low = "red",
    mid = "whitesmoke",
    high = "blue",
    midpoint = 0
  )+
  theme_void()+
  labs(
    title = "2017 NJ Governor Election Margin",
    subtitle = "Democratic (blue) vs Republican (red)",
    fill = "Margin win (%)",
    caption = "Source: NJ DOE"
  )