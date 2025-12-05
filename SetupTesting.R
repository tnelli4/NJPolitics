library(tidyverse)
library(maps)
library(sf)
library(ggplot2)


# Base graph of NJ Counties -----------------------------------------------
nj_counties <- st_read("NJ_Counties_3857.shx")

x = 1
ggplot(nj_counties) +
  geom_sf(aes(fill = x)) +
  theme_void()+
  labs(
    title = "Title of Graph",
    subtitle = "Subtitle of Graph",
    fill = "Var Fill Here",
    caption = "Source of data"
  )

# -------------------------------------------------------------------------

NJ_2017_results <- read.csv("nj_gov_2017_csv.txt")

NJ_2017_results$margin = NJ_2017_results$PHILIP.MURPHY..Democratic./NJ_2017_results$KIM.GUADAGNO..Republican.
NJ_2017_results <- full_join(NJ_2017_results, nj_counties, by = "COUNTY")

ggplot(NJ_2017_results) +
  geom_sf(aes(fill = margin)) +
  theme_void()+
  labs(
    title = "Title of Graph",
    subtitle = "Subtitle of Graph",
    fill = "Var Fill Here",
    caption = "Source of data"
  )
