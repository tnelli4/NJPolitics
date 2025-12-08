nj_2024_results <- read_csv("nj_2024_president_csv.txt")

nj_2025_results <- read.csv("nj_gov_2025_csv.txt")
nj_2025_results <- nj_2025_results %>%
  rename_with(~ paste0("2025_", .x), -COUNTY)

nj_counties <- st_read("NJ_Counties_3857.shx")

nj_swing <- left_join(nj_counties, nj_2024_results, by = "COUNTY")
nj_swing <- left_join(nj_swing, nj_2025_results, by = "COUNTY")

nj_swing$margin_2024 <- (nj_swing$`2024_KAMALA D. HARRIS_Democratic`- nj_swing$`2024_DONALD J. TRUMP_Republican`)/(nj_swing$`2024_KAMALA D. HARRIS_Democratic`+ nj_swing$`2024_DONALD J. TRUMP_Republican`)*100
nj_swing$margin_2025 <- (nj_swing$`2025_SHERRILL_DEM`- nj_swing$`2025_CIATTARELLI_REP`)/(nj_swing$`2025_SHERRILL_DEM`+ nj_swing$`2025_CIATTARELLI_REP`)*100
nj_swing$margin_swing <- nj_swing$margin_2025 - nj_swing$margin_2024


flipped_counties <- c("GLOUCESTER", "CUMBERLAND", "ATLANTIC", "MORRIS", "PASSAIC")

flipped_pts <- nj_2017_results_sf %>% 
  filter(COUNTY %in% flipped_counties) %>% 
  st_point_on_surface()

ggplot(nj_swing) +
  geom_sf(aes(fill = margin_swing)) +
  scale_fill_gradient2(
    low = "red",
    mid = "whitesmoke",
    high = "blue",
    midpoint = 0
  )+
  theme_void()+
  labs(
    title = "2024-2025 NJ  Election Swing ",
    subtitle = "Democratic (blue) vs Republican (red)",
    fill = "Change in Margin win (%)",
    caption = "Source: NJ DOE"
  )+
  geom_sf(data = flipped_pts,
          shape = 8,
          size = 3,
          color = "black")
