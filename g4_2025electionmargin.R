
nj_2025_results <- read.csv("nj_gov_2025_csv.txt")
nj_2025_results <- nj_2025_results %>%
  rename_with(~ paste0("2025_", .x), -COUNTY)

nj_2025_results$margin_2025 <- (nj_2025_results$`2025_SHERRILL_DEM`- nj_2025_results$`2025_CIATTARELLI_REP`)/(nj_2025_results$`2025_SHERRILL_DEM`+ nj_2025_results$`2025_CIATTARELLI_REP`)*100

nj_counties <- st_read("NJ_Counties_3857.shx")

nj_2025_results_sf <- left_join(nj_counties, nj_2025_results, by = "COUNTY")

ggplot(nj_2025_results_sf) +
  geom_sf(aes(fill = margin_2025)) +
  scale_fill_gradient2(
    low = "red",
    mid = "whitesmoke",
    high = "blue",
    midpoint = 0,
    limits = c(-80,80),
    breaks = seq(-80,80,20)
  )+
  theme_void()+
  labs(
    title = "2025 NJ Governor Election Margin",
    subtitle = "Democratic (blue) vs Republican (red)",
    fill = "Margin win (%)",
    caption = "Source: NJ DOE"
  )
