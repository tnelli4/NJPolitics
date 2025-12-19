nj_2024_results <- read_csv("nj_2024_president_csv.txt")

nj_2020_results <- read_csv("nj_2020_president_csv.txt")

nj_counties <- st_read("NJ_Counties_3857.shx")

nj_swing <- left_join(nj_counties, nj_2024_results, by = "COUNTY")
nj_swing <- left_join(nj_swing, nj_2020_results, by = "COUNTY")

nj_swing$margin_2024 <- (nj_swing$`2024_KAMALA D. HARRIS_Democratic`- nj_swing$`2024_DONALD J. TRUMP_Republican`)/(nj_swing$`2024_KAMALA D. HARRIS_Democratic`+ nj_swing$`2024_DONALD J. TRUMP_Republican`)*100
nj_swing$margin_2020 <- (nj_swing$`2020_JOSEPH R. BIDEN_Democratic`- nj_swing$`2020_DONALD J. TRUMP_Republican`)/(nj_swing$`2020_JOSEPH R. BIDEN_Democratic`+ nj_swing$`2020_DONALD J. TRUMP_Republican`)*100
nj_swing$margin_swing <- nj_swing$margin_2024 - nj_swing$margin_2020

ggplot(nj_swing) +
  geom_sf(aes(fill = margin_swing)) +
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
    title = "2020-2024 NJ Election Swing ",
    subtitle = "Democratic (blue) vs Republican (red)",
    fill = "Change in Margin win (%)",
    caption = "Source: NJ DOE"
  )
