nj_2017_results <- read.csv("nj_gov_2017_csv.txt")
nj_2017_results <- nj_2017_results %>%
  rename_with(~ paste0("2017_", .x), -COUNTY)

nj_2021_results <- read.csv("nj_gov_2021_csv.txt")
nj_2021_results <- nj_2021_results %>%
  rename_with(~ paste0("2021_", .x), -COUNTY)

nj_counties <- st_read("NJ_Counties_3857.shx")

nj_swing <- left_join(nj_counties, nj_2017_results, by = "COUNTY")
nj_swing <- left_join(nj_swing, nj_2021_results, by = "COUNTY")

nj_swing$margin_2017 <- (nj_swing$`2017_PHILIP.MURPHY..Democratic.`- nj_swing$`2017_KIM.GUADAGNO..Republican.`)/(nj_swing$`2017_PHILIP.MURPHY..Democratic.`+ nj_swing$`2017_KIM.GUADAGNO..Republican.`)*100
nj_swing$margin_2021 <- (nj_swing$`2021_PHILIP.MURPHY..Democratic.`- nj_swing$`2021_JACK.CIATTARELLI..Republican.`)/(nj_swing$`2021_PHILIP.MURPHY..Democratic.`+ nj_swing$`2021_JACK.CIATTARELLI..Republican.`)*100
nj_swing$margin_swing <- nj_swing$margin_2021 - nj_swing$margin_2017

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
    title = "2017-2021 NJ Governor Election Swing ",
    subtitle = "Democratic (blue) vs Republican (red)",
    fill = "Change in Margin win (%)",
    caption = "Source: NJ DOE"
  )

# -------------------------------------------------------------------------

p1 <- ggplot(nj_swing) +
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
    title = "2017-2021 NJ Governor Election Swing ",
    subtitle = "Democratic (blue) vs Republican (red)",
    fill = "Change in Margin win (%)",
    caption = "Source: NJ DOE"
  )

p2 <- ggplot(nj_swing) +
  geom_sf(aes(fill = margin_2021)) +
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
    title = "2021 NJ Governor Election Margin",
    subtitle = "Democratic (blue) vs Republican (red)",
    fill = "Margin win (%)",
    caption = "Source: NJ DOE"
  )

p3 <-ggplot(nj_swing) +
  geom_sf(aes(fill = margin_2017)) +
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
    title = "2017 NJ Governor Election Margin",
    subtitle = "Democratic (blue) vs Republican (red)",
    fill = "Margin win (%)",
    caption = "Source: NJ DOE"
  )
library(patchwork)
p3 + p2 + p1



