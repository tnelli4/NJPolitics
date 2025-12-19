library(tidyverse)
library(maps)
library(sf)
library(ggplot2)
library(dplyr)
library(scales)

nj_2017_results <- read.csv("nj_gov_2017_csv.txt")
nj_2017_results <- nj_2017_results %>%
  rename_with(~ paste0("2017_", .x), -COUNTY)

nj_2021_results <- read.csv("nj_gov_2021_csv.txt")
nj_2021_results <- nj_2021_results %>%
  rename_with(~ paste0("2021_", .x), -COUNTY)


nj_2025_results <- read.csv("nj_gov_2025_csv.txt")
nj_2025_results <- nj_2025_results %>%
  rename_with(~ paste0("2025_", .x), -COUNTY)

nj_counties <- st_read("NJ_Counties_3857.shx")

nj_voter_reg <- read_csv("nj_voter_reg.txt")

nj_results <- left_join(nj_counties, nj_2017_results, by = "COUNTY")
nj_results <- left_join(nj_results, nj_2021_results, by = "COUNTY")
nj_results <- left_join(nj_results, nj_2025_results, by = "COUNTY")

nj_results$margin_2017 <- (nj_results$`2017_PHILIP.MURPHY..Democratic.`- nj_results$`2017_KIM.GUADAGNO..Republican.`)/(nj_results$`2017_PHILIP.MURPHY..Democratic.`+ nj_results$`2017_KIM.GUADAGNO..Republican.`)*100
nj_results$margin_2021 <- (nj_results$`2021_PHILIP.MURPHY..Democratic.`- nj_results$`2021_JACK.CIATTARELLI..Republican.`)/(nj_results$`2021_PHILIP.MURPHY..Democratic.`+ nj_results$`2021_JACK.CIATTARELLI..Republican.`)*100
nj_results$margin_2025 <- (nj_results$`2025_SHERRILL_DEM`- nj_results$`2025_CIATTARELLI_REP`)/(nj_results$`2025_SHERRILL_DEM`+ nj_results$`2025_CIATTARELLI_REP`)*100
nj_results$change_margin2017_pp <- nj_results$margin_2025 - nj_results$margin_2017
nj_results$change_margin2021_pp <- nj_results$margin_2025 - nj_results$margin_2021


nj_voter_reg$dem_reg_share_2017 = nj_voter_reg$`2017_DEM`/nj_voter_reg$`2017_Total`
nj_voter_reg$dem_reg_share_2021 = nj_voter_reg$`2021_DEM`/nj_voter_reg$`2021_Total`
nj_voter_reg$dem_reg_share_2025 = nj_voter_reg$`2025_DEM`/nj_voter_reg$`2025_Total`
nj_voter_reg$change_dem_reg_share2017_pp = 100*(nj_voter_reg$dem_reg_share_2025 - nj_voter_reg$dem_reg_share_2017)
nj_voter_reg$change_dem_reg_share2021_pp = 100*(nj_voter_reg$dem_reg_share_2025 - nj_voter_reg$dem_reg_share_2021)


scatter_data2017 <- nj_voter_reg %>%
  select(COUNTY, change_dem_reg_share2017_pp) %>%
  inner_join(nj_results %>%
               select(COUNTY, change_margin2017_pp), by = "COUNTY")


ggplot(scatter_data2017, aes(x = change_dem_reg_share2017_pp, y = change_margin2017_pp))+
  geom_point()+
  geom_text(aes(label = COUNTY), vjust = -0.5, hjust = .5, size = 3)+
  theme_bw()+
  geom_smooth(se = F, method = "lm")+
  labs(
    title = "Registration Shift vs Vote-Margin Shift (2017 → 2025)",
    subtitle = "Each point is a county. Positive values indicate a shift toward Democrats.",
    x = "Δ Democratic registration share (percentage points), 2017 → 2025",
    y = "Δ Democratic two-party margin (percentage points), 2017 → 2025",
    caption = "Sources: NJ Division of Elections (results, 2017 & 2025); SVRS registration snapshots (on/near General Election Day). Two-party margin uses D and R only."
  )+
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_hline(yintercept = 0, linetype = "dotted") 

# -------------------------------------------------------------------------



scatter_data2021 <- nj_voter_reg %>%
  select(COUNTY, change_dem_reg_share2021_pp) %>%
  inner_join(nj_results %>%
               select(COUNTY, change_margin2021_pp), by = "COUNTY")


ggplot(scatter_data2021, aes(x = change_dem_reg_share2021_pp, y = change_margin2021_pp))+
  geom_point()+
  geom_text(aes(label = COUNTY), vjust = -0.5, hjust = .5, size = 3)+
  theme_bw()+
  geom_smooth(se = F,method = "lm")+
  labs(
    title = "Registration Shift vs Vote-Margin Shift (2021 → 2025)",
    subtitle = "Each point is a county. Positive values indicate a shift toward Democrats.",
    x = "Δ Democratic registration share (percentage points), 2021 → 2025",
    y = "Δ Democratic two-party margin (percentage points), 2021 → 2025",
    caption = "Sources: NJ Division of Elections (results, 2021 & 2025); SVRS registration snapshots (on/near General Election Day). Two-party margin uses D and R only."
  )

