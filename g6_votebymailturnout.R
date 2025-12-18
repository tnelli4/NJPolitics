library(tidyverse)
library(ggplot2)
library(dplyr)


nj_vbmturnout <- read.csv("nj_votebymailturnout.txt")

nj_vbmturnout <- rename(nj_vbmturnout, Year = Election.Year)
nj_vbmturnout <- filter(nj_vbmturnout, Year > 2010)
nj_vbmturnout <- filter(nj_vbmturnout, Election.Type == "General")

nj_vbmturnout$percent_of_vbm = nj_vbmturnout$Votes.Cast.By.Vote.By.Mail.Ballots/nj_vbmturnout$Total.Number.of.Votes.Cast*100

avg_by_year <- nj_vbmturnout %>%
  group_by(Year) %>%
  summarize(avg_percent = mean(percent_of_vbm, na.rm = TRUE))

ggplot(nj_vbmturnout, aes(x = Year, y = percent_of_vbm, group = County, colour = County))+
  geom_line(aes(color = County), alpha = .5)+
  scale_x_continuous(expand = c(0,0),breaks = seq(min(nj_vbmturnout$Year), max(nj_vbmturnout$Year), by = 2, ))+
  theme_classic()+
  labs(
    title = "Percentage of Ballots Cast that are 'Vote by Mail'",
    y = "% of VBM ballots cast"
  )+
  geom_line(data = avg_by_year,
            aes(x = Year, y = avg_percent),
            linewidth = 1,
            inherit.aes = F,
            color = "black")
