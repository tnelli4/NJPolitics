library(tidyverse)
library(ggplot2)


nj_vbmturnout <- read.csv("nj_votebymailturnout.txt")

nj_vbmturnout <- rename(nj_vbmturnout, Year = Election.Year)
nj_vbmturnout <- filter(nj_vbmturnout, Year > 2010)
nj_vbmturnout <- filter(nj_vbmturnout, Election.Type == "General")

nj_vbmturnout$percent_of_vbm = nj_vbmturnout$Votes.Cast.By.Vote.By.Mail.Ballots/nj_vbmturnout$Total.Number.of.Votes.Cast*100
ggplot(nj_vbmturnout, aes(x = Year, y = percent_of_vbm, group = County, colour = County))+
  geom_line()+
  scale_x_continuous(breaks = seq(min(nj_vbmturnout$Year), max(nj_vbmturnout$Year), by = 2, ))+
  scale_x_continuous(expand = c(0,0))+
  theme_classic()+
  labs(
    title = "Percentage of Ballots Cast that are 'Vote by Mail'",
    y = "% of VBM ballots cast"
  )

