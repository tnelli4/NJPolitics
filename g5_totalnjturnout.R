library(ggplot2)

nj_turnout <- read.csv("njturnout.txt")

nj_turnout <- filter(nj_turnout, Year >= 2010)
nj_turnout$percent_cast = nj_turnout$Total.Number.of.Ballots.Cast/nj_turnout$Total.Number.of.Persons.Entitled.to.Vote * 100

ggplot(nj_turnout, aes(x = Year, y = percent_cast))+
  geom_line()+
  geom_smooth(se = F , method = "lm")+
  theme_classic()+
  scale_x_continuous(breaks = seq(min(nj_turnout$Year), max(nj_turnout$Year), by = 2))+
  labs(
    title = "Percent Turnout of Eligible Voters",
    subtitle = "The proportion of eligable voters who went out and casted a ballot over time",
    y = "% of ballots cast" 
  )

nj_turnout_2 <- read.csv("njturnout.txt")
nj_turnout_2$percent_cast = nj_turnout_2$Total.Number.of.Ballots.Cast/nj_turnout_2$Total.Number.of.Persons.Entitled.to.Vote * 100

nj_turnout_2 <- filter(nj_turnout_2, (Year - min(Year))%% 4 == 2)

ggplot(nj_turnout_2, aes(x = Year, y = percent_cast))+
  geom_line()+
  geom_smooth(se = F)+
  theme_classic()+
  scale_x_continuous(breaks = seq(min(nj_turnout$Year), max(nj_turnout$Year), by = 2))+
  labs(
    title = "Percent Turnout of Eligible Voters",
    subtitle = "The proportion of eligable voters who went out and casted a ballot over time",
    y = "% of ballots cast" 
  )
