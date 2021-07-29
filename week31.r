library(tidytuesdayR)
library(tidyverse)
library(ggstatsplot)
library(ggthemes)
tuesdata <- tidytuesdayR::tt_load(2021, week = 31)
olympics <- tuesdata$olympics
regions <- tuesdata$regions
regions <- regions %>% 
  rename(noc=NOC)
olympics <- left_join(olympics,regions)
scored.by_athlete <- olympics %>%
  filter(!is.na(medal)) %>%
  mutate(score=case_when(
    medal=="Gold"~3,
    medal=="Silver"~2,
    medal=="Bronze"~1,
    TRUE~0
  )) %>%
  group_by(year,season,event,medal) %>%
  slice(1)
scored.by_region <- scored.by_athlete %>%
  group_by(region,year,season) %>%
  summarise(
    total_score=sum(score,na.rm=T)
    ) %>%
  group_by(region,season) %>%
  summarise(
    mean_score=mean(total_score,na.rm=T)
    ) 
scored.by_region.ranked <- scored.by_region %>%
  group_by(season) %>%
  mutate(rank=dense_rank(desc(mean_score)))
grouped_ggdotplotstats(data=scored.by_region.ranked %>% filter(rank<11),
              grouping.var=season,
              y=region,
              x=mean_score,
              xlab="Mean medal score (MMS)",
              caption="MMS: average of the weighted sum (per Olympic Games) of all awarded medals (bronze = 1, silver = 2, gold = 3)",
              bf.message=FALSE,
              results.subtitle=FALSE,
              plotgrid.args = list(nrow = 2),
              annotation.args=list(title="Performance in the Olympics by Region (top ten), 1896-2016"),
              ggtheme = ggthemes::theme_economist()
              )