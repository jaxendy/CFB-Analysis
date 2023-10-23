library(tidyverse)
library(ggrepel)
library(cfbfastR)
library(usethis)
library(dplyr)
library(formattable)
library(Matrix)
library(ggimage)
Sys.setenv(CFBD_API_KEY = "Hw3IwN3G6TDcC73Yt69iw1CraWvKf8oXM7ayXZ+sjAy0E2D61znhDfYZepJhK+AJ")



pbp_2022 <-load_cfb_pbp(
  seasons = 2022,
  dbConnection = NULL,
  tablename = NULL
)

## Loading in the 2023 season data
pbp_2023 <-load_cfb_pbp(
  seasons = 2023,
  dbConnection = NULL,
  tablename = NULL
)

## Loading in the FBS teams
FbsTeams <- cfbd_team_info(year = 2023, only_fbs = TRUE)




week7epa <- pbp_2023 %>%
  group_by(game_id, pos_team) %>% 
  filter(!is.na(EPA)) %>%
  filter(week == 7) %>%
  filter(pos_team %in% FbsTeams$school & def_pos_team %in% FbsTeams$school) %>%
  summarise(def_pos_team,
            epa = sum(EPA),
               plays = n(),
               epa_play = epa / plays,
            passPlays = sum(if_else(pass == 1, 1, 0)),
            rushPlays = sum(if_else(rush == 1, 1, 0)),
            epaPass = sum(if_else(pass == 1, EPA, 0)),
            epaRush = sum(if_else(rush == 1, EPA, 0)),
            epaPerPass = epaPass / passPlays,
            epaPerRush = epaRush / rushPlays) %>%
  filter(!(epaPass == 0 & epaRush == 0)) %>%
  unique()
  
week8epa <- pbp_2023 %>%
  group_by(game_id, pos_team) %>% 
  filter(!is.na(EPA)) %>%
  filter(week == 8) %>%
  filter(wp_before > .05) %>%
  filter(pos_team %in% FbsTeams$school & def_pos_team %in% FbsTeams$school) %>%
  summarise(def_pos_team,
            epa = sum(EPA),
            plays = n(),
            epa_play = epa / plays,
            passPlays = sum(if_else(pass == 1, 1, 0)),
            rushPlays = sum(if_else(rush == 1, 1, 0)),
            epaPass = sum(if_else(pass == 1, EPA, 0)),
            epaRush = sum(if_else(rush == 1, EPA, 0)),
            epaPerPass = epaPass / passPlays,
            epaPerRush = epaRush / rushPlays) %>%
  filter(!(epaPass == 0 & epaRush == 0)) %>%
  unique()

sznepa2023 <- pbp_2023 %>%
  group_by(pos_team) %>% 
  filter(!is.na(EPA)) %>%
  filter(wp_before > .05) %>%
  filter(pos_team %in% FbsTeams$school & def_pos_team %in% FbsTeams$school) %>%
  filter(!(pos_team %in% c("Jacksonville State", "Sam Houston State"))) %>%
  summarise(epa2023 = sum(EPA),
            plays2023 = n(),
            epa_play2023 = epa2023 / plays2023,
            passPlays2023 = sum(if_else(pass == 1, 1, 0)),
            rushPlays2023 = sum(if_else(rush == 1, 1, 0)),
            epaPass2023 = sum(if_else(pass == 1, EPA, 0)),
            epaRush2023 = sum(if_else(rush == 1, EPA, 0)),
            epaPerPass2023 = epaPass2023 / passPlays2023,
            epaPerRush2023 = epaRush2023 / rushPlays2023) %>%
  filter(!(epaPass2023 == 0 & epaRush2023 == 0)) %>%
  unique()

sznepa2022 <- pbp_2022 %>%
  group_by(pos_team) %>% 
  filter(!is.na(EPA)) %>%
  filter(wp_before > .05) %>%
  filter(pos_team %in% FbsTeams$school & def_pos_team %in% FbsTeams$school) %>%
  filter(!(pos_team %in% c("Jacksonville State", "Sam Houston State"))) %>%
  summarise(epa2022 = sum(EPA),
            plays2022 = n(),
            epa_play2022 = epa2022 / plays2022,
            passPlays2022 = sum(if_else(pass == 1, 1, 0)),
            rushPlays2022 = sum(if_else(rush == 1, 1, 0)),
            epaPass2022 = sum(if_else(pass == 1, EPA, 0)),
            epaRush2022 = sum(if_else(rush == 1, EPA, 0)),
            epaPerPass2022 = epaPass2022 / passPlays2022,
            epaPerRush2022 = epaRush2022 / rushPlays2022) %>%
  filter(!(epaPass2022 == 0 & epaRush2022 == 0)) %>%
  unique()

mergedEpa <- merge(sznepa2022, sznepa2023, by = "pos_team", all = TRUE)

mergedEpa %>%
  ggplot(aes(x = epa_play2022, y = epa_play2023)) +
  geom_point() +
  geom_text(
    label = mergedEpa$pos_team) +
  scale_color_identity(aesthetics = c("fill", "color"))+ 
  theme_bw() +
  geom_hline(yintercept = mean(mergedEpa$epa_play2022), linetype = "dashed")+
  geom_vline(xintercept = mean(mergedEpa$epa_play2023), linetype = "dashed")+
  labs(
    x = "EPA/Play 2022",
    y = "EPA/Play 2023",
    title = "EPA/Play in 2022 vs. 2023",
    subtitle = "Win percentage must be greater than 5% before the start of the play")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8))+
  theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 12, hjust = 0.5)) +
  annotate("text", x=-.2225, y=.275, size = 3, color = "purple", fontface = "bold", label= "Good 2023, Bad 2022") + 
  annotate("text", x=-.225, y=-.25, size = 3, color = "red", fontface = "bold", label= "Bad 2023, Bad 2022") +
  annotate("text", x=.18, y=.275, size = 3, color = "green4", fontface = "bold", label= "Good 2023, Good 2022")+
  annotate("text", x=.185, y=-.25, size = 3, color = "purple", fontface = "bold", label= "Bad 2023, Good 2022") 
  


## NO WIN PERCENTAGE


sznepa2023noWp <- pbp_2023 %>%
  group_by(pos_team) %>% 
  filter(!is.na(EPA)) %>%
  filter(pos_team %in% FbsTeams$school & def_pos_team %in% FbsTeams$school) %>%
  filter(!(pos_team %in% c("Jacksonville State", "Sam Houston State"))) %>%
  summarise(epa2023 = sum(EPA),
            plays2023 = n(),
            epa_play2023 = epa2023 / plays2023,
            passPlays2023 = sum(if_else(pass == 1, 1, 0)),
            rushPlays2023 = sum(if_else(rush == 1, 1, 0)),
            epaPass2023 = sum(if_else(pass == 1, EPA, 0)),
            epaRush2023 = sum(if_else(rush == 1, EPA, 0)),
            epaPerPass2023 = epaPass2023 / passPlays2023,
            epaPerRush2023 = epaRush2023 / rushPlays2023) %>%
  filter(!(epaPass2023 == 0 & epaRush2023 == 0)) %>%
  unique()

sznepa2022noWp <- pbp_2022 %>%
  group_by(pos_team) %>% 
  filter(!is.na(EPA)) %>%
  filter(pos_team %in% FbsTeams$school & def_pos_team %in% FbsTeams$school) %>%
  filter(!(pos_team %in% c("Jacksonville State", "Sam Houston State"))) %>%
  summarise(epa2022 = sum(EPA),
            plays2022 = n(),
            epa_play2022 = epa2022 / plays2022,
            passPlays2022 = sum(if_else(pass == 1, 1, 0)),
            rushPlays2022 = sum(if_else(rush == 1, 1, 0)),
            epaPass2022 = sum(if_else(pass == 1, EPA, 0)),
            epaRush2022 = sum(if_else(rush == 1, EPA, 0)),
            epaPerPass2022 = epaPass2022 / passPlays2022,
            epaPerRush2022 = epaRush2022 / rushPlays2022) %>%
  filter(!(epaPass2022 == 0 & epaRush2022 == 0)) %>%
  unique()

mergedEpaNoWp <- merge(sznepa2022noWp, sznepa2023noWp, by = "pos_team", all = TRUE)

mergedEpaNoWp %>%
  ggplot(aes(x = epa_play2022, y = epa_play2023)) +
  geom_point() +
  geom_text(
    label = mergedEpaNoWp$pos_team) +
  scale_color_identity(aesthetics = c("fill", "color"))+ 
  theme_bw() +
  geom_hline(yintercept = mean(mergedEpaNoWp$epa_play2022), linetype = "dashed")+
  geom_vline(xintercept = mean(mergedEpaNoWp$epa_play2023), linetype = "dashed")+
  labs(
    x = "EPA/Play 2022",
    y = "EPA/Play 2023",
    title = "EPA/Play in 2022 vs. 2023")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8))+
  theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 12, hjust = 0.5)) +
  annotate("text", x=-.2225, y=.275, size = 3, color = "purple", fontface = "bold", label= "Good 2023, Bad 2022") + 
  annotate("text", x=-.225, y=-.25, size = 3, color = "red", fontface = "bold", label= "Bad 2023, Bad 2022") +
  annotate("text", x=.18, y=.275, size = 3, color = "green4", fontface = "bold", label= "Good 2023, Good 2022")+
  annotate("text", x=.185, y=-.25, size = 3, color = "purple", fontface = "bold", label= "Bad 2023, Good 2022") 
