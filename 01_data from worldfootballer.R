# collects EPL match results for 2022-23 season from understat via worldfootballer

library(tidyverse)
library(tidylog)
library(janitor)
library(worldfootballR)

# some custom functions
source("~/Data/r/basic functions.R")

# get data
epl_results2223 <- understat_league_match_results(league = "EPL", season_start_year = 2022) %>%
	select(-isResult)
glimpse(epl_results2223)

epl_results2223 %>%
	count(home_team) %>%
	view()

epl_2223_byteam_home <- epl_results2223 %>%
	select(home_team, home_goals, away_goals, home_xG, away_xG, forecast_win, forecast_draw, forecast_loss) %>%
	group_by(home_team) %>%
	mutate(ga_home = away_goals) %>%
	mutate(xga_home = away_xG) %>%
	mutate(points_home = case_when(home_goals > away_goals ~ 3,
																 home_goals < away_goals ~ 0,
																 home_goals == away_goals ~ 1)) %>%
	mutate(points_exp_home = case_when(((forecast_win > forecast_draw) & (forecast_win > forecast_loss)) ~ 3,
																		 ((forecast_draw > forecast_win) & (forecast_draw > forecast_loss)) ~ 1,
																		 TRUE ~ 0)) %>%
	mutate(Total = rowSums(across(where(is.numeric)))) %>%
	bind_rows(summarize(., description.y = "Total", across(where(is.numeric), sum))) %>%
	filter(description.y == "Total") %>%
	mutate(goals_minus_xg_home = home_goals - home_xG) %>%
	mutate(ga_minus_xga_home = ga_home - xga_home) %>%
	mutate(points_minus_points_exp_home = points_home - points_exp_home) %>%
	ungroup() %>%
	select(team = home_team, goals_home = home_goals, xg_home = home_xG, goals_minus_xg_home,
				 ga_home, xga_home, ga_minus_xga_home,
				 points_home, points_exp_home, points_minus_points_exp_home)
glimpse(epl_2223_byteam_home)


epl_2223_byteam_away <- epl_results2223 %>%
	select(away_team, home_goals, away_goals, away_xG, home_xG, forecast_win, forecast_draw, forecast_loss) %>%
	group_by(away_team) %>%
	mutate(ga_away = home_goals) %>%
	mutate(xga_away = home_xG) %>%
	mutate(points_away = case_when(home_goals <  away_goals ~ 3,
																 home_goals > away_goals ~ 0,
																 home_goals == away_goals ~ 1)) %>%
	mutate(points_exp_away = case_when(((forecast_loss > forecast_draw) & (forecast_win < forecast_loss)) ~ 3,
																		 ((forecast_draw > forecast_win) & (forecast_draw > forecast_loss)) ~ 1,
																		 TRUE ~ 0)) %>%
	mutate(Total = rowSums(across(where(is.numeric)))) %>%
	bind_rows(summarize(., description.y = "Total", across(where(is.numeric), sum))) %>%
	filter(description.y == "Total") %>%
	mutate(goals_minus_xg_away = away_goals - away_xG) %>%
	mutate(ga_minus_xga_away = ga_away - xga_away) %>%
	mutate(points_minus_points_exp_away = points_away - points_exp_away) %>%
	ungroup() %>%
	select(team = away_team, goals_away = away_goals, xg_away = away_xG, goals_minus_xg_away,
				 ga_away, xga_away, ga_minus_xga_away,
				 points_away, points_exp_away, points_minus_points_exp_away)
glimpse(epl_2223_byteam_away)

## bring in league table info...note, this is from FB Ref, and XG formula is different from understat. FB Ref uses opta,
 ## understat has their own formula
epltable_2223 <- fb_season_team_stats(country = "ENG", gender = "M", season_end_year = "2023", tier = "1st",
																			stat_type = "league_table") %>%
	rename(team = Squad) %>%
	mutate(team = case_when(team == "Nott'ham Forest" ~ "Nottingham Forest",
													team == "Manchester Utd" ~ "Manchester United",
													team == "Newcastle Utd" ~ "Newcastle United",
													TRUE ~ team))
glimpse(epltable_2223)

epltable_2223 %>%
	count(team)


## merge all together
epl_2223_byteam_all <- epl_2223_byteam_home %>%
	merge(epl_2223_byteam_away) %>%
	mutate(team = case_when(team == "Wolverhampton Wanderers" ~ "Wolves",
													 team == "Leeds" ~ "Leeds United",
													 team == "Leicester" ~ "Leicester City",
													 TRUE ~ team)) %>%
	merge(epltable_2223) %>%
	mutate(goals_total = goals_home + goals_away) %>%
	mutate(xg_total = xg_home + xg_away) %>%
	mutate(goals_minus_xg_total = goals_total - xg_total) %>%
	mutate(ga_total = ga_home + ga_away) %>%
	mutate(xga_total = xga_home + xga_away) %>%
	mutate(ga_minus_xga_total = ga_total - xga_total) %>%
	mutate(points_total = points_home + points_away) %>%
	mutate(points_exp_total = points_exp_home + points_exp_away) %>%
	mutate(points_minus_points_exp_total = points_total - points_exp_total) %>%
	select(team, rank = Rk, W:L, Pts, Pts.MP, points_total, points_home, points_away,
				 points_exp_total, points_exp_home, points_exp_away,
				 points_minus_points_exp_total, points_minus_points_exp_home, points_minus_points_exp_away,
				 goals_total, goals_home, goals_away,
				 xg_total, xg_home, xg_away, goals_minus_xg_total, goals_minus_xg_home, goals_minus_xg_away,
				 ga_total, ga_home, ga_away, xga_total, xga_home, xga_away, ga_minus_xga_total,
				 GF:GD, xG:xGD.90)

glimpse(epl_2223_byteam_all)

epl_results2223 %>%
	ggplot(aes(home_xG, home_goals), color = home_team) +
	geom_point(aes(color = home_team))

write_csv(epl_2223_byteam_all, "~/Data/football data files/epl_2223_byteam.csv")

