#ZIP

library(dplyr)
library(magrittr)
library(knitr)
library(ggplot2)
library(tidyr)
library(forcats)
library(kableExtra)
library(rstan)
library(loo)
set.seed(42)


setwd("C:/Users/lucap/OneDrive/Desktop/Bayesian")
getwd()

#read csv
data = read.csv("/Users/lucap/OneDrive/Desktop/Bayesian/df_full_premierleague.csv", sep = ',')
#make a sub-df for first analysis based on the paper
df_full = data.frame(data$X, # index
                     data$season, # season reference
                     data$home_team, # home team
                     data$away_team, # away team
                     data$goal_home_ft, # home team goals scored in the match
                     data$goal_away_ft, # away team goals scored in the match
                     data$home_possession, # home team possession in the match
                     data$away_possession, # away team possession in the match
                     data$home_shots, # home team shots in the match
                     data$away_shots, # away team shots in the match
                     data$home_passes, # home team passes in the match
                     data$away_passes # away team passes in the match
)

df_full <- df_full %>% 
  rename(
    "g" = "data.X"
    ,"season" = "data.season"
    ,"home_team" = "data.home_team"
    ,"away_team" = "data.away_team"
    ,"goal_home" = "data.goal_home_ft"
    ,"goal_away" = "data.goal_away_ft"
    ,"home_possession" = "data.home_possession"
    ,"away_possession" = "data.away_possession"
    ,"home_shots" = "data.home_shots" 
    ,"away_shots" = "data.away_shots" 
    ,"home_passes" = "data.home_passes"
    ,"away_passes" = "data.away_passes"
  )

#df_full = df_full[ df_full$data.season == '20/21' | df_full$data.season == '19/20' | df_full$data.season == '18/19' , ]
df1 = df_full[df_full$season == '18/19' , ]
df1['g'] <- df1['g'] - 3038

#made a unique index
df1 <- df1 %>%   mutate(home_id = case_when(home_team == "Manchester United" ~ 1
                                            ,home_team == "Leicester City" ~ 2
                                            ,home_team == "Wolverhampton Wanderers" ~ 3
                                            ,home_team == "Cardiff City" ~ 4
                                            ,home_team == "West Ham United" ~ 5
                                            ,home_team == "Liverpool" ~ 6
                                            ,home_team == "Everton" ~ 7
                                            ,home_team == "Southampton" ~ 8
                                            ,home_team == "Brighton and Hove Albion" ~ 9
                                            ,home_team == "Arsenal" ~ 10
                                            ,home_team == "AFC Bournemouth" ~ 11
                                            ,home_team == "Manchester City" ~ 12
                                            ,home_team == "Watford" ~ 13
                                            ,home_team == "Crystal Palace" ~ 14
                                            ,home_team == "Chelsea" ~ 15
                                            ,home_team == "Burnley" ~ 16
                                            ,home_team == "Tottenham Hotspur" ~ 17
                                            ,home_team == "Huddersfield Town" ~ 18
                                            ,home_team == "Newcastle United" ~ 19
                                            ,home_team == "Fulham" ~ 20
                                            ,TRUE ~ 99  ))

df1 <- df1 %>%   mutate(away_id = case_when(away_team == "Manchester United" ~ 1
                                            ,away_team == "Leicester City" ~ 2
                                            ,away_team == "Wolverhampton Wanderers" ~ 3
                                            ,away_team == "Cardiff City" ~ 4
                                            ,away_team == "West Ham United" ~ 5
                                            ,away_team == "Liverpool" ~ 6
                                            ,away_team == "Everton" ~ 7
                                            ,away_team == "Southampton" ~ 8
                                            ,away_team == "Brighton and Hove Albion" ~ 9
                                            ,away_team == "Arsenal" ~ 10
                                            ,away_team == "AFC Bournemouth" ~ 11
                                            ,away_team == "Manchester City" ~ 12
                                            ,away_team == "Watford" ~ 13
                                            ,away_team == "Crystal Palace" ~ 14
                                            ,away_team == "Chelsea" ~ 15
                                            ,away_team == "Burnley" ~ 16
                                            ,away_team == "Tottenham Hotspur" ~ 17
                                            ,away_team == "Huddersfield Town" ~ 18
                                            ,away_team == "Newcastle United" ~ 19
                                            ,away_team == "Fulham" ~ 20
                                            ,TRUE ~ 99  ))



g = nrow(df1)

stan_dat_1 <- list(
  g = g, 
  teams = length(unique(df1$home_team)), #number of teams
  home_id = df1$home_id,
  away_id = df1$away_id,
  goal_home = df1$goal_home,
  goal_away = df1$goal_away
  #pn = pn, #games want to predict
  #pred_home_id = df1$home_id[376:g],
  #pred_away_id = df1$away_id[376:g]
)


comp_model_ZIP<- stan_model(file = "ZIP_Regression_model.stan")
# Sampling (refresh = 0 avoid printing the trace)
fit_ZIP <- sampling(comp_model_ZIP, 
                    data = stan_dat_1, core = 8, iter = 4000)

check_hmc_diagnostics(fit_ZIP)
ZIP_params = extract(fit_ZIP)

traceplot(fit_ZIP,par=c("home"))

print(fit_ZIP, pars = c('home','att[1]','def[1]'))

teams <- unique(df1$home_team)

fit.summary <- summary(fit_ZIP)
att_t <- summary(fit_ZIP, pars = "att", probs = c('0.025', '0.975'))
att_t <- att_t$summary
att_t <- as.data.frame(att_t)

att_t <- att_t %>%
  mutate(team = teams) %>%
  relocate(team, .before = mean)
att_t <- att_t[1:6]
att_t <- att_t[order(-att_t$mean), ]

kbl(att_t, format = "html", align = "c", digits = 3) %>%
  kable_paper(full_width = FALSE) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))





