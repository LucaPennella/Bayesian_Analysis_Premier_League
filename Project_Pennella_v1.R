### Project Bayesian Statistics - Luca Pennella ###

#Premier league data analysis inspired by the Bayesian statistical techniques used in the paper:
#'Baio, Gianluca, and Marta Blangiardo. "Bayesian hierarchical model for the prediction of football results." Journal of Applied Statistics 37.2 (2010): 253-264.'

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

mc.cores = parallel::detectCores()

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


mcu <- df1[c(1, 33), ]

#comparison between home and away goals
ds_goal <- df1[, c("goal_home", "goal_away")]
ds_goal = pivot_longer(ds_goal, cols = c("goal_home", "goal_away"))

ggplot(data = ds_goal, aes(x = value,  fill = name)) +
  geom_histogram(position = position_dodge(), binwidth = 0.5) + 
  scale_x_continuous(breaks=0:8) + 
  labs(x ='Goals', y='Number of Games', title = 'Comparison between home and away goals')

#Best and worst attack and defense
home_att <- df1[, c("home_team","goal_home")]
away_att <- df1[, c("away_team", "goal_away")]
home_att = aggregate(home_att$goal_home, list(home_att$home_team), FUN=sum) 
away_att = aggregate(away_att$goal_away, list(away_att$away_team), FUN=sum)
colnames(home_att) <- c("team","goal_home")
colnames(away_att) <- c("team","goal_away")

goals_teams = merge(home_att, away_att, by="team")
goals_teams["tot_goals"] = goals_teams["goal_away"] + goals_teams["goal_home"]
all_goals <- goals_teams[, c("team", "tot_goals")]

all_goals %>%
  mutate(team = fct_reorder(team,  tot_goals))  %>%
  ggplot( aes(x=team, y=tot_goals)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.8) +
  coord_flip() +
  xlab("") +
  theme_bw() + 
  labs(y ='Number of Goals', title = 'Number of goals scored divided by team')


tables = read.csv("/Users/lucap/OneDrive/Desktop/Bayesian/epl_1819.csv", sep = ',')


sub_goal <- tables[, c("Team","defence_goals_conceeded")]

sub_goal %>%
  mutate(Team = fct_reorder(Team,  desc(defence_goals_conceeded)))  %>%
  ggplot( aes(x=Team, y=defence_goals_conceeded)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.8) +
  coord_flip() +
  xlab("") +
  theme_bw() + 
  labs(y ='Number of Goals', title = 'Number of goals conceded divided by team')


# generate tables
tables_f <- tables[, c("Team","general_points")]
colnames(tables_f) <- c("Team","Points")

#knitr::kable(tables_f)

tables_f %>%
  kbl() %>%
  kable_paper(full_width = T) %>%
  column_spec(2, color = "white",
              background = spec_color(tables_f$Points[1:20], end = 0.7),
              popover = paste("am:", mtcars$am[1:8]))

# type of win ratio
df1$diff = df1$goal_home - df1$goal_away
df1 = df1 %>% mutate(result = ifelse(diff > 0, "Home Team win", ifelse(diff < 0, "Away Team win", "Draw")))

result = count(df1, result)


ggplot(result, aes(x = reorder(result, -n), y = n, fill = factor(n))) + 
  geom_bar(stat = "identity") +
  theme_minimal() + 
  labs(x = "", y = "Freq", title = "Type of victory") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

######### Analyses

#pn = 5 #predicted matches
g = nrow(df1)
#ngob = g-pn #number of games to fit

#stan_dat <- list(
#g = g, 
#teams = length(unique(df1$home_team)), #number of teams
#home_id = df1$home_id[1:g],
#away_id = df1$away_id[1:g],
#goal_home = df1$goal_home[1:g],
#goal_away = df1$goal_away[1:g]
#pn = pn, #games want to predict
#pred_home_id = df1$home_id[376:g],
#pred_away_id = df1$away_id[376:g]
#)



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


#####################################
# (simple) Poisson Regression Model #
#####################################

## compile the model saved in Simple_Poisson_Regression_model


comp_model_HP <- stan_model(file = "Hierarchical_Poisson_Regression_model_new.stan")
# Sampling (refresh = 0 avoid printing the trace)
fit_HP <- sampling(comp_model_HP, 
                   data = stan_dat_1, cores = 8, iter = 6000)

check_hmc_diagnostics(fit_HP)
traceplot(fit_HP,par=c("home"))


print(fit_HP, pars = c('home','att[1]','def[1]'))


nhparams = extract(fit_HP)

#scatterplot with attack e defense force
attack = colMeans(nhparams$att)
defense = colMeans(nhparams$def)
plot(attack,defense,xlim=c(-0.4,1.1))
abline(h=0)
abline(v=0)
teams = unique(df1$home_team)
text(attack,defense, labels=teams, cex=0.7, pos=4)

#Table attack
teams<-unique(df1$home_team)

fit.summary<-summary(fit_HP)
att_t<-summary(fit_HP,pars = "att", probs=c('0.025','0.975'))
att_t<-att_t$summary
att_t<-as.data.frame(att_t)

att_t<-att_t%>%mutate(team=teams)%>%relocate(team, .before = mean)
att_t<-att_t[1:6]
att_t <- att_t[order(-att_t$mean), ]
att_t

#Table defense
def_t<-summary(fit_HP,pars = "def", probs=c('0.025','0.975'))
def_t<-def_t$summary
def_t<-as.data.frame(def_t)

def_t<-def_t%>%mutate(team=teams)%>%relocate(team, .before = mean)
def_t<-def_t[1:6]
def_t

#Table home
home_t<-summary(fit_HP,pars = "home", probs=c('0.025','0.975'))
home_t<-home_t$summary
home_t<-as.data.frame(home_t)
home_t<-home_t[1:5]
home_t


#home goal and away goal simulation
goal_home_for <- as.matrix(fit_HP, pars="goal_home_pred")
goal_away_for <- as.matrix(fit_HP, pars="goal_away_pred")

goal_home_for = round(colMeans(goal_home_for), 0)
goal_away_for = round(colMeans(goal_away_for), 0)

goal_away_for
goal_home_for

#create simulated ds
season_sim_ds = data.frame(
g_sim = df1$g,
home_team_sim = df1$home_team,
away_team_sim = df1$away_team,
home_id_sim = df1$home_id,
away_id_sim = df1$away_id,
goal_home_sim = goal_home_for,
goal_away_sim = goal_away_for
)

standigs_away_sim = season_sim_ds %>%
  group_by(away_team_sim) %>%
  rename(team = away_team_sim) %>%
  summarise(
    pts_away = sum(goal_away_sim > goal_home_sim) * 3 + sum(goal_away_sim == goal_home_sim),
    away_wins = sum(goal_away_sim > goal_home_sim),
    away_ties = sum(goal_away_sim == goal_home_sim),
    away_losses = sum(goal_away_sim < goal_home_sim),
    away_scored = sum(goal_away_sim),
    away_conceded = sum(goal_home_sim),
    away_goal_diff = away_scored - away_conceded)

standigs_home_sim = season_sim_ds %>%
  group_by(home_team_sim) %>%
  rename(team = home_team_sim) %>%
  summarise(
    pts_home = sum(goal_home_sim > goal_away_sim) * 3 + sum(goal_home_sim == goal_away_sim),
    home_wins = sum(goal_home_sim > goal_away_sim),
    home_ties = sum(goal_home_sim == goal_away_sim),
    home_losses = sum(goal_home_sim < goal_away_sim),
    home_scored = sum(goal_home_sim),
    home_conceded = sum(goal_away_sim),
    home_goal_diff = home_scored - home_conceded)

standigs_sim = standigs_home_sim %>%
  left_join(standigs_away_sim, by="team") %>%
  group_by(team, .drop=FALSE) %>%
  summarise(
    Pt = pts_home + pts_away,
    V = away_wins + home_wins,
    P = away_ties + home_ties,
    S = away_losses + home_losses,
    GF = away_scored + home_scored,
    GS = away_conceded + home_conceded,
    DR = GF - GS
  )

standigs_sim <- standigs_sim %>%
  arrange(desc(Pt))

# Creiamo una tabella con i dati ordinati
standigs_sim_table <- kable(standigs_sim, format = "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
  kable_styling("striped", full_width = FALSE)

# Stampa la tabella
standigs_sim_table


###Negative Binomial

comp_model_NB <- stan_model(file = "Hierarchical_Negative_binomial_model_new.stan")
# Sampling (refresh = 0 avoid printing the trace)
fit_NB <- sampling(comp_model_NB, 
                   data = stan_dat_1, core = 8, iter = 6000)

check_hmc_diagnostics(fit_NB)


print(fit_NB, pars = c('home_att[1]','home_def[1]'))
traceplot(fit_NB,par=c('home_att[1]','home_def[1]'))


nb_params = extract(fit_NB)


#scatterplot with attack e defense force
attack_nb = colMeans(nb_params$home_att)
defense_nb = colMeans(nb_params$home_def)
plot(attack_nb,defense_nb,xlim=c(-0.4,1.1))
abline(h=0)
abline(v=0)
teams = unique(df1$home_team)
text(attack_nb,defense_nb, labels=teams, cex=0.7, pos=4)

data_graph <- data.frame(teams = unique(df1$home_team), attack_nb, defense_nb)


#Table attack
teams<-unique(df1$home_team)

fit.summary<-summary(fit_NB)
att_t_nb<-summary(fit_NB,pars = c("home_att"), probs=c('0.025','0.975'))
att_t_nb<-att_t_nb$summary
att_t_nb<-as.data.frame(att_t_nb)
att_t_nb<-att_t_nb%>%mutate(team=teams)%>%relocate(team, .before = mean)
att_t_nb<-att_t_nb[1:6]
att_t_nb <- att_t_nb[order(-att_t_nb$mean), ]
att_t_nb

att_HPG <- att_HPG[order(-att_HPG$mean), ]


#home goal and away goal simulation
goal_home_for_nb <- as.matrix(fit_NB, pars="goal_home_pred")
goal_away_for_nb <- as.matrix(fit_NB, pars="goal_away_pred")

goal_home_for_nb = round(colMeans(goal_home_for_nb), 0)
goal_away_for_nb = round(colMeans(goal_away_for_nb), 0)



#create simulated ds
season_sim_ds_nb = data.frame(
  g_sim = df1$g,
  home_team_sim = df1$home_team,
  away_team_sim = df1$away_team,
  home_id_sim = df1$home_id,
  away_id_sim = df1$away_id,
  goal_home_sim = goal_home_for_nb,
  goal_away_sim = goal_away_for_nb
)


standigs_away_sim_nb = season_sim_ds_nb %>%
  group_by(away_team_sim) %>%
  rename(team = away_team_sim) %>%
  summarise(
    pts_away = sum(goal_away_sim > goal_home_sim) * 3 + sum(goal_away_sim == goal_home_sim),
    away_wins = sum(goal_away_sim > goal_home_sim),
    away_ties = sum(goal_away_sim == goal_home_sim),
    away_losses = sum(goal_away_sim < goal_home_sim),
    away_scored = sum(goal_away_sim),
    away_conceded = sum(goal_home_sim),
    away_goal_diff = away_scored - away_conceded)

standigs_home_sim_nb = season_sim_ds_nb %>%
  group_by(home_team_sim) %>%
  rename(team = home_team_sim) %>%
  summarise(
    pts_home = sum(goal_home_sim > goal_away_sim) * 3 + sum(goal_home_sim == goal_away_sim),
    home_wins = sum(goal_home_sim > goal_away_sim),
    home_ties = sum(goal_home_sim == goal_away_sim),
    home_losses = sum(goal_home_sim < goal_away_sim),
    home_scored = sum(goal_home_sim),
    home_conceded = sum(goal_away_sim),
    home_goal_diff = home_scored - home_conceded)

standigs_sim_nb = standigs_home_sim_nb %>%
  left_join(standigs_away_sim_nb, by="team") %>%
  group_by(team, .drop=FALSE) %>%
  summarise(
    Pt = pts_home + pts_away,
    V = away_wins + home_wins,
    P = away_ties + home_ties,
    S = away_losses + home_losses,
    GF = away_scored + home_scored,
    GS = away_conceded + home_conceded,
    DR = GF - GS
  )


standigs_sim_nb <- standigs_home_sim_nb %>%
  left_join(standigs_away_sim_nb, by = "team") %>%
  group_by(team, .drop = FALSE) %>%
  summarise(
    Pt = pts_home + pts_away,
    V = away_wins + home_wins,
    P = away_ties + home_ties,
    S = away_losses + home_losses,
    GF = away_scored + home_scored,
    GS = away_conceded + home_conceded,
    DR = GF - GS
  )

# Ordina la tabella in base alla colonna Pt (punti totali) in ordine decrescente
standigs_sim_nb <- standigs_sim_nb %>%
  arrange(desc(Pt))

# Creiamo una tabella con i dati ordinati
standigs_sim_table <- kable(standigs_sim_nb, format = "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
  kable_styling("striped", full_width = FALSE)

# Stampa la tabella
standigs_sim_table


###Baio e Blangiardo second model

dir_alp_att = read.csv("/Users/lucap/OneDrive/Desktop/Bayesian/dir_alp_att.csv", sep = ';')
dir_alp_def = read.csv("/Users/lucap/OneDrive/Desktop/Bayesian/dir_alp_def.csv", sep = ';')

stan_dat_2 = list(
  g = g, 
  teams = length(unique(df1$home_team)), #number of teams
  home_id = df1$home_id,
  away_id = df1$away_id,
  goal_home = df1$goal_home,
  goal_away = df1$goal_away,
  dir_alp_att = dir_alp_att,
  dir_alp_def = dir_alp_def
)

comp_model_HPG <- stan_model(file = "Hierarchical_groups_Poisson_Regression_model.stan")
# Sampling (refresh = 0 avoid printing the trace)
fit_HPG <- sampling(comp_model_HPG, 
                   data = stan_dat_2, core = 8, iter = 4000)

check_hmc_diagnostics(fit_HPG)
pg_params = extract(fit_HPG)

traceplot(fit_HPG,par=c("home"))

print(fit_HPG, pars = c('home','att[1]','def[1]'))

#scatterplot with attack e defense force
attack = colMeans(pg_params$att)
defense = colMeans(pg_params$def)
plot(attack,defense,xlim=c(-0.4,1.1))
abline(h=0)
abline(v=0)
teams = unique(df1$home_team)
text(attack,defense, labels=teams, cex=0.7, pos=4)

#Table attack
teams<-unique(df1$home_team)

fit.summary<-summary(fit_HPG)
att_HPG <- summary(fit_HPG, pars = "att", probs = c('0.025', '0.975'))
att_HPG <- att_HPG$summary
att_HPG <- as.data.frame(att_HPG)

att_HPG <- att_HPG %>% mutate(team = teams) %>% relocate(team, .before = mean)
att_HPG <- att_HPG[1:6]

att_HPG <- att_HPG[order(-att_HPG$mean), ]

att_HPG

#home goal and away goal simulation
goal_home_for_hpg <- as.matrix(fit_HPG, pars="goal_home_pred")
goal_away_for_hpg <- as.matrix(fit_HPG, pars="goal_away_pred")

goal_home_for_hpg = round(colMeans(goal_home_for_hpg), 0)
goal_away_for_hpg = round(colMeans(goal_away_for_hpg), 0)



#create simulated ds
season_sim_ds_hpg = data.frame(
  g_sim = df1$g,
  home_team_sim = df1$home_team,
  away_team_sim = df1$away_team,
  home_id_sim = df1$home_id,
  away_id_sim = df1$away_id,
  goal_home_sim = goal_home_for_hpg,
  goal_away_sim = goal_away_for_hpg
)


standigs_away_sim_hpg = season_sim_ds_hpg %>%
  group_by(away_team_sim) %>%
  rename(team = away_team_sim) %>%
  summarise(
    pts_away = sum(goal_away_sim > goal_home_sim) * 3 + sum(goal_away_sim == goal_home_sim),
    away_wins = sum(goal_away_sim > goal_home_sim),
    away_ties = sum(goal_away_sim == goal_home_sim),
    away_losses = sum(goal_away_sim < goal_home_sim),
    away_scored = sum(goal_away_sim),
    away_conceded = sum(goal_home_sim),
    away_goal_diff = away_scored - away_conceded)

standigs_home_sim_hpg  = season_sim_ds_hpg  %>%
  group_by(home_team_sim) %>%
  rename(team = home_team_sim) %>%
  summarise(
    pts_home = sum(goal_home_sim > goal_away_sim) * 3 + sum(goal_home_sim == goal_away_sim),
    home_wins = sum(goal_home_sim > goal_away_sim),
    home_ties = sum(goal_home_sim == goal_away_sim),
    home_losses = sum(goal_home_sim < goal_away_sim),
    home_scored = sum(goal_home_sim),
    home_conceded = sum(goal_away_sim),
    home_goal_diff = home_scored - home_conceded)

standigs_sim_hpg  = standigs_home_sim_hpg  %>%
  left_join(standigs_away_sim_hpg , by="team") %>%
  group_by(team, .drop=FALSE) %>%
  summarise(
    Pt = pts_home + pts_away,
    V = away_wins + home_wins,
    P = away_ties + home_ties,
    S = away_losses + home_losses,
    GF = away_scored + home_scored,
    GS = away_conceded + home_conceded,
    DR = GF - GS
  )


# Ordina la tabella in base alla colonna Pt (punti totali) in ordine decrescente
standigs_sim_hpg <- standigs_sim_hpg %>%
  arrange(desc(Pt))

# Creiamo una tabella con i dati ordinati
standigs_sim_table_hpg  <- kable(standigs_sim_hpg , format = "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
  kable_styling("striped", full_width = FALSE)

# Stampa la tabella
standigs_sim_table_hpg

######comparing model
### Home
loglik1_home = as.matrix(fit_HPG, pars="log_lik_1")
loglik2_home = as.matrix(fit_HP, pars="log_lik_1")

# LOO
loo1_home = loo(loglik1_home)
loo2_home = loo(loglik2_home)

loo_diff_home = loo_compare(loo1_home, loo2_home)
print(loo_diff_home, simplify = FALSE)

# WAIC
waic1_home = waic(loglik1_home)
waic2_home = waic(loglik2_home)

waic_diff_home = loo_compare(waic1_home, waic2_home)
print(waic_diff_home, simplify = FALSE)

######AWAY

loglik1_away = as.matrix(fit_HPG, pars="log_lik_2")
loglik2_away = as.matrix(fit_HP, pars="log_lik_2")

# LOO
loo1_away = loo(loglik1_away)
loo2_away = loo(loglik2_away)

loo_diff_away = loo_compare(loo1_away, loo2_away)
print(loo_diff_away, simplify = FALSE)


# WAIC
waic1_away = waic(loglik1_away)
waic2_away = waic(loglik2_away)

waic_diff_away = loo_compare(waic1_away, waic2_away)
print(waic_diff_away, simplify = FALSE)

rm(model_1_fit, model_2_fit)
rm(loglik1_away, loglik2_away)

####ZIP

comp_model_ZIP<- stan_model(file = "ZIP_Regression_model.stan")
# Sampling (refresh = 0 avoid printing the trace)
fit_ZIP <- sampling(comp_model_ZIP, 
                   data = stan_dat_1, core = 8, iter = 4000)

check_hmc_diagnostics(fit_ZIP)
ZIP_params = extract(fit_ZIP)

traceplot(fit_ZIP,par=c("home"))

print(fit_ZIP, pars = c('home','att[1]','def[1]'))

#scatterplot with attack e defense force
attack = colMeans(ZIP_params$att)
defense = colMeans(ZIP_params$def)
plot(attack,defense,xlim=c(-0.4,1.1))
abline(h=0)
abline(v=0)
teams = unique(df1$home_team)
text(attack,defense, labels=teams, cex=0.7, pos=4)






