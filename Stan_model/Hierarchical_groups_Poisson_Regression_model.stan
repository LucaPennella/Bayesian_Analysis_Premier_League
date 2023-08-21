// Hierarchical_groups_Poisson_Regression_model
data {
  int<lower=1> g;
  int<lower=2> teams;
  
  int<lower=0> home_id[g]; // home team index
  int<lower=0> away_id[g]; // away team index
  
  int<lower=0> goal_home[g]; // goals home team
  int<lower=0> goal_away[g]; // goals away team
  
  vector[3] dir_alp_att[teams];
  vector[3] dir_alp_def[teams];
}

parameters {
  real home;
  
  simplex[3] pi_attack[teams];
  simplex[3] pi_defense[teams];
  
  real<lower=-3, upper=0> mu_att_bottom;
  real mu_att_middle;
  real<lower=0, upper=3> mu_att_top;
  
  real<lower=0, upper=3> mu_def_bottom;
  real mu_def_middle;
  real<lower=-3, upper=0> mu_def_top;
  
  vector[teams] att_free;
  vector[teams] def_free;
  
  real<lower=0> sigma_att[3];
  real<lower=0> sigma_def[3];
}

transformed parameters {
  vector[teams] att;
  vector[teams] def;
  
  
  att = att_free - mean(att_free);
  def = def_free - mean(def_free);
}

model {
  vector[3] mix_attack;
  vector[3] mix_defense;
  
  home ~ normal(0, 10000);
  
  // bottom teams
  sigma_att[1] ~ inv_gamma(0.1,0.1);
  sigma_def[1] ~ inv_gamma(0.1,0.1);
  mu_att_bottom ~ normal(0,10000);
  mu_def_bottom ~ normal(0,10000);
  
  // middle teams
  sigma_att[2] ~ inv_gamma(0.1,0.1);
  sigma_def[2] ~ inv_gamma(0.1,0.1);
  mu_att_middle ~ normal(0,10000);
  mu_def_middle ~ normal(0,10000);
  
  // top teams
  sigma_att[3] ~ inv_gamma(0.1,0.1);
  sigma_def[3] ~ inv_gamma(0.1,0.1);
  mu_att_top ~ normal(0,10000);
  mu_def_top ~ normal(0,10000);
  
  
  for (t in 1:teams) {
    pi_attack[t,] ~ dirichlet(dir_alp_att[t]);
    pi_defense[t,] ~ dirichlet(dir_alp_def[t]);
    
    mix_attack[1] = log(pi_attack[t,1]) + student_t_lpdf(att[t] | 4, mu_att_bottom, sigma_att[1]); 
    mix_defense[1] = log(pi_defense[t,1]) + student_t_lpdf(def[t] | 4, mu_def_bottom, sigma_def[1]);
    
    mix_attack[2] = log(pi_attack[t,2]) + student_t_lpdf(att[t] | 4, mu_att_middle, sigma_att[2]); 
    mix_defense[2] = log(pi_defense[t,2]) + student_t_lpdf(def[t] | 4, mu_def_middle, sigma_def[2]);
    
    mix_attack[3] = log(pi_attack[t,3]) + student_t_lpdf(att[t] | 4, mu_att_top, sigma_att[3]); 
    mix_defense[3] = log(pi_defense[t,3]) + student_t_lpdf(def[t] | 4, mu_def_top, sigma_def[3]);
    
    target += log_sum_exp(mix_attack);
    target += log_sum_exp(mix_defense);
  }
  
  for (i in 1:g) {
    goal_home[i] ~ poisson_log(home + att[home_id[i]] + def[away_id[i]]);
    goal_away[i] ~ poisson_log(att[away_id[i]] + def[away_id[i]]);
  }
}

generated quantities {
  int<lower=0> goal_home_pred[g];
  int<lower=0> goal_away_pred[g];
  
  vector[g] log_lik_1;
  vector[g] log_lik_2;
  
  for (i in 1:g) {
    log_lik_1[i] = poisson_log_lpmf(goal_home[i] |home + att[home_id[i]] + def[away_id[i]]);
    goal_home_pred[i] = poisson_log_rng(home + att[home_id[i]] + def[away_id[i]]);
    
    log_lik_2[i] = poisson_log_lpmf(goal_away[i] |att[away_id[i]] + def[home_id[i]]);
    goal_away_pred[i] = poisson_log_rng(att[away_id[i]] + def[home_id[i]]);
  }
}

