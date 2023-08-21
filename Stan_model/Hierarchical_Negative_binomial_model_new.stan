data {
  int teams;
  int g;
  int home_id[g];
  int away_id[g];
  
  int<lower=0> goal_home[g];
  int<lower=0> goal_away[g];
}
parameters {
  real mu_home_att;
  real mu_away_att;
  real mu_home_def;
  real mu_away_def;
  
  real<lower=0> sigma_att;
  real<lower=0> sigma_def;
  real<lower=0> phi_home;
  real<lower=0> phi_away;
  
  vector[teams-1] home_att_raw;
  vector[teams-1] away_att_raw;
  vector[teams-1] home_def_raw;
  vector[teams-1] away_def_raw;
}
transformed parameters {
  
  vector[g] log_mu_home;
  vector[g] log_mu_away;
  vector[teams] home_att;
  vector[teams] away_att;
  vector[teams] home_def;
  vector[teams] away_def;
  
  // need to make sum(att)=sum(def)=0 
  for (k in 1:(teams-1)) {
    home_att[k] = home_att_raw[k];
    away_att[k] = away_att_raw[k];
    home_def[k] = home_def_raw[k];
    away_def[k] = away_att_raw[k];
  }
  
  home_att[teams] = -sum(home_att_raw);
  away_att[teams] = -sum(away_att_raw);
  home_def[teams] = -sum(home_def_raw);
  away_def[teams] = -sum(away_def_raw);
  
  // getting mu in log form 
  log_mu_home = home_att[home_id] + away_def[away_id];
  log_mu_away = away_att[away_id] + home_def[home_id]; 
}

model {
  
  mu_home_att ~ normal(0.2, 10);
  mu_away_att ~ normal(0, 10);
  mu_home_def ~ normal(0, 10);
  mu_away_def ~ normal(0, 10);
  sigma_att ~ gamma(10, 10);
  sigma_def ~ gamma(10, 10);
  phi_home ~ normal(10, 10);
  phi_away ~ normal(10, 10);
  
  home_att_raw ~ normal(mu_home_att, sigma_att);
  away_att_raw ~ normal(mu_away_att, sigma_att);
  home_def_raw ~ normal(mu_home_def, sigma_def);
  away_def_raw ~ normal(mu_away_def, sigma_def);
  

  goal_home ~ neg_binomial_2_log(log_mu_home, phi_home); 
  goal_away ~ neg_binomial_2_log(log_mu_away, phi_away);
  
}

generated quantities {
  int<lower=0> goal_home_pred[g]; 
  int<lower=0> goal_away_pred[g]; 
  
  //vector[g] log_lik_1;
  //vector[g] log_lik_2;
  
 // for (i in 1:g) {
    //log_lik_1[i] = poisson_log_lpmf(goal_home[i] |home + att[home_id[i]] + def[away_id[i]]);
    //goal_home_pred = neg_binomial_2_log_rng(log_mu_home[home_id[i]], phi_home[home_id[i]]);
    //neg_binomial_2_log_lpmf
    goal_home_pred = neg_binomial_2_log_rng(log_mu_home, phi_home);
      
  //goal_away ~ neg_binomial_2_log(log_mu_away, phi_away);
    //log_lik_2[i] = poisson_log_lpmf(goal_away[i] |att[away_id[i]] + def[home_id[i]]);
    //goal_away_pred[i] = neg_binomial_2_log_rng(log_mu_away[away_id[i]], phi_away[away_id[i]]);
    goal_away_pred = neg_binomial_2_log_rng(log_mu_away, phi_away);
  }
