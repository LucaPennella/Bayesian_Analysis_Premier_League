// Hierarchical_Poisson_Regression_model_new

data {
  int<lower=1> g; // number of games
  int<lower=2> teams; //number of teams
  
  int<lower=0> home_id[g]; // home team index
  int<lower=0> away_id[g]; // away team index
  
  int<lower=0> goal_home[g]; // goals home team
  int<lower=0> goal_away[g]; // goals away team
  
  //int<lower=0> pn; //predicted games
  //int<lower=0> pred_home_id[pn];
  //int<lower=0> pred_away_id[pn]; 
  
}


parameters {
  real home;
  vector[teams] att_free;
  vector[teams] def_free;
  
  real mu_att;
  real<lower=0> sigma_att;
  real mu_def;
  real<lower=0> sigma_def;
}

transformed parameters {
  //vector[g] theta1;
  //vector[g] theta2;
  
  vector[teams] att;
  vector[teams] def;
  
  //theta1 = exp(home + att[home_id] - def[away_id] );
  //theta2 = exp( att[away_id] - def[home_id] );
  att = att_free - mean(att_free);
  def = def_free - mean(def_free);
}


model {
//hyper priors
mu_att ~ normal(0,10000);
mu_def ~ normal(0,10000);

sigma_att ~ inv_gamma(0.1,0.1);
sigma_def ~ inv_gamma(0.1,0.1);

//priors

  home ~ normal(0,10000);

  for (t in 1:teams) {
    att_free[t] ~ normal(mu_att, sigma_att);
    def_free[t] ~ normal(mu_def, sigma_def);
  }
  
  for (i in 1:g) {
    goal_home[i] ~ poisson_log(home + att[home_id[i]] + def[away_id[i]]);
    goal_away[i] ~ poisson_log(att[away_id[i]] + def[home_id[i]]);
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

