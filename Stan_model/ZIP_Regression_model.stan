// Hierarchical_Zero_Inflated_Poisson_Regression_model

data {
  int<lower=1> g; // number of games
  int<lower=2> teams; // number of teams
  
  int<lower=0> home_id[g]; // home team index
  int<lower=0> away_id[g]; // away team index
  
  int<lower=0> goal_home[g]; // goals home team
  int<lower=0> goal_away[g]; // goals away team
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
  vector[teams] att;
  vector[teams] def;
  
  att = att_free - mean(att_free);
  def = def_free - mean(def_free);
}

model {
  // hyper priors
  mu_att ~ normal(0, 10000);
  mu_def ~ normal(0, 10000);
  sigma_att ~ inv_gamma(0.1, 0.1);
  sigma_def ~ inv_gamma(0.1, 0.1);

  home ~ normal(0, 10000);

  for (t in 1:teams) {
    att_free[t] ~ normal(mu_att, sigma_att);
    def_free[t] ~ normal(mu_def, sigma_def);
  }

  for (i in 1:g) {
    // Zero-Inflated Poisson for goal_home
    if (goal_home[i] == 0)
      target += log_sum_exp(log(0.05) + bernoulli_lpmf(1 | 0.5), 
                            bernoulli_lpmf(0 | 0.5) + poisson_log_lpmf(0 | home + att[home_id[i]] + def[away_id[i]]));
    else
      target += bernoulli_lpmf(0 | 0.5) + poisson_log_lpmf(goal_home[i] | home + att[home_id[i]] + def[away_id[i]]);

    // Zero-Inflated Poisson for goal_away
    if (goal_away[i] == 0)
      target += log_sum_exp(log(0.05) + bernoulli_lpmf(1 | 0.5), 
                            bernoulli_lpmf(0 | 0.5) + poisson_log_lpmf(0 | att[away_id[i]] + def[home_id[i]]));
    else
      target += bernoulli_lpmf(0 | 0.5) + poisson_log_lpmf(goal_away[i] | att[away_id[i]] + def[home_id[i]]);
  }
}





