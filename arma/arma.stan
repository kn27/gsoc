transformed data{
  int T_sim = 200;
  real mu_sim = 3.94; //normal_rng(0,2);
  real phi_sim = 0.42; //normal_rng(0,2);
  real theta_sim = 0.01; //normal_rng(0,2);
  real<lower = 0> sigma_sim = 0.2; //inv_gamma_rng(5,1);
  
  // real mu_sim = normal_rng(0,2);
  // real <lower = 0, upper = 1> phi_sim = uniform_rng(0,1);
  // real <lower = 0, upper = 1> theta_sim = uniform_rng(0,1);
  // real<lower = 0> sigma_sim = inv_gamma_rng(5,1);
  
  real nu_sim[T_sim];
  real err_sim[T_sim];
  real y_sim[T_sim];
  
  for (t in 1:T_sim){
    err_sim[t] = normal_rng(0, sigma_sim);  
  }
  nu_sim[1] = mu_sim + phi_sim * mu_sim;
  y_sim[1] = nu_sim[1] + err_sim[1];
  for (t in 2:T_sim) {
     nu_sim[t] = mu_sim + phi_sim * y_sim[t - 1] + theta_sim * err_sim[t - 1];
     y_sim[t] = nu_sim[t] + err_sim[t];
  }
}

parameters {
  real mu;             // mean coefficient
  real phi;            // autoregression coefficient
  real theta;          // moving average coefficient
  real<lower=0> sigma; // noise scale
}

model {
  vector[T_sim] nu;  // prediction for time t
  vector[T_sim] err; // error for time t

  mu ~ normal(0, 2);
  phi ~ inv_gamma(5, 1);
  theta ~ normal(0, 2);
  sigma ~ inv_gamma(5,1); //cauchy(0, 2.5);

  nu[1] = mu + phi * mu; // assume err[0] == 0
  err[1] = y_sim[1] - nu[1];
  for (t in 2:T_sim) {
    nu[t] = mu + phi * y_sim[t - 1] + theta * err[t - 1];
    err[t] = y_sim[t] - nu[t];
  }
  err ~ normal(0, sigma);
}

generated quantities{
  real mu_hat = mu_sim;
  real phi_hat = phi_sim;
  real theta_hat = theta_sim;
  real sigma_hat = sigma_sim;
}
