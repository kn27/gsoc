functions {
    // This function exists because rstan doesn't have the linspaced_vector function yet, and ^ does not take vector input ...
    vector linspaced_vector_(int N, real start, real end){
      real h;
      vector[N] out;
      h = (end-start)/N;
      for (i in 1:N){
        out[i]= start + (i-1)*h;
      }
      return out;
    }vector linspaced_vector_square(int N, real start, real end){
      real h;
      vector[N] out;
      h = (end-start)/N;
      for (i in 1:N){
        out[i]= (start + (i-1)*h)^2;
      }
      return out;
    }
    vector modified_bessel_first_kind_(int len, int[] v, real z){
      //int len;
      //len = size(v);
      real output[len];
      for (i in 1:len){
        output[i] = modified_bessel_first_kind(v[i], z);
      }
      return to_vector(output);
    }

    vector diagSPD_EQ(real alpha, real rho, real L, int M) {
      return sqrt((alpha^2) * sqrt(2*pi()) * rho * exp(-0.5*(rho*pi()/2/L)^2 * linspaced_vector_square(M, 1, M)));
    }
    /* real spd_Matt(real alpha, real rho, real w) { */
    /*   real S = 4*alpha^2 * (sqrt(3)/rho)^3 * 1/((sqrt(3)/rho)^2 + w^2)^2; */
    /*   return sqrt(S); */
    /* } */
    vector diagSPD_periodic(real alpha, real rho, int M) {
      vector[M] q;
      real a = 1/rho^2;
      int one_to_M[M];
      for (m in 1:M) one_to_M[m] = m;
      q = sqrt(alpha^2 * 2 / exp(a) * to_vector(modified_bessel_first_kind_(size(one_to_M),one_to_M, a)));
      return append_row(q,q);
    }
    matrix PHI_EQ(int N, int M, real L, vector x) {
      return sin(diag_post_multiply(rep_matrix(pi()/(2*L) * (x+L), M), linspaced_vector_(M, 1, M)))/sqrt(L);
    }
    matrix PHI_periodic(int N, int M, real w0, vector x) {
      matrix[N,M] mw0x = diag_post_multiply(rep_matrix(w0*x, M), linspaced_vector_(M, 1, M));
      return append_col(cos(mw0x), sin(mw0x));
    }
}

data {
  int<lower=1> N;      // number of observations
  vector[N] x;         // univariate covariate
  vector[N] y;         // target variable

  real<lower=0> c_f1;  // factor c to determine the boundary value L
  int<lower=1> M_f1;   // number of basis functions for smooth function
}
transformed data {
  // Normalize data
  real xmean = mean(x);
  real ymean = mean(y);
  real xsd = sd(x);
  real ysd = sd(y);
  vector[N] xn = (x - xmean)/xsd;
  vector[N] yn = (y - ymean)/ysd;
  // Basis functions for f1
  real L_f1 = c_f1*max(xn);
  matrix[N,M_f1] PHI_f1 = PHI_EQ(N, M_f1, L_f1, xn);
}
parameters {
  real intercept;               //
  vector[M_f1] beta_f1;         // the basis functions coefficients
  real<lower=0> lengthscale_f1; // lengthscale of f1
  real<lower=0> sigma_f1;       // scale of f1
  real<lower=0> sigma;          // residual scale
}
model {
  // spectral densities for f1
  vector[M_f1] diagSPD_f1 = diagSPD_EQ(sigma_f1, lengthscale_f1, L_f1, M_f1);
  // priors
  intercept ~ normal(0, 1);
  beta_f1 ~ normal(0, 1);
  lengthscale_f1 ~ lognormal(log(700/xsd), 1);
  sigma_f1 ~ normal(0, .5);
  sigma ~ normal(0, .5);
  // model
  yn ~ normal_id_glm(PHI_f1, intercept, diagSPD_f1 .* beta_f1, sigma);
}
generated quantities {
  vector[N] f;
  vector[N] log_lik;
  {
    // spectral densities for f1
    vector[M_f1] diagSPD_f1 = diagSPD_EQ(sigma_f1, lengthscale_f1, L_f1, M_f1);
    // function scaled back to original scale
    f = (intercept + PHI_f1 * (diagSPD_f1 .* beta_f1))*ysd + ymean;
    // log_liks for loo
    for (n in 1:N) log_lik[n] = normal_lpdf(y[n] | f[n], sigma*ysd);
  }
}
