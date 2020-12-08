data {
  int N; // number of non-missing observation lines
  int M; // number of agencies
  int J; // number of years
  real y[N]; // indicators
  int ind_justice[N]; // agencies
  int ind_case[N]; // years
}
parameters {
  vector[M] x; // ideal points
  vector[J] alpha;
  vector[J] beta;
  real<lower=0> alpha_sd;
  real<lower=0> beta_sd;
  real<lower=0> sigma;
}
model {
  for (i in 1:N)
    y[i] ~ normal(alpha[ind_case[i]]+x[ ind_justice[i] ]*beta[ ind_case[i] ], sigma);
  x ~ normal(0, 1);
  alpha ~ normal(0, alpha_sd);
  beta ~ normal(0, beta_sd);
  alpha_sd ~ cauchy(0, 1);
  beta_sd ~ cauchy(0, 1);
}
generated quantities {
  vector[M] ideal = (x - mean(x)) / sd(x);
}