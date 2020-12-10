data {
  int N; // number of non-missing observation lines
  int M; // number of agencies
  int I; // number of indicators
  int Time; // number of years
  real y[N]; // indicators
  int agency[N]; // agencies
  int year[N]; // years
}
parameters {
  vector[M] x; // ideal points
  vector[Time] alpha;
  vector[Time] beta;
  real<lower=0> alpha_sd;
  real<lower=0> beta_sd;
  real<lower=0> sigma;
}
model {
  for (n in 1:N) {
    for (i in 1:I){
      
      y[n] ~ normal(alpha[year[n]]+x[agency[n]]*beta[ year[n]], sigma);
    }
  }
  x ~ normal(0, 1);
  alpha ~ normal(0, alpha_sd);
  beta ~ normal(0, beta_sd);
  alpha_sd ~ cauchy(0, 1);
  beta_sd ~ cauchy(0, 1);
}
generated quantities {
  vector[M] capacity = (x - mean(x)) / sd(x);
  if (x[1] > x[2])
    capacity = -capacity;
}