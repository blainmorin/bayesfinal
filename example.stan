data {
  int N; // number of non-missing votes
  int M; // number of voters/justices
  int J; // number of cases/bills
  int y[N]; // votes
  int ind_justice[N];
  int ind_case[N];
}
parameters {
  vector[M] x; // ideal points
  vector[J] alpha;
  vector[J] beta;
  real<lower=0> alpha_sd;
  real<lower=0> beta_sd;
}
model {
  for (i in 1:N)
    y[i] ~ bernoulli_logit(alpha[ind_case[i]]+x[ ind_justice[i] ]*beta[ ind_case[i] ]);
  x ~ normal(0, 1);
  alpha ~ normal(0, alpha_sd);
  beta ~ normal(0, beta_sd);
  alpha_sd ~ cauchy(0, 1);
  beta_sd ~ cauchy(0, 1);
}
generated quantities {
  vector[M] ideal = (x - mean(x)) / sd(x);
  if (x[8] > x[4])
    ideal = -ideal;
}