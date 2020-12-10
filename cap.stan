data {
  int N; // number of non-missing observation lines
  int M; // number of agencies
  int Time; // number of years
  int J; // number of indicators
  matrix[N, J] y; // indicators
  int agency[N]; // agencies
  int year[N]; // years
}
parameters {
  matrix[M,Time] x; // capacity estimates
  vector[J] alpha;
  vector[J] beta;
  real<lower=0> alpha_sd;
  real<lower=0> beta_sd;
  vector<lower=0>[J] sigma;
}
model {
  
  for (j in 1:J){
    for (i in 1:N){
      y[i, j] ~ normal(alpha[j] +  x[ agency[i], year[i] ] * beta[j], sigma[j]);
    }
    beta[j] ~ normal(0, beta_sd);
    beta_sd ~ cauchy(0, 1);
    alpha[j] ~ normal(0, alpha_sd);
    alpha_sd ~ cauchy(0, 1);
  }
  
  
  x ~ normal(0, 1);
  

  
}
generated quantities {
  
  
  matrix[M,Time] capacity = (x - rep_matrix(mean(x), M, Time)) / sd(x);
  
}