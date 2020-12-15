M <- 10 # number of agencies
Time <- 10 # number of years
J <- 5 # number of measurements
change_sd <- 1
alpha <- rnorm(J)
beta <- rexp(J)
sigma <- sqrt(rexp(J))
x <- t(replicate(M, cumsum(rnorm(Time)))) # true values of latent variables
# fill observations
z <- array(0, dim=c(M,Time,J))
for (i in 1:M)
    z[i,,] <- t(alpha + (beta %o% x[i,]) + matrix(rnorm(J * Time, 0, sigma), J))
sdata <- list(M=M, Time=Time, J=J, z=z, start_time=rep(1, M), end_time=rep(Time,M), which_pos = 1)

library(rstan)
sx <- stan("kalman.stan", data=sdata, iter=2000, thin=10, chains=4)
x.samples <- extract(sx, "x_samples")$x_samples
x.mean <- apply(x.samples, 2:3, mean)

check = summary(sx)$summary
