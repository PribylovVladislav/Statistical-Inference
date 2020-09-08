# Part 1: Simulation Exercise Instructions

lambda <- 0.2
mns <- NULL
vars <- NULL

for (i in 1 : 1000) {
    distributions <- rexp(40, lambda)
    mns <- c(mns, mean(distributions))
    vars <- c(vars, var(distributions))
}

experimental_mean <- mean(mns)
experimental_variance <- mean(vars)

theoretical_variance <- 1/lambda**2
theoretical_mean <- 1/lambda

difference_mean <- theoretical_mean - experimental_mean
difference_variance <- theoretical_variance - experimental_variance

mean_confidence_interval <- experimental_mean + c(-1, 1)*qnorm(.975)*sqrt(theoretical_variance/1000)
variance_confidence_interval <- experimental_variance + c(-1, 1)*qnorm(.975)*sqrt(theoretical_variance/1000)

hist(mns, xlab = 'Means', main = 'Histogram of means')