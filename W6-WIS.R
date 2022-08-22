##################################################
### xy exp( sin (xy) )
##################################################

set.seed(1)
N <- 1e5

wis <- function(N = 1e4)
{
  samp <- runif(2*N, min = 0, max = pi)
  x <- head(samp, N)
  y <- tail(samp, N)
  weights <- exp(sin(x*y))
  
  return(list(weights = weights, samples = cbind(x,y)))
}

output <- wis(N = N)
est <- mean( output$samples[,1]* output$samples[,2] * output$weights) / mean(output$weights)
est


##################################################
### Demonstrating convergence
##################################################
reps <- 5e2
N <- 1e3
ests <- numeric(length = reps)


plot(1:N, rep(est, N), type = "n", ylim = c(0, 8), ylab = "Running Estimates", xlab = "Sample Size")
for(r in 1:reps)
{
  output <- wis(N = N)
  cumsum.num <- cumsum(output$samples[,1]*output$samples[,2]* output$weights)
  cumsum.den <- cumsum(output$weights)
  lines(1:N, cumsum.num/cumsum.den, col = "red")
}


## Comparing with simple importance sampling

for(r in 1:reps)
{
  samp <- runif(2*N, min = 0, max = pi)
  x <- head(samp, N)
  y <- tail(samp, N)
  fn.value <- pi^2* x*y*exp(sin(x*y))/15.5092
  # only numerator here multiplied with a*b
  cumsum.num <- cumsum(fn.value)
  lines(1:N, cumsum.num/(1:N), col = "blue")
}
abline(h = 2.15356, col = "black")


#Simple importance sampling is maybe slightly better!
