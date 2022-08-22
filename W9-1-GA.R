

################################################
## MLE for location Cauchy distribution using
## Gradient Ascent method
## We will plot the likelihood as well
## and the quadratic approximations
################################################
set.seed(1)
mu.star <- 5  # Setting true mu
n <- 4  # sample size
X <- rt(n, df = 1) + mu.star

## Function calculates  the log-likelihood
log.like <- function(mu, X)
{
  n <- length(X)
  rtn <- -n*log(pi) - sum( log(1 + (X - mu)^2) )
  return(rtn)
}

mu.x <- seq(-10, 40, length = 1e3)  # A sequence of mu's 
ll.est <- sapply(mu.x, log.like, X)  # evaluating log-likelihood at the mus
plot(mu.x, ll.est, type = 'l', ylab = "log-likelihood", xlab = expression(mu))  # plotting log-likelihood. Not concave, so we need to choose good starting values.


## Starting Gradient-Ascent method
tol <- 1e-5  # tolerance level for when to stop algorithm


## Returns derivate of log-likelihood
f.prime <- function(X, mu)
{
  rtn <- sum(2* (X - mu)/(1 + (X-mu)^2))  #f.prime
  return(rtn)
}

# Returns double derivative of log-likelihood.
f.double.prime <- function(X, mu)
{
  rtn <- sum( 2 * ( 2*(X-mu)^2/ (1 + (X - mu)^2)^2  - (1 + (X-mu)^2)^(-1) )  )
  return(rtn)
}

## Loop below stops when |mu_(k+1) - mu_(k)| < tol
t <- .3  # change this to 1 and see what happens to the "bad" starting value


# Plotting the log-likelihood
plot(mu.x, ll.est, type = 'l', ylab = "log-likelihood", xlab = expression(mu))


## Bad  starting values

current <- 7  # Bad starting value
diff <- 100
iter <- 0
mu.k <- current
## Loop below stops when |mu_(k+1) - mu_(k)| < tol
while( (diff > tol) && iter < 1000)
{
  iter <- iter + 1
  update <- current + t*f.prime(X, current) 
  mu.k <- c(mu.k, update)
  diff <- abs(current - update)
  current <- update
}
current  # final approximation to MLE
evals <- sapply(mu.k, log.like, X)


for(l in 1:min(5,iter))
{
  points(mu.k[l], evals[l], pch = 16, col = rgb(1,0,0, alpha = .5))
  foo <- mu.k[l]
  approx <- log.like(foo, X) + f.prime(X, foo)*(mu.x - foo) -(mu.x - foo)^2/(2*t)
  lines(mu.x, approx, col = rgb(1,0,0, alpha = .5))
  Sys.sleep(2)
}
points(mu.k, evals, pch = 16, col = rgb(1,0,0, alpha = .5))




## Best starting values

current <- median(X)  # Good starting value
diff <- 100  # inital large value for difference
iter <- 0    # counting the number of iterations

mu.k <- current
while( (diff > tol) && iter < 1000)
{
  iter <- iter + 1
  update <- current + t*f.prime(X, current)  # GD update
  mu.k <- c(mu.k, update)
  diff <- abs(current - update)
  current <- update
}
current  # final approximation to MLE
evals <- sapply(mu.k, log.like, X)
for(l in 1:min(5, iter))
{
  points(mu.k[l], evals[l], pch = 16, col = rgb(0,0,1, alpha = .5))
  foo <- mu.k[l]
  approx <- log.like(foo, X) + f.prime(X, foo)*(mu.x - foo) -(mu.x - foo)^2/(2*t)
  lines(mu.x, approx, col = rgb(0,0,1, alpha = .5))
  Sys.sleep(2)
}
points(mu.k, evals, pch = 16, col = rgb(0,0,1, alpha = .5))



## Worst starting values

current <- 19  # Worst starting value
diff <- 100
iter <- 0
mu.k <- current
while( (diff > tol) && iter < 1000)
{
  iter <- iter + 1
  update <- current + t*f.prime(X, current) 
  mu.k <- c(mu.k, update)
  diff <- abs(current - update)
  current <- update
}
current  # final approximation to MLE
evals <- sapply(mu.k, log.like, X)
for(l in 1:iter)
{
  points(mu.k[l], evals[l], pch = 16, col = rgb(.2,.7,.1, alpha = .5))
  foo <- mu.k[l]
  approx <- log.like(foo, X) + f.prime(X, foo)*(mu.x - foo) -(mu.x - foo)^2/(2*t)
  lines(mu.x, approx, col = rgb(.2,.7,.1, alpha = .5))
  Sys.sleep(2)
}

points(mu.k, evals, pch = 16, col = rgb(.2,.7,.1, alpha = .5))
legend("topright", legend = c("Good starting", "Bad starting", "Horrible starting"), pch = 16, col = c("blue", "red", rgb(.2,.7,.1)))




################################################
## MLE for logistic regression
## Using gradient ascent
################################################
library(mcmc) #to load a dataset
data(logit)
head(logit)  # y is response and 4 covariates
y <- logit$y
X <- as.matrix(logit[, 2:5])
p <- dim(X)[2]

f.gradient <- function(y, X, beta)
{
  beta <- matrix(beta, ncol = 1)
  pi <- exp(X %*% beta) / (1 + exp(X%*%beta))  
  rtn <- colSums(X* as.numeric(y - pi))
  return(rtn)
}


store.beta <- matrix(0, nrow = 1, ncol = p)
store.grads <- NULL
beta_k <- rep(0, p) # start at all 0s
grads <- 100 # large values
t <- .1
tol <- 1e-8
iter <- 0
while((grads > tol) && iter < 1e4)  #not too many iterations
{
  iter <- iter+1
  old <- beta_k
  foo <- f.gradient(y = y, X= X, beta = old)
  grads <- sqrt(sum(foo^2))
  store.grads <- c(store.grads, grads)  ## storing the gradients
  beta_k = old + t* foo
  store.beta <- rbind(store.beta, beta_k)
}
iter # number of iterations
beta_k # last estimate

plot(store.grads, type= "b", pch = 16, ylab = "Norm of gradient")
abline(h = 0, col = "red")


