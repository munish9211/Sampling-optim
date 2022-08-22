################################################
## EM Algorithm for the Old Faithful Geyser data
################################################

data(faithful)
head(faithful)

x <- faithful$eruptions
hist(x, freq = FALSE, breaks = 30, main = "Eruptions")

# (pi_1, mu_1, mu_2, sigma^2_1, sigma^2_2)
theta <- c(.6, 5,1, 1, 1) # starting value
current <- theta
diff <- 100
tol <- 1e-5
iter <- 0
store <- current


## Estep gamma calculation
gamma_ick <- function(x, theta, C = 2)
{
  pis <- c(theta[1:(C-1)], 1- sum(theta[1:(C-1)]))
  mu <- theta[C:(2*C-1)]
  sig2 <- tail(theta, C)
  
  gamma_prob <- matrix(0, nrow = length(x), ncol = C)
  for(c in 1:C)
  {
    gamma_prob[ ,c]  <- dnorm(x, mean = mu[c], sd = sqrt(sig2[c]))* pis[c]
  }
  
  gamma_prob <- gamma_prob/(rowSums(gamma_prob))
  return(gamma_prob[,-C])
}


while(diff > tol)
{
  iter <- iter + 1
  
  # E step: find gamma_{i,c,k} for just c = 1, since for c = 2 is just 1-Ep
  Ep <- gamma_ick(x, theta = current)
  
  # M-step
  theta[1] <- mean(Ep)
  theta[2] <- sum(Ep*x) / sum(Ep)
  theta[3] <- sum((1-Ep)*x) / sum(1-Ep)
  theta[4] <- sum(Ep*(x - theta[2])^2) / sum(Ep)
  theta[5] <- sum((1-Ep)*(x - theta[3])^2) / sum(1-Ep)
  
  diff <- sqrt(sum( (theta - current)^2))
  current <- theta
  store <- rbind(store, theta)
}
current

# Final estimates of the probability
# that each observation is in Class C.
Prob.Z <- gamma_ick(x, theta = current, C = 2)
head(round(Prob.Z, 4))

# Make plot of iterative model fits
for(i in 1:dim(store)[1])
{
  test.x <- seq(min(x), max(x), length = 1000)
  test.y <- store[i,1]* dnorm(test.x, mean = store[i,2], sd = sqrt(store[i,4])) + (1-store[i,1]) *dnorm(test.x, mean = store[i,3], sd = sqrt(store[i,5]))
  Sys.sleep(.1)
  lines(test.x, test.y, col = rgb(1,0,0, alpha = .5))
}
lines(test.x, test.y, col = rgb(0,0,1, alpha = 1), lwd = 2)

# add color
color <- 1*(Ep < .5) + 3*(Ep >= .5)
points(x, rep(0, length(x)), pch = 16, col = color)




################################################
## Doing this again on the Waiting times

x <- faithful$waiting

# (pi_1, mu_1, mu_2, sigma^2_1, sigma^2_2)
theta <- c(.6, 30,100, 1, 1)
current <- theta
diff <- 100
tol <- 1e-5
iter <- 0
store <- current

while(diff > tol)
{
  iter <- iter + 1
  
  # E step: find gamma_{i,c,k} for just c = 1, since for c = 2 is just 1-Ep
  Ep <- gamma_ick(x, theta = current, C = 2)
  
  # M-step
  theta[1] <- mean(Ep)
  theta[2] <- sum(Ep*x) / sum(Ep)
  theta[3] <- sum((1-Ep)*x) / sum(1-Ep)
  theta[4] <- sum(Ep*(x - theta[2])^2) / sum(Ep)
  theta[5] <- sum((1-Ep)*(x - theta[3])^2) / sum(1-Ep)
  
  diff <- max( abs(theta - current))
  current <- theta
  store <- rbind(store, theta)
}
current

#hist(x, freq = FALSE, breaks = 30, main = "Wiating times") # a little bit of asymmetry in the first eruptions, so maybe Gaussian isn't the right model
for(i in 1:dim(store)[1])
{
  test.x <- seq(min(x), max(x), length = 1000)
  test.y <- store[i,1]* dnorm(test.x, mean = store[i,2], sd = sqrt(store[i,4])) + (1-store[i,1]) *dnorm(test.x, mean = store[i,3], sd = sqrt(store[i,5]))
  lines(test.x, test.y, col = rgb(1,0,0, alpha = .5))
}
lines(test.x, test.y, col = rgb(0,0,1, alpha = 1))

