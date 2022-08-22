set.seed(10)

##############################################
#### RoU region for N(theta, s2)
##############################################

theta <- 5
s2 <- 1

a <- 1/(2*pi*s2)^(.25)
foo <- (theta - sqrt(theta^2 + 8*s2))/(2*s2)
b <- foo * sqrt(dnorm(foo, theta, sqrt(s2)))

foo <- (theta + sqrt(theta^2 + 8*s2))/(2*s2)
c <- foo * sqrt(dnorm(foo, theta, sqrt(s2)))

N <- 5e3
samples <- matrix(0, nrow = N, ncol = 2)
n <- 0
while(n < N)
{
  prop.u <- runif(1, min = 0, max = a)
  prop.v <- runif(1, min = b, max = c)
  if(abs(prop.v - theta*prop.u) <= sqrt(-4* s2* prop.u^2 *(log(prop.u) + log(2*pi*s2)/4) ) )
  {
    n <- n+1
    samples[n, ] <- c(prop.u,prop.v)
  }
}

x <- samples[,2]/samples[,1]
z <- (x - theta)/sqrt(s2)
# define color based on regions
color <- 0
for(i in 0:3)
{
  color <- color + (i+1)*(z > i & z < (i+1) )
}
for(i in (-1:-3) )
{
  color <- color + (i+8)*(z > (i) & z < (i+1))
}


# to plot the regions
u <- seq(0.00001, (2*pi*s2)^(-.25), length = 1e3)
foo <-  sqrt(-4*s2*u^2 *(log(u) + log(2*pi*s2)/4))

par(mfrow = c(1,2))
plot(u, theta * u + foo, type = 'l', ylab = "v", main = "C region for N(theta,s2)", ylim = range(c(theta * u + foo, theta * u - foo)))
lines(u,theta * u - foo )

points(samples, col = color + 1, pch = 16)


y <- dnorm(x, mean = theta, sd = sqrt(s2))
plot(x, y, col = color + 1, pch = 16, main = "Normal samples with density")
