###########################################
## Accept Reject algorithm to draw from
## Binomial(n,p)
###########################################
set.seed(1)

# Function draws one value from Binom(n,p)
# n = number of trials
# p = probability of success
draw_binom <- function(n, p)
{
  accept <- 0 # Will track the acceptance
  try <- 0 # Will track the number of proposals
  
  # upper bound calculated in the notes
  x <- 0:n
  all_c <- choose(n,x) * (1-p)^(n - 2*x) * p^(x-1)
  c <- max(all_c) + .001 # final c with slight increase for numerical stability.
  
  
  while(accept == 0)
  {
    try <- try + 1

    U <- runif(1)
    prop <- rgeom(1, prob = p) #draw proposal
    
    ratio <- dbinom(x = prop, size = n, prob = p)/
      (c* dgeom(x = prop, prob = p))
    if(U < ratio)
    {
      accept <- 1
      rtn <- prop
    }
  }
  return(c(rtn, try))
}

draw_binom(n = 10, p = .25)


###
# If we want X1, ..., Xn ~ Binom(n.p)
# we need to call the function multiple times

# sample size
N <- 1e3
samp <- numeric(N)
n.try <- numeric(N)
for(t in 1:N)
{
   # I use as a dummy variable often
   foo <- draw_binom(n = 10, p = .25)
   samp[t] <- foo[1]
   n.try[t] <- foo[2]
}
mean(samp) #should be n*p = 2.5
mean(n.try)


###########################################
## A closer look at Binomial and Geometric
###########################################
# Turns out, this choice of Binomial and Geometric
# can work, but not always. In the code below, 
# increase n to see what happens

p <- .25
n <- 1000
x <- 0:(n)
mass.geom <- dgeom(x, p)
mass.bin <- dbinom(x, size = n, prob = p)

all_c <- choose(n,x) * (1-p)^(n - 2*x) * p^(x-1)
(c <- max(all_c))


plot(x, mass.geom, pch = 16, col = "red", type= "n")
points(mass.bin, pch = 16, col = "red", type= "h")
points(mass.geom, pch = 16, col = "blue", type = "h", lty = 2)


# Matching the means:
# choosing p* for rgeom so that np = (1-p*)/p*
p.star <- 1/(n*p + 1)
mass.geom <- dgeom(x, p.star)
mass.bin <- dbinom(x, size = n, prob = p)

# (this was different in the lecture, now the error has
# has been fixed)
all_c <- choose(n,x) * (1-p)^(n - x) *(1-p.star)^(-x) * p^(x) / p.star
(c <- max(all_c))


plot(mass.geom, pch = 16, col = "red", type= "n")
points(mass.bin, pch = 16, col = "red", type= "h")
points(mass.geom, pch = 16, col = "blue", type = "h", lty = 2)


