set.seed(100)
#######################################
## Bernoulli factory
## to generate 1e5 realization from  
## Bern(p^2 (1-p)) using only Bern(p)
#######################################

p <- .90   # setting p
N <- 1e5   # setting N, the number of samples


############
## Method 1:  the longer way

bf_sample <- numeric(length = N)  #vector stores the output

method1.time <- proc.time()  # to track the time it takes for this method
for(i in 1:N)
{
  
  x1 <- rbinom(1, size = 1, prob = p)
  if(x1 == 1)
  {
    x2 <- rbinom(1, size = 1, prob = p)
    if(x2 == 1)
    {
      x3 <- rbinom(1, size = 1, prob = p)
      if(x3 == 0)
      {
        
        bf_sample[i] <- 1
      } else{
        bf_sample[i] <- 0
      }
      
    } else{
      bf_sample[i] <- 0
    }
  } else{
    bf_sample[i] <- 0
  }
}
method1.time <- proc.time() - method1.time
# checking if algorithm works
mean(bf_sample)
p^2 * (1-p)   # truth


############
# Method  2: Very short way.
bf_sample <- numeric(length = N)  #vector stores the output

method2.time <- proc.time()  #to track the time for this method
for(i in 1:N)
{
  x_vec <- rbinom(3, size = 1, prob = p)
  bf_sample[i] <- x_vec[1] * x_vec[2] * (1 - x_vec[3])
}
method2.time <- proc.time() - method2.time

# checking if algorithm works
mean(bf_sample)
p^2 * (1-p)   # truth


############
# Method 3: Yet another way for this example

bf_sample <- numeric(length = N)  #vector stores the output

method3.time <- proc.time()  #to track the time for this method
for(i in 1:N)
{
  x_vec <- rbinom(3, size = 1, prob = p)
  y <- rbinom(1, size = 1, prob = 1/3)
  if(sum(x_vec) == 2 && y == 1) # this will give 3*p^2*(1-p)
  {
    bf_sample[i] <- 1
  } else{
    bf_sample[i] <- 0
  }
}
method3.time <- proc.time() - method3.time

# checking if algorithm works
mean(bf_sample)
p^2 * (1-p)   # truth





# comparing times
c(method1.time[3], method2.time[3], method3.time[3])



