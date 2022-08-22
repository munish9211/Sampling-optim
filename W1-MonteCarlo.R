# Monty Hall Calulcations
set.seed(1)
repeats <- 1e4 # We will repeat the experiment 10000 times
win.no.switch <- numeric(length = repeats) # will save 0 or 1 based on winning no switch
win.switch <- numeric(length = repeats)   # will save 0 or 1 based on winning switch

doors <- 3   # three doors
for(r in 1:repeats) # Repeat process many times
{
  # The setup
    prize <- sample(1:doors, 1)  # randomly select the door which has the prize
  
  # Contestants are ready. Game starts
  chosen.door <- sample(1:doors, 1)   # choose a door
  
  # reveal a door that is not the chosen door and not the door 
  # with a prize in it. If doors let to reveal is 1
  # then no other option. If more than one door, then choose randomly
  doors.left <- (1:doors)[-c(prize, chosen.door)]
  if(length(doors.left) == 1)
  {
    reveal <- doors.left
  }else{
    reveal <- sample(doors.left, size = 1) # randomly choose which door to reveal 
  }
  
  # Checking if you win if you didn't switch
  win.no.switch[r] <- chosen.door == prize  #tracking win if don't change door

  # What would happen if you did switch.
  # If number of doors to switch to is 1, then no other option
  # otherwise choose doors randomly
  other.doors <- (1:doors)[-c(chosen.door, reveal)]
  
  if(length(other.doors) == 1)
  {
    switch.door <- other.doors
  }else{
    switch.door <- sample(other.doors, size = 1)
  }
  
  win.switch[r] <- switch.door == prize
}


## ----monty_results----------------
mean(win.no.switch) #Prob of winning if you don't switch
mean(win.switch) # Prob of winning if you switch


## ----toy_coll, cache = TRUE-------
set.seed(1)

# the setup
prob.table <- c(.2, .1, .1, .1, .1, .1, .05, .05, .05, .05, .02, .02, .02, .02, .02)
boxes <- 1:length(prob.table)

# writing the code a little differently now.
# I made a function that will be called repeatedly
box.count <- function(prob)
{
  check <- rep(0, length(prob))
  i <- 0
  while(sum(check) < length(prob)) # check if all toys collected
  {
    x <- sample(boxes, 1, prob = prob) # generate a toy with given prob
    check[x] <- 1    # x has been collected
    i <- i + 1
  }
  return(i)
}

repeats <- 1e4  
sim.boxes <- numeric(repeats)
for(i in 1:repeats)
{
  sim.boxes[i] <- box.count(prob = prob.table)
}


## ----toy_col_hist, fig.height = 3.5, fig.width = 3.5, fig.align = "center"----
hist(sim.boxes, breaks = 30)


## ----toy_avg----------------------
mean(sim.boxes)


## ----esin, cache = TRUE-----------
x <- seq(0,pi,length = 1e3) # grid on x-axis
plot(x, exp(sin(x)), type ='l')

set.seed(1)
repeats <- 1e4

esin <- numeric(length = repeats)
for(i in 1:repeats)
{
  samp <- runif(1, min = 0, max = pi) # draw from U(0, pi)
  esin[i] <- exp(sin(samp))
}
pi * mean(esin)  #pi*E(exp(sin(x))))

