newsvend.func <- function(q = 120, cu = 2, co = 0.5, mu = 100, sigma = 20){
  # integrand functions
  int.1 <- function(y) {(y - q) * dnorm(y, mean = mu, sd = sigma)}
  int.2 <-  function(y) {(q - y) * dnorm(y, mean = mu, sd = sigma)}
  
  cu * integrate(int.1, lower = q, upper= Inf)$value + co * integrate(int.2 , lower =0, upper = q)$value
}

newsvend.func(q = 120, cu = 1, co = .75, mu = 100, sigma = 30 )


# Formulate a loop that calculates the inventory records over n periods based on an initial stock level (say  i=20) where every 4 periods 40 units arrive at the inventory. Sample the demand for each period from a normal distribution with D ∼ N(10, 2) and round to integers.

# number of periods
n <- 100
i.0 <- 20

d.vec <- rnorm(n = n, mean = 10, sd = 2)
d.vec <- round(d.vec, digits = 0)
i.vec <- numeric(n)

for(i in 1:n){
  
  if(i == 1){
    i.vec[i] <- i.0 
    } 
  else{
    if(i %% 4 == 0){
      i.vec[i] <- i.vec[i-1] + 40 - d.vec[i]
    }
    else{
      i.vec[i] <- i.vec[i-1] - d.vec[i]
    }
  }
}

# plot inventory level and demand vector as a step function

