newsvend.func <- function(q = 120, cu = 2, co = 0.5, mu = 100, sigma = 20){
  # integrand functions
  int.1 <- function(y) {(y - q) * dnorm(y, mean = mu, sd = sigma)}
  int.2 <-  function(y) {(q - y) * dnorm(y, mean = mu, sd = sigma)}
  
  cu * integrate(int.1, lower = q, upper= Inf)$value + co * integrate(int.2 , lower =0, upper = q)$value
}

newsvend.func(q = 120, cu = 1, co = .75, mu = 100, sigma = 30 )
