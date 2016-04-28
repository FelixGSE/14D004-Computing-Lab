################################################################################
####################             Problem set 2              ####################
################################################################################

# Author:       Felix Gutmann
# Programm:     Barcelona graduate school of economics - M.S. Data Science 
# Course:       14D004 Computing Lab (Term 1)
# Last update:  13.10.15

################################################################################

################################################################################

congruential.generator <- function(n, seed){
  # First basic random number generator
  a <- 48271
  m <- 2**31-1
  c <- 1
  x <- rep(0,(n+1))
  x[1] <- seed
  for(i in 2:(n+1)){
    x[i] <- (a * x[i-1] + c) %% m
  }
  #return draws
  draws <- x[2:(n+1)] / m
  return(draws)
}

normal.simulator <- function(uni.draws,mean,sd){
  # Some stuff
  n <- length(uni.draws)
  ind1 <- seq(1,n-1,2)
  ind2 <- seq(2,n,2)
  sub <- lapply(1:(n/2),function(i){uni.draws[ ind1[i]:ind2[i] ] })
  #Generate z1 and z2
  x   <- sapply(1:(n/2),function(i){( sqrt ( -2 * log( sub[[i]][2]) ) * cos ( 2 * pi * sub[[i]][1] ) ) * sd  + mean})
  y   <- sapply(1:(n/2),function(i){( sqrt ( -2 * log( sub[[i]][2]) ) * sin ( 2 * pi * sub[[i]][1] ) ) * sd  + mean})
  draw.normal <- c(rbind(x, y))
  return(draw.normal)
}

beta.simulator <- function(n,alpha,beta,k){
  draw.beta <- rep(0,n)
  #Generate beta draws
  for(i in 1:n){
    repeat{
      x     <- runif(1)
      u     <- runif(1)
      if( u < dbeta(x,alpha,beta)/(k*dunif(x)) ){
        draw.beta[i] <- x
        break
      }
    }
  }
  return(draw.beta)
}

pareto.simulator <- function(rand.imp,alpha){
  n <- length(rand.imp)
  #Generate draws from pareto
  draw <- sapply(1:n,function(i){(1-rand.imp[i])**(-(1/alpha))})
  return(draw)
}
################################################################################

################################################################################