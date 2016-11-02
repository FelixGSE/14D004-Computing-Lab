################################################################################
####################             Problem set 4              ####################
################################################################################

# Author:       Felix Gutmann
# Programm:     Barcelona graduate school of economics - M.S. Data Science 
# Course:       14D004 Computing Lab (Term 1)
# Last update:  27.10.15

################################################################################

################################################################################

my.chol <- function (A){
  nrows <- nrow(A)
  L <- matrix(0,nrows,nrows)
  L[1,1] <- sqrt(A[1,1])
  if (nrows > 1){
      L[2,1] = A[2,1]/L[1,1]
      L[2,2] <- sqrt(A[2,2]-L[2,1]^2.0)
      if (nrows > 2){
        for (i in 3:nrows){
          L[i,1] = A[i,1]/L[1,1]
          for (j in 2:(i-1)){
            aux <- sum(L[i,1:(j-1)]*L[j,1:(j-1)])
            L[i,j] <- (A[i,j]- aux)/L[j,j]
          }
          L[i,i] <- sqrt(A[i,i]-sum(L[i,1:(i-1)]^2.0))
        }
      }
  }
  return (L)
}


my.forward.solve <- function (L,b){
  size <- length(b)
  y <- rep(0,size)
  y[1] <- b[1]/L[1,1]
  if (size>1){
    for (i in 2:size){
      aux <- sum (L[i,1:(i-1)]*y[1:(i-1)])
      y[i] <- (b[i] - aux)/L[i,i]
    }
  }
  return (y)
}

my.back.solve <- function(L,y) {
  
  n <- length(y)
  x <- rep(0,n)
  
  x[n] <- y[n]/L[n,n]
  
  for (r in (n-1):1) { 
    
    l <- 0 
    
    for (c in (r+1):n) { 
      l <- l + L[r,c] * x[c] 
    } 
    
    x[r] <- (y[r] - l) / L[r,r] # calculate the value of element r
    
  }
  
  x
  
} 

my.solve <- function(A, b) {
  L <- my.chol(A)
  y <- my.forward.solve(L, b)
  x <- my.back.solve(t(L), y)
  return(x)
}

################################################################################

################################################################################