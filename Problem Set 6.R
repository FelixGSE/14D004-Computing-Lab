################################################################################
####################             Problem set 6              ####################
################################################################################

# Author:       Felix Gutmann
# Programm:     Barcelona graduate school of economics - M.S. Data Science 
# Course:       14D004 Computing Lab (Term 1)
# Last update:  24.11.15

################################################################################

################################################################################

probit.iwls <- function(y,X){
  #Probit fit
  mitter         <- 100
  beta          <- rep(0,ncol(X))
  beta.pr     <- beta
  pm            <- X %*% beta
  mue            <- ( y + 0.5 ) / 2
  for(i in 1:mitter) {
    q      <- 1 / dnorm( qnorm( mue ) )
    V      <- mue * ( 1 - mue )
    z      <- pm + ( y - mue ) * q
    w      <- 1 / ( V * q^2 )
    W      <- diag( as.numeric(w) )
    beta <- solve( t(X) %*% W %*% X ) %*% t(X) %*% W %*% z 
    pm      <- X %*% beta
    mue  <- pnorm(pm)
    if(sum( ( beta - beta.pr )^2 ) < 1e-4){break}
    beta.pr <- beta
  }
  return(beta) 
}

probit.cov <- function(y,X,beta){
  pm     <- X %*% beta
  mue <- pnorm( pm )
  V     <- mue * ( 1 - mue )
  q     <- 1 / dnorm( qnorm( mue ) )
  w     <- 1 / ( V * q^2 )
  W     <- diag( as.numeric(w) )
  cov <- solve( t(X) %*% W %*% X )
  return(cov)
}

probit.fitted <- function(y,X,beta){
  out     <- pnorm( X %*% beta )
  return(out)
}

################################################################################

################################################################################