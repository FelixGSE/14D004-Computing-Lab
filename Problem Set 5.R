################################################################################
####################             Problem set 5              ####################
################################################################################

# Author:       Felix Gutmann
# Programm:     Barcelona graduate school of economics - M.S. Data Science 
# Course:       14D004 Computing Lab (Term 1)
# Last update:  17.11.15

################################################################################

################################################################################

lin.reg <- function(y,X){
  #Least square estimators
  beta <- solve(t(X)%*%X) %*% t(X) %*% y
  return(beta)
}

#Ex2

ridge.reg <- function(y,X,lambda){
  #Ridge estimator
  beta.r <- solve(t(X) %*% X + lambda * diag( ncol( X ) ) ) %*% t(X) %*% y
  return(beta.r)
}

#Ex3

lasso.reg <- function(y,X,lambda){
  #Lasso regression
  beta.ls <- solve(t(X)%*%X) %*% t(X) %*% y
  beta.pr <- beta.ls
  mi      <- 100
  p       <- ncol(X)
  for(i in 1:mi){
    for(j in 1:p){
      y.aux <- y - X[ ,setdiff(1:p,j)] %*% beta.ls[setdiff(1:p,j)]
      x.aux <- X[,j]
      cov <- sum(y.aux * x.aux)
      #Awesome
      var <- sum(x.aux * x.aux)
      temp1 <- cov/var
      temp2 <- lambda/(2*var)
      beta.ls[j] <-  sign(temp1)*max( c( abs(temp1)-temp2,0 ) )
    }
    if( sum( (beta.ls-beta.pr)**2)  < 1e-6 ){break}
    beta.pr <- beta.ls
  }
  return(beta.ls)
}

#4

cross.validation <- function(y,X,lambda,FUN=""){
  #cross validation
  y   <- as.matrix(y)
  n   <- 1:nrow(X)
  s   <- split(n, ceiling(1:length(n)/(nrow(X)/5)))
  rss <- rep(0,5) 
  for(i in 1:5){
    x.temp.s   <- X[s[[i]],]
    x.temp.s2  <- X[setdiff(unlist(s),s[[i]]),]
    y.temp     <- y[setdiff(unlist(s),s[[i]]),]
    y.temp2    <- y[s[[i]],]
    #output
    beta.temp  <- FUN(y.temp,x.temp.s2,lambda)
    fit.temp   <- x.temp.s %*% beta.temp
    rss[i]     <- sum((y.temp2-fit.temp)^2)
    }
  return(mean(rss))
}

################################################################################

################################################################################