################################################################################
####################             Problem set 3              ####################
################################################################################

# Author:       Felix Gutmann
# Programm:     Barcelona graduate school of economics - M.S. Data Science 
# Course:       14D004 Computing Lab (Term 1)
# Last update:  20.10.15

################################################################################

################################################################################

wald.test <- function(y,x,alpha){
  #Compute wald test
  my <- mean(y)
  mx <- mean(x)
  d <- my-mx
  se <- sqrt( (var(y) / length(y) ) + ( var( x ) / length( x ) ) )
  test.stat <- d / se
  pval <- 1-pnorm(test.stat)
  zscore <- qnorm(1-alpha)
  reject <- zscore < test.stat
  #Output
  output <- list(test.stat ,pval,reject)
  names(output) <- c("stat","pval","reject")
  return(output)
}

mann.whitney.test <- function(samp1,samp2,alpha){
  #Compute mw test
  n1 <- length( samp1 )
  n2 <- length( samp2 )
  R  <- sum( rank( c( samp1 , samp2 ) )[1:n1] )
  U  <- n1 * n2 + ( ( n1 * ( n1 + 1 ) ) / 2 ) - R
  mu <- ( n1 * n2 ) / 2
  sd <- sqrt( n1 * n2 * ( n1 + n2 + 1 ) / 12 )
  test.stat <- ( U - mu ) / sd
  pvalue <- pnorm( test.stat )
  zscore <- qnorm( alpha )
  #Something
  reject <-  test.stat < zscore
  output <- list(test.stat,pvalue,reject)
  names(output) <- c("stat","pval","reject")
  return(output)
}

mc.test.size <- function(FUN="",alpha,S,n){
  #cool mc simulation
  reject <- matrix( 0 , S , nrow( n ) )
  for(i in 1: nrow(n) ){
    for(s in 1:S){
      X <- rt( n[i,1], 2.1 )
      Y <- rt( n[i,2], 2.1 )
      res <- FUN(X,Y,alpha)$reject
      reject[s,i] <- res
    }
  }
  #output
  out <- colMeans(reject)
  return(out)
}

mc.test.power <- function(FUN="",alpha,S,N,delta){
  #Some stuff
  l <- length(delta)
  reject <- matrix(0,S,l)  
  for( i in 1:l ){
    for( s in 1:S ){
      y <- rt(N[1],2.1)+delta[i]
      x <- rt(N[2],2.1)
      res <- FUN(y,x,alpha)$reject
      reject[s,i] <- res
      #Some more stuff
    }
  }
  out <- colMeans(reject)
  return(out)
}

################################################################################

################################################################################