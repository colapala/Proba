VonNeumann <- function(n, p=1, graine)
{
  x <-  rep(graine,n*p+1)
  for(i in 2:(n*p+1))
  {
    numbers <- strsplit(format(x[i-1]^2,scientific=FALSE),'')[[1]]
    while(length(numbers)>4){ 
        numbers <- numbers[2:(length(numbers)-1)] 
    }
    x[i] <- as.numeric(numbers)%*%(10^seq(length(numbers)-1,0,-1))
  }
  x <- matrix(x[2:(n*p+1)],nrow=n,ncol=p)
  return(x)
}


MersenneTwister <- function(n, p=1, graine)
{
  set.seed(graine,kind='Mersenne-Twister')
  x <- sample.int(2^32-1,n*p)
  x <- matrix(x,nrow=n,ncol=p)
  return(x)
}


binary <- function(x)
{
  if((x<2^31)&(x>=0))
    return( as.integer(rev(intToBits(as.integer(x)))) )
  else{
    if((x<2^32)&(x>0))
      return( c(1,binary(x-2^31)[2:32]) )
    else{
      cat('Erreur dans binary : le nombre etudie n est pas un entier positif en 32 bits.\n')
      return(c())
    }
  }
}

RANDU <- function(k,graine)
{
  mat<-matrix(0,nrow=k,ncol=1)
  for(i in 1:k){
    mat[i,1]<- 65539*graine
    mat[i,1]<-mat[i,1]%%(2^31)
    graine <-mat[i,1]
  }
  return (mat)
}

StandardMinimal <- function(k,graine)
{
  mat<-matrix(0,nrow=k,ncol=1)
  for(i in 1:k){
    mat[i,1]<- 16807*graine
    mat[i,1]<-mat[i,1]%%((2^31)-1)
    graine <-mat[i,1]
  }
  return (mat)
  
}

