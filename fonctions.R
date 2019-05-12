Frequency <-function(x,nb)
{
  somme<-0
  for(j in 1:length(x)){
    bit_mt <- binary(x[j,1])
    for(i in (32+1-nb):32){
      somme<-somme+2*bit_mt[i]-1
    }
  }
  sommeAbs<-abs(somme)/(sqrt(nb*length(x)))
  pvaleur<-2*(1-pnorm(sommeAbs))
  return(pvaleur)
}

Runs <-function(x,nb)
{
  pvaleur<-0.0
  n<-nb*length(x)
  sommeUn<-0
  for(j in 1:length(x)){
    bit_mt <- binary(x[j,1])
    for(i in (32+1-nb):32){
      if(bit_mt[i]==1){
        sommeUn<-sommeUn+1
      }
    }
  }
  
  Pi<-sommeUn/n
  
  if((Pi-(1/2))<(2/(sqrt(n)))){
    somme<-1
    for(j in 1:length(x)){
      bit_mt <- binary(x[j,1])
      for(i in (32+1-nb):32-1){
         if(bit_mt[i]==bit_mt[i+1]){
         somme<-somme+1
         }
      }
    }
    pvaleur<-2*(1-pnorm(abs(somme-2*n*Pi*(1-Pi))/(2*sqrt(n)*Pi*(1-Pi))))
  }
  
  return(pvaleur)
}

Ordre <-function(x,TailleSeq){
  vecteur<-vector("numeric", TailleSeq)
  for(j in 1:TailleSeq){
    vecteur[j]<-x[j,1]
  }
  pvaleur<-order.test(vecteur, d=4, echo=FALSE)$p.value
  return(pvaleur)
}