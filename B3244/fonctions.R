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

FileMM1 <-function(lambda,mu,D){
  
  arrivee<- list()
  depart<- list()
  
  temps<-rexp(1,lambda)
  
  #pour les arriv�es : 
  #r�gulier, tous les exp(lamb)
  
  j<-0
  while (temps<D)
  {
    j<-j+1
    arrivee[[j]]<-temps
    temps<-temps+rexp(1,lambda)
  }
  
  #pour les d�parts : 
  #pr le client c : d�part exp(mu) apr�s l'arriv�e SI le client c-1 est d�j� parti
  
  taille=length(arrivee)
  print(taille)
  
  if(taille>0){
    depart[[1]]<-arrivee[[1]]+rexp(1,mu) #Initialise pour le 1e client
  }
  
  depasse<-1
  for(i in 2:taille){
    
    if(depasse==1){
    
    if(arrivee[[i]]>depart[[(i-1)]]){ #le client c-1 est d�j� parti
      #met � jour le temps et le fixe pour le d�part
      temps<-arrivee[[i]]+rexp(1,mu) 
      if(temps<D){
          depart[[i]]<-temps
      }else{
        depasse<-0
      }
    }else { #le client c-1 est toujours l� : il faut attendre son d�part
      #met � jour le temps et le fixe pour le d�part
      temps=depart[[(i-1)]]+rexp(1,mu)
      if(temps<D){
          depart[[i]]<- temps
      }else{
        depasse<-0
      }
    }
    }
  }
  
  return(list(val1=depart, val2=arrivee))
  
}

FileMM1Version2 <-function(arrivee,depart){
  
  #------------------------------------------------
  # Calcul de l'�volution des clients dans le syst�me
  
  temps<- list()
  clients<- list()
  Nbclients=length(arrivee)
  
  i<-1 # pr les d�parts
  j<-1 # pr les arriv�es
  n<-2 # pr les clients et le temps
  clients[[1]]<-0
  temps[[1]]<-0
  
  #on parcourt les liste arrivee et depart en comparant les temps � chaque fois
  #pour savoir l'ordre chronologique des �v�nements qui se sont produits
  
  while(i<=(length(depart)+1)&&j<=(Nbclients+1)&&n<=(Nbclients+length(depart)+1)){#Tant qu'il y a encore des �v�nements
    
    if(i==(length(depart)+1)){
      clients[[n]]<-clients[[n-1]]+1 #on ajoute un client
      temps[[n]]<-arrivee[[j]] #on met le temps � jour
      j<-j+1
      n<-n+1
    }else if(j==(Nbclients+1)){
      clients[[n]]<-clients[[n-1]]-1 #on enl�ve un client
      temps[[n]]<-depart[[i]] #on met le temps � jour
      i<-i+1
      n<-n+1
    }else{
    
    if(depart[[i]]>=arrivee[[j]]){ #Chronologiquement, il y a eu une arriv�e
        clients[[n]]<-clients[[n-1]]+1 #on ajoute un client
        temps[[n]]<-arrivee[[j]] #on met le temps � jour
        j<-j+1
        n<-n+1
    } else if(depart[[i]]<arrivee[[j]]) { #Chronologiquement, il y a eu un d�part
        clients[[n]]<-clients[[n-1]]-1 #on enl�ve un client
        temps[[n]]<-depart[[i]] #on met le temps � jour
        i<-i+1
        n<-n+1
    }
    }
  }
  
  #------------------------------------------------
  #Calcul de la moyenne du temps d'attente

  moyAtt<-0

  #fait la somme pour chaque client
  for (i in 1:length(depart))
  {
    moyAtt<-moyAtt + depart[[i]] - arrivee[[i]];
  }

  #divise par le nombre de clients -> on enl�ve ceux qui ne sont pas encore partis
  moyAtt<-moyAtt/length(depart)


 # ------------------------------------------------
   #Calcul nombre moyen de clients
  
   taille<-length(clients)
  
   nbMoyClients<-0
  
   #fait la somme sur chaque intervalle de temps
   for (i in 1:(taille-1))
   {
     nbMoyClients<-nbMoyClients + (clients[[i]] * (temps[[(i+1)]] - temps[[i]]))
   }
   #ajoute le dernier intervalle de temps + divise par D
   nbMoyClients<-(nbMoyClients + clients[[taille]] * (D - temps[[taille]]))/D
  
  
   #------------------------------------------------
  
  return(list(val1=temps, val2=clients, val2=moyAtt, val4=nbMoyClients))
}


