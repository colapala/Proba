
library(randtoolbox)
source('generateurs.R')
source('fonctions.R')

lambda<-8
mu<-15
D<-12

result1<-FileMM1(lambda,mu,D)
depart<-result1[[1]]
arrivee<-result1[[2]]

#affichage console pour débugger (à enlever)
print("depart")
for(i in 1:length(depart)){
  print(depart[[i]])
}

print("arrivee")
for(i in 1:length(arrivee)){
  print(arrivee[[i]])
}

#Q7
result2<-FileMM1Version2(arrivee,depart)
temps<-result2[[1]]
client<-result2[[2]]

cat(unlist(temps))
cat(unlist(client))

par(mfrow=c(1,2))
plot(unlist(temps),unlist(client),xlab='temps', ylab='nbClients En Attente', main='M/M/1')

attenteMoyenne<-result2[[3]]
nbClientMoyen<-result2[[4]]