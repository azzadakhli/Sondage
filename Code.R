library(readxl)
df<- as.data.frame(read_excel("C:/Users/azza/Desktop/sondage.xlsx"))
colnames(df) <-c("id" , "sex" , "age" , "csp" , "temps" , "educ","opt" ,"way" , "period" , "most_used")
df$id <- rep(1:nrow(df))


library(ggplot2)
#Sexe

sex <- pie( table(df$sex) , labels = c("36","30" ), main = "Population par sexe", col = rainbow(length(x)),
legend("topright", c("Homme", "Femme")), cex = 0.6,fill = rainbow(length(x)))

install.packages("MASS")
install.packages("lpSolve")
install.packages("sampling")
library(sampling)
library(lpSolve)
library(MASS)
N=100
n=20

library(stringr)
temps <- as.vector(df$temps)
temps <- str_replace(temps, "3h-5h" , "4") # la moyenne de 3h et 5h
temps <- str_replace(temps, "5h ou plus" , "9.5") # la moyenne de 5h à 14h ( maximum)
temps <- str_replace(temps , "2h ou moins" ,"1" ) # la moyenne
temps <- as.numeric(temps)
mean(temps)
var(temps)


########PESR#########
ech=matrix(0,nrow=20,ncol=20) ##matrice qui va contenir les 20 échantillon
ICpesr=matrix(0,nrow=20,ncol=2) ##matrice qui va contenir les 20 IC
temps.bar.pesr=NULL ##vecteur qui va contenir les 20 estimateurs de la moyenne
var.pesr=NULL##vecteur qui va contenir les 20 estimateurs de la variance
##Tirage de 20 échantillons
for (i in 1:20){
  
  s=srswor(n,N)
  ech[,i]=temps[s==1][1:20]
  temps.bar.pesr[i]=mean(ech[,i])
  var.pesr[i]=var(ech[,i])
}

##Intervalles de confiance
for(i in 1:20){
  ICpesr[i,]=c(temps.bar.pesr[i]- 1.96*sqrt(var(temps.bar.pesr)/20),temps.bar.pesr[i]+1.96*sqrt(var(temps.bar.pesr)/20))
  
}
longueurpesr=ICpesr[,2]-ICpesr[,1]
moy_temps.bar.pesr=mean(temps.bar.pesr)
###Résultats
##Les IMC dans les 20 échantillons tirés
ech

##Les estimateurs de la moyenne dans les 20 échantillons tirés
temps.bar.pesr
##Les estimateurs de la variance dans les 20 échantillons tirés
var.pesr
##Les 20 intervalles de confiances de la moyenne sont
ICpesr
##Longueurs des Intervalles de Confiance
longueurpesr
##Représentation des IC
plot(seq(1,20,by=1),IC[,1],col="red",ylim=c(min(min(IC[,2]),min(IC[,1])),max(max(IC[,2]),max(IC[,1])))
)
lines(seq(1,20,by=1),IC[,1],col="blue")
points(seq(1,20,by=1),IC[,2],col="red")
lines(seq(1,20,by=1),IC[,2],col="blue")
##La moyenne des 20 estimateurs de la moyenne est
moy_temps.bar.pesr



########PEAR#########
ech=matrix(0,nrow=20,ncol=20) ##matrice qui va contenir les 20 échantillon
ICpear=matrix(0,nrow=20,ncol=2) ##matrice qui va contenir les 20 IC
temps.bar.pear=NULL ##vecteur qui va contenir les 20 estimateurs de la moyenne
var.pear=NULL ##vecteur qui va contenir les 20 estimateurs de la variance
##Tirage de 20 échantillons
for (i in 1:20){
  
  ech[,i]=sample(temps,20,replace=TRUE)
  temps.bar.pear[i]=mean(ech[,i])
  var.pear[i]=var(ech[,i])
}

##Intervalles de confiance
for(i in 1:20){
  seq(1, 20, by = 1)
  ICpear[, 1]
  ICpear[i,]=c(temps.bar.pear[i]- 1.96*sqrt(var(temps.bar.pear)/20),temps.bar.pear[i]+1.96*sqrt(var(temps.bar.pear)/20))
  
}
longueurpear=ICpear[,2]-ICpear[,1]
moy_temps.bar.pear=mean(temps.bar.pear)
###Résultats
##Les IMC dans les 20 échantillons tirés
ech
##Les estimateurs de la moyenne dans les 20 échantillons tirés
temps.bar.pear
##Les estimateurs de la variance dans les 20 échantillons tirés
var.pear
##Les 20 intervalles de confiances de la moyenne sont
ICpear
##Longueurs de Intervalles de Confiance
longueurpear
##Représentation des IC
plot(seq(1,20,by=1),IC[,1],col="red",ylim=c(min(min(IC[,2]),min(IC[,1])),max(max(IC[,2]),max(IC[,1])))
)
lines(seq(1,20,by=1),IC[,1],col="blue")
points(seq(1,20,by=1),IC[,2],col="red")
lines(seq(1,20,by=1),IC[,2],col="blue")
##La moyenne des 20 estimateurs de la moyenne est
moy_temps.bar.pear


#Sondage à proba inégales
#sondage aléatoire à probabilité inégales
###PISR####
pi=inclusionprobabilities(temps,n)
sum(pi)
#les20echantillons
ech=matrix(0,nrow=20,ncol=20)
#les_20_Intervalles_de_confiances
ICpisr=matrix(0,nrow=20,ncol=2)
#Tirage_de_20_echantillons
temps.bar.pisr=NULL
var.pisr=NULL
for ( i in 1:n) {
  s=UPsystematic(pi)
  ech[,i]=temps[s==1][1:20]
  temps.bar.pisr[i]= mean(ech[,i])
  var.pisr[i]=var(ech[,i])
  
}

#IC_pisr
for(i in 1:n){
  ICpisr[i,]=c(temps.bar.pisr[i]-1.96*sqrt(var(temps.bar.pisr)/20),
           temps.bar.pisr[i]+1.96*sqrt(var(temps.bar.pisr)/20))
}
longueurpisr=ICpisr[,2]-ICpisr[,1]
moy_temps.bar.pisr=mean(temps.bar.pisr)

##Les IMC dans les 20 échantillons tirés
ech
##Les estimateurs de la moyenne dans les 20 échantillons tirés
temps.bar.pisr
##Les estimateurs de la variance dans les 20 échantillons tirés
var.pisr
##Les 20 intervalles de confiances de la moyenne sont
ICpisr
##Longueurs de Intervalles de Confiance
longueurpisr
##Représentation des IC
plot(seq(1,20,by=1),IC[,1],col="red",ylim=c(min(min(IC[,2]),min(IC[,1])),max(max(IC[,2]),
                                                                             max(IC[,1])))
)
lines(seq(1,20,by=1),IC[,1],col="blue")
points(seq(1,20,by=1),IC[,2],col="red")
lines(seq(1,20,by=1),IC[,2],col="blue")

##La moyenne des 20 estimateurs de la moyenne est
moy_temps.bar.pisr

########PIAR#########
##On définit le vecteur pk
pk=inclusionprobabilities(temps,n)/n
sum(pk)
v=c(0,cumsum(pk))
ech=matrix(0,nrow=20,ncol=20) ##matrice qui va contenir les 20 échantillon
ICpiar=matrix(0,nrow=20,ncol=2) ##matrice qui va contenir les 20 IC
temps.bar.piar=NULL ##vecteur qui va contenir les 20 estimateurs de la moyenne
var.piar=NULL ##vecteur qui va contenir les 20 estimateurs de la variance
##Tirage de 20 échantillons
for (i in 1:20){
  s=NULL
  for(j in 1:20){
    u=runif(1,0,1)
    for(k in 2:101){
      if( (v[k-1]<=u)&(v[k]>u) ) {
        s=c(s,k-1)
      }
      
    }
  }
  
  ech[,i]=temps[s]
  temps.bar.piar[i]=mean(ech[,i])
  var.piar[i]=var(ech[,i])
}
##Intervalles de confiance
for(i in 1:20){
  ICpiar[i,]=c(temps.bar.piar[i]-1.96*sqrt(var(temps.bar.piar)/20),temps.bar.piar[i]+1.96*sqrt(var(temps.bar.piar)/20))
}
longueurpiar=ICpiar[,2]-ICpiar[,1]
moy_y.bar.piar=mean(temps.bar.piar)
###Résultats
##Les IMC dans les 20 échantillons tirés
ech
##Les estimateurs de la moyenne dans les 20 échantillons tirés
temps.bar.piar
##Les estimateurs de la variance dans les 20 échantillons tirés
var.piar
##Les 20 intervalles de confiances de la moyenne sont
ICpiar
##Longueurs de Intervalles de Confiance
longueurpiar
##Représentation des IC
plot(seq(1,20,by=1),IC[,1],col="red",ylim=c(min(min(IC[,2]),min(IC[,1])),max(max(IC[,2]),max(IC[,1])))
)
lines(seq(1,20,by=1),IC[,1],col="green")
points(seq(1,20,by=1),IC[,2],col="red")
lines(seq(1,20,by=1),IC[,2],col="blue")
##La moyenne des 20 estimateurs de la moyenne est
moy_y.bar.piar

########STRATIFICATION#########
####Même Tailles de Strates####
##La variable "Sexe" va être utilisée comme variable de stratification
##La variable présente 2 valeurs:
levels(df$sex)
##La taille de chaque strate
N1=sum(df$sex=="Homme")
N2=sum(df$sex=="Femme")
ech=matrix(0,nrow=20,ncol=20) ##matrice qui va contenir les 20 échantillon
ICstrat1=matrix(0,nrow=20,ncol=2) ##matrice qui va contenir les 20 IC
temps.bar.strat1=NULL ##vecteur qui va contenir les 20 estimateurs de la moyenne
var.strat1=NULL ##vecteur qui va contenir les 20 estimateurs de la variance
seq(1, 20, by = 1)
ICstrat1[, 1]
##Tirage de 20 échantillons
for(i in 1:20){
  s=strata(df[order(df$sex),], stratanames="sex", size=c(10,10), method="srswor")
  ech[,i]=temps[s$ID_unit]
  temps.bar.strat1[i]=mean(ech[,i])
  var.strat1[i]=var(ech[,i])
}

##Intervalles de confiance
for(i in 1:20){
  ICstrat1[i,]=c(temps.bar.strat1[i]- 1.96*sqrt(var(temps.bar.strat1)/20),temps.bar.strat1[i]+1.96*sqrt(var(temps.bar.strat1)/20))
  
}
longueurstrat1=ICstrat1[,2]-ICstrat1[,1]
moy_temps.bar.strat1=mean(temps.bar.strat1)
###Résultats
##Les IMC dans les 20 échantillons tirés
ech
##Les estimateurs de la moyenne dans les 20 échantillons tirés
temps.bar.strat1
##Les estimateurs de la variance dans les 20 échantillons tirés
var.strat1
##Les 20 intervalles de confiances de la moyenne sont
ICstrat1
##Longueurs de Intervalles de Confiance
longueurstrat1
##Représentation des IC
plot(seq(1,20,by=1),IC[,1],col="red",ylim=c(min(min(IC[,2]),min(IC[,1])),max(max(IC[,2]),max(IC[,1])))
)
lines(seq(1,20,by=1),IC[,1],col="blue")
points(seq(1,20,by=1),IC[,2],col="red")
lines(seq(1,20,by=1),IC[,2],col="blue")
##La moyenne des 20 estimateurs de la moyenne est
moy_temps.bar.strat1

########STRATIFICATION#########
####Allocation proportionnelle####
##La variable "Genre" va être utilisée comme variable de stratification
##La variable présente 2 valeurs:
levels(df$sex)
##La taille de chaque strate
N1=sum(df$sex=="Femme")
N2=sum(df$sex=="Homme")
n1=round((n/N)*N1)
n2=round((n/N)*N2)
ech=matrix(0,nrow=20,ncol=20) ##matrice qui va contenir les 20 échantillon
ICstrat2=matrix(0,nrow=20,ncol=2) ##matrice qui va contenir les 20 IC
temps.bar.strat2=NULL ##vecteur qui va contenir les 20 estimateurs de la moyenne
var.strat2=NULL ##vecteur qui va contenir les 20 estimateurs de la variance
##Tirage de 20 échantillons
for(i in 1:20){
  s=strata(df[order(df$sex),], stratanames="sex", size=c(n1,n2), method="srswor")
  ech[,i]=temps[s$ID_unit]
  temps.bar.strat2[i]=mean(ech[,i])
  var.strat2[i]=var(ech[,i])
}
##Intervalles de confiance
for(i in 1:20){
  ICstrat2[i,]=c(temps.bar.strat2[i]-1.96*sqrt(var(temps.bar.strat2)/20),temps.bar.strat2[i]+1.96*sqrt(var(temps.bar.strat2)/20))
}
longueurstrat2=ICstrat2[,2]-ICstrat2[,1]
moy_temps.bar.strat2=mean(temps.bar.strat2)
###Résultats
##Les IMC dans les 20 échantillons tirés
ech
##Les estimateurs de la moyenne dans les 20 échantillons tirés
temps.bar.strat2
##Les estimateurs de la variance dans les 20 échantillons tirés
var.strat2
##Les 20 intervalles de confiances de la moyenne sont
ICstrat2

##Longueurs de Intervalles de Confiance
longueurstrat2
##Représentation des IC
plot(seq(1,20,by=1),IC[,1],col="red",ylim=c(min(min(IC[,2]),min(IC[,1])),max(max(IC[,2]),max(IC[,1])))
)
lines(seq(1,20,by=1),IC[,1],col="blue")
points(seq(1,20,by=1),IC[,2],col="red")
lines(seq(1,20,by=1),IC[,2],col="blue")
##La moyenne des 20 estimateurs de la moyenne est
moy_temps.bar.strat2


########STRATIFICATION#########
#### Allocation Optimale ####
##Division de la population en 2 strates
tempsf=temps[df$sex=="Femme"]
tempsh=temps[df$sex=="Homme"]
##La taille de chaque strate
N1=sum(df$sex=="Femme")
N2=sum(df$sex=="Homme")
##La taille optimale de chaque strate
n.Femme=round((20*N1*sqrt((n/(n-1))*var(tempsf)))/(N1*sqrt((n/(n-1))*var(tempsf))+N2*sqrt((n/(n- 1))*var(tempsh))))
n.Homme=round((20*N2*sqrt((n/(n-1))*var(tempsh)))/(N1*sqrt((n/(n-1))*var(tempsf))+N2*sqrt((n/(n- 1))*var(tempsh))))
ech=matrix(0,nrow=20,ncol=20) ##matrice qui va contenir les 20 échantillons
seq(1, 20, by = 1)
ICstrat3[, 1]
ICstrat3=matrix(0,nrow=20,ncol=2) ##matrice qui va contenir les 20 IC
temps.bar.strat3=NULL ##vecteur qui va contenir les 20 estimateurs de la moyenne
var.strat3=NULL ##vecteur qui va contenir les 20 estimateurs de la variance
##Tirage de 20 échantillons
for(i in 1:20){
  s=strata(df[order(df$sex),], stratanames="sex", size=c(n.Femme,n.Homme),
           method="srswor")
  ech[,i]=temps[s$ID_unit]
  temps.bar.strat3[i]=mean(ech[,i])
  var.strat3[i]=var(ech[,i])
}
##Intervalles de confiance
for(i in 1:20){
  ICstrat3[i,]=c(temps.bar.strat3[i]-1.96*sqrt(var(temps.bar.strat3)/20),temps.bar.strat3[i]+1.96*sqrt(var(temps.bar.strat3)/20))
}
longueurstrat3=ICstrat3[,2]-ICstrat3[,1]
moy_temps.bar.strat3=mean(temps.bar.strat3)
###Résultats
##Les IMC dans les 20 échantillons tirés
ech
##Les estimateurs de la moyenne dans les 20 échantillons tirés
temps.bar.strat3
##Les estimateurs de la variance dans les 20 échantillons tirés
var.strat3
##Les 20 intervalles de confiances de la moyenne sont
ICstrat3
##Longueurs de Intervalles de Confiance
longueurstrat3
##Représentation des IC
plot(seq(1,20,by=1),IC[,1],col="red",ylim=c(min(min(IC[,2]),min(IC[,1])),max(max(IC[,2]),max(IC[,1])))
)
lines(seq(1,20,by=1),IC[,1],col="blue")
points(seq(1,20,by=1),IC[,2],col="red")
lines(seq(1,20,by=1),IC[,2],col="blue")
##La moyenne des 20 estimateurs de la moyenne est
moy_temps.bar.strat3


########PAR GRAPPES#########
ech=matrix(0,nrow=46,ncol=10) ##matrice qui va contenir les 10 échantillons
ICgrap=matrix(0,nrow=10,ncol=2) ##matrice qui va contenir les 10 IC
temps.bar.grap=NULL ##vecteur qui va contenir les 10 estimateurs de la moyenne
var.grap=NULL ##vecteur qui va contenir les 10 estimateurs de la variance
##On va choisir la variable "le réseau social le plus utilisé" comme variables auxiliaire
##la variable auxiliare présente M=5 
modal=c("Facebook, Messenger","Instagram","Tiktok","Youtube", "Autre")
M=5
m=round(M/4) ##Taille de l'échantillon (nombre des grappes) à tirer

##Tirage de 10 échantillons
#tirage 1#
echant1=NULL ##vecteur qui va contenir le 1er tirage
s=sample(modal,2)
echant1=c(temps[df$most_used==s[1]],temps[df$most_used==s[2]])
ech[1:length(echant1),1]=echant1 ##On construit la matrice "ech" (echantillon)
##au fur et à mesure après chaque tirage
##On répète l'opération 10 fois
temps.bar.grap[1]=mean(echant1)
var.grap[1]=var(echant1)
#tirage 2
echant2=NULL
s=sample(modal,2)
echant2=c(temps[df$most_used==s[1]],temps[df$most_used==s[2]])
ech[1:length(echant2),2]=echant2
temps.bar.grap=c(temps.bar.grap,mean(echant2))
var.grap=c(var.grap,var(echant2))

#tirage 3
echant3=NULL
s=sample(modal,2)
echant3=c(temps[df$most_used==s[1]],temps[df$most_used==s[2]])
ech[1:length(echant3),3]=echant3
temps.bar.grap=c(temps.bar.grap,mean(echant3))
var.grap=c(var.grap,var(echant3))
#tirage 4
echant4=NULL
s=sample(modal,2)
echant4=c(temps[df$most_used==s[1]],temps[df$most_useds==s[2]])
ech[1:length(echant4),4]=echant4
temps.bar.grap=c(temps.bar.grap,mean(echant4))
var.grap=c(var.grap,var(echant4))
#tirage 5
echant5=NULL
s=sample(modal,2)
echant5=c(temps[df$most_used==s[1]],temps[df$most_used==s[2]])
ech[1:length(echant5),5]=echant5
temps.bar.grap=c(temps.bar.grap,mean(echant5))
var.grap=c(var.grap,var(echant5))
#tirage 6
echant6=NULL
#s=sample(modal,2)
echant6=c(temps[df$most_used==s[1]],temps[df$most_used==s[2]])
ech[1:length(echant6),6]=echant6
temps.bar.grap=c(temps.bar.grap,mean(echant6))
var.grap=c(var.grap,var(echant6))
#tirage 7
echant7=NULL
s=sample(modal,2)
echant7=c(temps[df$most_used==s[1]],temps[df$most_used==s[2]])
ech[1:length(echant7),7]=echant7
temps.bar.grap=c(temps.bar.grap,mean(echant7))
var.grap=c(var.grap,var(echant7))
#tirage 8
echant8=NULL
#s=sample(modal,2)
echant8=c(temps[df$most_used==s[1]],temps[df$most_used==s[2]])
ech[1:length(echant8),8]=echant8
temps.bar.grap=c(temps.bar.grap,mean(echant8))
var.grap=c(var.grap,var(echant8))
#tirage 9
echant9=NULL
#s=sample(modal,2)
echant9=c(temps[df$most_used==s[1]],temps[df$most_used==s[2]])
ech[1:length(echant9),9]=echant9
temps.bar.grap=c(temps.bar.grap,mean(echant9))
var.grap=c(var.grap,var(echant9))
#tirage 10
echant10=NULL
s=sample(modal,2)
echant10=c(temps[df$most_used==s[1]],temps[df$most_used==s[2]])
ech[1:length(echant10),10]=echant10
temps.bar.grap=c(temps.bar.grap,mean(echant10))
var.grap=c(var.grap,var(echant10))
##Intervalles de confiance
for(i in 1:10){
  ICgrap[i,]=c(temps.bar.grap[i]- 1.96*sqrt(var(temps.bar.grap)/sum(ech[,i]!=0)),temps.bar.grap[i]+1.96*sqrt(var(temps.bar.grap)/sum(ech[,i]!=0)))
}
longueurgrap=ICgrap[,2]-ICgrap[,1]
moy_temps.bar.grap=mean(temps.bar.grap)
###Résultats
##Les IMC dans les 10 échantillons tirés
ech
##Les estimateurs de la moyenne dans les 10 échantillons tirés
temps.bar.grap
##Les estimateurs de la variance dans les 10 échantillons tirés
var.grap
##Les 10 intervalles de confiances de la moyenne sont
ICgrap
##Longueurs de Intervalles de Confiance
longueurgrap
##Représentation des IC
plot(seq(1,10,by=1),IC[,1],col="red",ylim=c(min(min(IC[,2]),min(IC[,1])),max(max(IC[,2]),max(IC[,1])))
)
lines(seq(1,10,by=1),IC[,1],col="blue")
points(seq(1,10,by=1),IC[,2],col="red")
lines(seq(1,10,by=1),IC[,2],col="blue")
##La moyenne des 10 estimateurs de la moyenne est
moy_temps.bar.grap

########A plusieurs degrés#########
ech=matrix(0,nrow=46,ncol=10) ##matrice qui va contenir les 10 échantillons
IC2deg=matrix(0,nrow=10,ncol=2)##matrice qui va contenir les 10 IC
temps.bar.plusdeg=NULL ##vecteur qui va contenir les 10 estimateurs de la moyenne
var.plusdeg=NULL ##vecteur qui va contenir les 10 estimateurs de la variance
##On va choisir la variable "le réseau social le plus utilisé" comme variables auxiliaire
##la variable auxiliaire présente M=5
modal=c("Facebook, Messenger","Instagram","Tiktok","Youtube", "Autre")
M=5
m=round(M/4) ##Taille de l'échantillon (nombre des grappes) à tirer
seq(1, 10, by = 1)
IC2deg[, 1]
##Tirage de 10 échantillons
#tirage 1
s.1=NULL;s.2=NULL
s.1=sample(modal,2)
grap1=df[df$most_used==s.1[1],]
grap2=df[df$most_used==s.1[2],]
##taille de l'échantillon tiré dans les grappes
if(round(length(grap1$id)/5)==0){nh1=1}
if(round(length(grap1$id)/5)>0){nh1=round(length(grap1$id)/5)}
if(round(length(grap2$id)/5)==0){nh2=1}
if(round(length(grap2$id)/5)>0){nh2=round(length(grap2$id)/5)}
##Tirage dans les grappes
s.2=c(sample(grap1$id,nh1),sample(grap2$id,nh2))
echant1.2=NULL
echant1.2[1]=temps[df$id==s.2[1]]
for(i in 2:(length(s.2))){
  echant1.2=c(echant1.2,temps[df$id==s.2[i]])
}

ech[1:length(echant1.2),1]=echant1.2
temps.bar.plusdeg[1]=mean(echant1)
var.plusdeg[1]=var(echant1)

#tirage 2
s.1=NULL;s.2=NULL
s.1=sample(modal,2)
grap1=df[df$most_used==s.1[1],]
grap2=df[df$most_used==s.1[2],]
##taille de l'échantillon tiré dans les grappes
if(round(length(grap1$id)/5)==0){nh1=1}
if(round(length(grap1$id)/5)>0){nh1=round(length(grap1$id)/5)}
if(round(length(grap2$id)/5)==0){nh2=1}
if(round(length(grap2$id)/5)>0){nh2=round(length(grap2$id)/5)}
##Tirage dans les grappes
s.2=c(sample(grap1$id,nh1),sample(grap2$id,nh2))
echant2.2=NULL
echant2.2[1]=temps[df$id==s.2[1]]
for(i in 2:(length(s.2))){
  echant2.2=c(echant2.2,temps[df$id==s.2[i]])
}

ech[1:length(echant2.2),2]=echant2.2
temps.bar.plusdeg=c(temps.bar.plusdeg,mean(echant2))
var.plusdeg=c(var.plusdeg,var(echant2))
#tirage 3
s.1=NULL;s.2=NULL
s.1=sample(modal,2)
grap1=df[df$most_used==s.1[1],]
grap2=df[df$most_used==s.1[2],]
##taille de l'échantillon tiré dans les grappes
if(round(length(grap1$id)/5)==0){nh1=1}
if(round(length(grap1$id)/5)>0){nh1=round(length(grap1$id)/5)}
if(round(length(grap2$id)/5)==0){nh2=1}
if(round(length(grap2$id)/5)>0){nh2=round(length(grap2$id)/5)}
##Tirage dans les grappes
s.2=c(sample(grap1$id,nh1),sample(grap2$id,nh2))
echant3.2=NULL
echant3.2[1]=temps[df$id==s.2[1]]
for(i in 2:(length(s.2))){
  echant3.2=c(echant3.2,temps[df$id==s.2[i]])
}

ech[1:length(echant3.2),3]=echant3.2
temps.bar.plusdeg=c(temps.bar.plusdeg,mean(echant3))
var.plusdeg=c(var.plusdeg,var(echant3))
#tirage 4
s.1=NULL;s.2=NULL
s.1=sample(modal,2)
grap1=df[df$most_used==s.1[1],]
grap2=df[df$most_used==s.1[2],]
##taille de l'échantillon tiré dans les grappes
if(round(length(grap1$id)/5)==0){nh1=1}
if(round(length(grap1$id)/5)>0){nh1=round(length(grap1$id)/5)}
if(round(length(grap2$id)/5)==0){nh2=1}
if(round(length(grap2$id)/5)>0){nh2=round(length(grap2$id)/5)}
##Tirage dans les grappes
s.2=c(sample(grap1$id,nh1),sample(grap2$id,nh2))
echant4.2=NULL
echant4.2[1]=temps[df$id==s.2[1]]
for(i in 2:(length(s.2))){
  echant4.2=c(echant4.2,temps[df$id==s.2[i]])
}

ech[1:length(echant4.2),4]=echant4.2
temps.bar.plusdeg=c(temps.bar.plusdeg,mean(echant4))
var.plusdeg=c(var.plusdeg,var(echant4))
#tirage 5
s.1=NULL;s.2=NULL
s.1=sample(modal,2)
grap1=df[df$most_used==s.1[1],]
grap2=df[df$most_used==s.1[2],]
##taille de l'échantillon tiré dans les grappes
if(round(length(grap1$id)/5)==0){nh1=1}
if(round(length(grap1$id)/5)>0){nh1=round(length(grap1$id)/5)}
if(round(length(grap2$id)/5)==0){nh2=1}
if(round(length(grap2$id)/5)>0){nh2=round(length(grap2$id)/5)}
##Tirage dans les grappes
s.2=c(sample(grap1$id,nh1),sample(grap2$id,nh2))
echant5.2=NULL
echant5.2[1]=temps[df$id==s.2[1]]
for(i in 2:(length(s.2))){
  echant5.2=c(echant5.2,temps[df$id==s.2[i]])
}

ech[1:length(echant5.2),5]=echant5.2
temps.bar.plusdeg=c(temps.bar.plusdeg,mean(echant5))
var.plusdeg=c(var.plusdeg,var(echant5))
#tirage 6
s.1=NULL;s.2=NULL
s.1=sample(modal,2)
grap1=df[df$most_used==s.1[1],]
grap2=df[df$most_used==s.1[2],]
##taille de l'échantillon tiré dans les grappes
if(round(length(grap1$id)/5)==0){nh1=1}
if(round(length(grap1$id)/5)>0){nh1=round(length(grap1$id)/5)}
if(round(length(grap2$id)/5)==0){nh2=1}
if(round(length(grap2$id)/5)>0){nh2=round(length(grap2$id)/5)}
##Tirage dans les grappes
s.2=c(sample(grap1$id,nh1),sample(grap2$id,nh2))
echant6.2=NULL
echant6.2[1]=temps[df$id==s.2[1]]
for(i in 2:(length(s.2))){
  echant6.2=c(echant6.2,temps[df$id==s.2[i]])
}

ech[1:length(echant6.2),6]=echant6.2
temps.bar.plusdeg=c(temps.bar.plusdeg,mean(echant6))
var.plusdeg=c(var.plusdeg,var(echant6))
#tirage 7
s.1=NULL;s.2=NULL
s.1=sample(modal,2)
grap1=df[df$most_used==s.1[1],]
grap2=df[df$most_used==s.1[2],]
##taille de l'échantillon tiré dans les grappes
if(round(length(grap1$id)/5)==0){nh1=1}
if(round(length(grap1$id)/5)>0){nh1=round(length(grap1$id)/5)}
if(round(length(grap2$id)/5)==0){nh2=1}
if(round(length(grap2$id)/5)>0){nh2=round(length(grap2$id)/5)}
##Tirage dans les grappes
s.2=c(sample(grap1$id,nh1),sample(grap2$id,nh2))
echant7.2=NULL
echant7.2[1]=temps[df$id==s.2[1]]
for(i in 2:(length(s.2))){
  echant7.2=c(echant7.2,temps[df$id==s.2[i]])
}

ech[1:length(echant7.2),7]=echant7.2
temps.bar.plusdeg=c(temps.bar.plusdeg,mean(echant7))
var.plusdeg=c(var.plusdeg,var(echant7))
#tirage 8
s.1=NULL;s.2=NULL
s.1=sample(modal,2)
grap1=df[df$most_used==s.1[1],]
grap2=df[df$most_used==s.1[2],]
##taille de l'échantillon tiré dans les grappes
if(round(length(grap1$id)/5)==0){nh1=1}
if(round(length(grap1$id)/5)>0){nh1=round(length(grap1$id)/5)}
if(round(length(grap2$id)/5)==0){nh2=1}
if(round(length(grap2$id)/5)>0){nh2=round(length(grap2$id)/5)}
##Tirage dans les grappes
s.2=c(sample(grap1$id,nh1),sample(grap2$id,nh2))
echant8.2=NULL
echant8.2[1]=temps[df$id==s.2[1]]
for(i in 2:(length(s.2))){
  echant8.2=c(echant8.2,temps[df$id==s.2[i]])
}

ech[1:length(echant8.2),8]=echant8.2
temps.bar.plusdeg=c(temps.bar.plusdeg,mean(echant8))
var.plusdeg=c(var.plusdeg,var(echant8))
#tirage 9
s.1=NULL;s.2=NULL
s.1=sample(modal,2)
grap1=df[df$most_used==s.1[1],]
grap2=df[df$most_used==s.1[2],]
##taille de l'échantillon tiré dans les grappes
if(round(length(grap1$id)/5)==0){nh1=1}
if(round(length(grap1$id)/5)>0){nh1=round(length(grap1$id)/5)}
if(round(length(grap2$id)/5)==0){nh2=1}
if(round(length(grap2$id)/5)>0){nh2=round(length(grap2$id)/5)}
##Tirage dans les grappes
s.2=c(sample(grap1$id,nh1),sample(grap2$id,nh2))
echant9.2=NULL
echant9.2[1]=temps[df$id==s.2[1]]
for(i in 2:(length(s.2))){
  echant9.2=c(echant9.2,temps[df$id==s.2[i]])
}

ech[1:length(echant9.2),9]=echant9.2
temps.bar.plusdeg=c(temps.bar.plusdeg,mean(echant9))
var.plusdeg=c(var.plusdeg,var(echant9))
#tirage 10
s.1=NULL;s.2=NULL
s.1=sample(modal,2)
grap1=df[df$most_used==s.1[1],]
grap2=df[df$most_used==s.1[2],]
##taille de l'échantillon tiré dans les grappes
if(round(length(grap1$id)/5)==0){nh1=1}
if(round(length(grap1$id)/5)>0){nh1=round(length(grap1$id)/5)}
if(round(length(grap2$id)/5)==0){nh2=1}
if(round(length(grap2$id)/5)>0){nh2=round(length(grap2$id)/5)}
##Tirage dans les grappes
s.2=c(sample(grap1$id,nh1),sample(grap2$id,nh2))
echant10.2=NULL
echant10.2[1]=temps[df$id==s.2[1]]
for(i in 2:(length(s.2))){
  echant10.2=c(echant10.2,temps[df$id==s.2[i]])
}
ech[1:length(echant10.2),10]=echant10.2
temps.bar.plusdeg=c(temps.bar.plusdeg,mean(echant10))
var.plusdeg=c(var.plusdeg,var(echant10))
##Intervalles de confiance
for(i in 1:10){
  IC2deg[i,]=c(temps.bar.plusdeg[i]- 1.96*sqrt(var(temps.bar.plusdeg)/sum(ech[,i]!=0)),temps.bar.plusdeg[i]+1.96*sqrt(var(temps.bar.plusdeg)/sum(ech[,
                                                                                                                                 i]!=0)))
}
longueur2deg=IC2deg[,2]-IC2deg[,1]
moy_temps.bar.plusdeg=mean(temps.bar.plusdeg)
###Résultats
##Les IMC dans les 10 échantillons tirés
ech
##Les estimateurs de la moyenne dans les 10 échantillons tirés
temps.bar.plusdeg
##Les estimateurs de la variance dans les 10 échantillons tirés
var.plusdeg
##Les 10 intervalles de confiances de la moyenne sont
IC2deg
##Longueurs des Intervalles de Confiance
longueur2deg
##Représentation des IC
plot(seq(1,10,by=1),IC[,1],col="red",ylim=c(min(min(IC[,2]),min(IC[,1])),max(max(IC[,2]),max(IC[,1])))
)
lines(seq(1,10,by=1),IC[,1],col="blue")
points(seq(1,10,by=1),IC[,2],col="red")
lines(seq(1,10,by=1),IC[,2],col="blue")
##La moyenne des 10 estimateurs de la moyenne est
moy_temps.bar.plusdeg



longueur2deg
longueurgrap
longueurpear
longueurpesr
longueurpiar
longueurpisr
longueurstrat1
longueurstrat2
longueurstrat3



plot.new() 
par(mar=c(4,4,3,5)) 
plot(longueur2deg ,col="blue",axes=F,xlab="",ylab="") 
axis(2, ylim=c(0,20),col="blue",col.axis="blue",at=seq(0, 19, by=1)) 
mtext("Axe de la courbe bleue",side=2,line=2.5,col="blue")  
par(new = T) 
plot(longueurgrap ,col="red",axes=F,xlab="",ylab="",ylim=c(20,40)) 
axis( 4 ,col="red",col.axis="red",at=seq(20, 40, by=5)) 
mtext("Axe de la courbe rouge",side=4,line=2.5,col="red") 



