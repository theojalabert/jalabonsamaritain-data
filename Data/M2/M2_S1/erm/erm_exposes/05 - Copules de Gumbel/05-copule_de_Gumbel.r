#########################################################
# librairies
#########################################################

#install.packages("gumbel")
library(gumbel)
library(tidyverse)
library(dplyr)
library(copula)


#########################################################
# 1-Observation des données pour définir la forme des lois marginales
#########################################################

Millau <- select(ERM.Millau, Date, ID.OMM.station, Vitesse.du.vent.moyen.10.mn)
Montpellier <- select(ERM.Montpellier, Date, ID.OMM.station, Vitesse.du.vent.moyen.10.mn)

# Fusionner les deux datasets par la colonne "date"
bdd_vent <- merge(Montpellier, Millau, by = "Date", all = TRUE)
colnames(bdd_vent)[colnames(bdd_vent) == "Vitesse.du.vent.moyen.10.mn.x"] <- "V_Montpellier"
colnames(bdd_vent)[colnames(bdd_vent) == "Vitesse.du.vent.moyen.10.mn.y"] <- "V_Millau"

# Utiliser na.omit() pour supprimer les lignes avec des valeurs manquantes
bdd_vent_2 <- na.omit(bdd_vent)

# Pour plus tard
bdd_red <- bdd_vent_2[, c(3, 5)]

Montpellier <- bdd_vent_2$V_Montpellier
Millau <- bdd_vent_2$V_Millau

# Histogrammes
par(mfcol=c(1,2))
hist(Montpellier, col="skyblue")
hist(Millau, col="skyblue")

# Calculer le quantile à 95 %
quantile_95Mo <- quantile(bdd_vent_2$V_Montpellier, 0.95)
quantile_95Mi <- quantile(bdd_vent_2$V_Millau, 0.95)

# Afficher le résultat
print(quantile_95Mo)
print(quantile_95Mi)

# Calculer le quantile à 99 %
quantile_99Mo <- quantile(bdd_vent_2$V_Montpellier, 0.99)
quantile_99Mi <- quantile(bdd_vent_2$V_Millau, 0.99)

# Afficher le résultat
print(quantile_99Mo)
print(quantile_99Mi)


#########################################################
# 2-Calibration des paramètres des marginales et de la copule
#########################################################

# estimation des paramÃ¨tres des lois marginales (supposÃ©es de loi gamma)
# et estimation du paramÃ¨tre de la copule en utilisant plusieurs mÃ©thodes
gumbel.IFM(x,y,marg = "gamma")
gumbel.MBE(x,y,marg = "gamma")
gumbel.EML(x,y,marg = "gamma")
estim = gumbel.EML(x,y,marg = "gamma")

# on utilise la derniÃ¨re mÃ©thode pour calibrer les paramÃ¨tres
beta1=estim[1]
omega1=estim[2]
beta2=estim[3]
omega2=estim[4]
alpha=estim[5]



#############
# 3-Simulation de nouvelles observations et calcul  du pay-off  (Montecarlo)
#############

N=4000 #nombre de simulation
n=1000 #nombre d'observation par simulation

K=40 #Franchise
L=200 #limite globale
K1=9.3 #seuil station 1
K2=9.8 #seuil station 2
L1=15 #limite station 1
L2=16 #limite station 2

Ct=c()
St=c()
for(j in 1:N){
  #création des echantillons
  uv=rgumbel(n,alpha)
  u=uv[,1]
  v=uv[,2]
  x=qgamma(u,shape=omega1,rate=beta1)
  y=qgamma(v,shape=omega2,rate=beta2)
  
  #calcul du pay-off
  I1=c()
  I2=c()
  for(i in 1:n){
    I1[i]=min(L1-K1,max(x[i]-K1,0))
    I2[i]=min(L2-K2,max(y[i]-K2,0))
  }
  St[j]=sum(I1,I2)/2
  Ct[j]=min(L-K,max(St[j]-K,0))
}
# moyenne des pay-off
mean(Ct)
# ecart type des pay-off
sd(Ct)
# VaR a 75% des pay-off
quantile(Ct,probs=0.75)
# VaR a 90% des pay-off
quantile(Ct,probs=0.90)


#############
# 4 comparaison des résultats avec une copule gaussienne
#############
#Initialisation de la copule gausienne
cop_gau  <-  ellipCopula(family = "normal", dim = 2, dispstr = "un")
#Calibrage paramètres des copules
fit_gau <- fitCopula(cop_gau, pobs(bdd_red), method = "ml") #Pour la copule Gaussienne
rho_gau <- coef(fit_gau)[1]
cop_gau  <-  ellipCopula(family = "normal", dim = 2, dispstr = "un", rho_gau)

#calculdu pay-off
Ct=c()
St=c()
for(j in 1:N){
  #simulation de la copule et des couples (X,Y) associées
  uv=rCopula(n, cop_gau)
  u=uv[,1]
  v=uv[,2]
  x=qgamma(u,shape=omega1,rate=beta1)
  y=qgamma(v,shape=omega2,rate=beta2)
  
  #calcul du pay-off
  I1=c()
  I2=c()
  for(i in 1:n){
    I1[i]=min(L1-K1,max(x[i]-K1,0))
    I2[i]=min(L2-K2,max(y[i]-K2,0))
  }
  St[j]=sum(I1,I2)/2
  Ct[j]=min(L-K,max(St[j]-K,0))
}
# moyenne des pay-off
mean(Ct)
# ecart type des pay-off
sd(Ct)
# VaR a 75% des pay-off
quantile(Ct,probs=0.75)
# VaR a 90% des pay-off
quantile(Ct,probs=0.90)