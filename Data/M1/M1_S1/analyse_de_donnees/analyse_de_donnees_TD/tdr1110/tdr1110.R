###              TDR 1110                ###
### CLUSTERING OU CLASSIFICATION AVANCÉE ###

library(ade4)
library(adegraphics)


me <- read.table("http://pbil.univ-lyon1.fr/R/donnees/mortality_Europe.txt", h=TRUE)
mame <- me[,c(5:9,11:24)]


#############################################
###  Mesure de la qualité d'une partition ###
#############################################

rownames(mame) <- me$Code
dmame <- dist(mame)

classif <- hclust(d = dmame, method = "ward.D2")

plot(classif, main="Dendrogramme des pays \n methode de Ward", xlab = "Les pays",
     sub = "")
abline(h = 250, col = "red", lwd = 1.5)


parti <- cutree(classif, h = 250)
parti

# CRITÈRE DU COUDE

plot(rev(classif$height), type = 'l', main = "hauteurs du dendrogramme décroissantes",
     ylab = "classif$height", xlab = "", las = 1)

points(1:length(classif$height), rev(classif$height), pch = 20)



diff_coude <- diff(rev(classif$height),lag=1,differences = 2)
plot(diff_coude)
lines(diff_coude)

x <- c(1, 2, 3, 4)




# Coefficient multiple R2

RSQ <- rep(0, nrow(mame))
sum(scale(mame, scale = FALSE)^2) -> SQTot

for (i in 1:nrow(mame)) {
  Cla <- as.factor(cutree(classif, i))
  sum(t((t(sapply(1:ncol(mame), function(i) tapply(mame[,i], Cla, mean)))-
           apply(mame, 2, mean))**2) * as.vector(table(Cla)))/SQTot -> RSQ[i]
}

plot(RSQ, pch=20, main='Valeurs de R2')
lines(RSQ)


# PSEUDO F

n <- length(RSQ)
Pf <- rep(0, n)

 for (k in 1:n){
   Pf[k] <- RSQ[k]*(n-k)/((1-RSQ[k])*(k-1))  
 }

k <- 1:length(RSQ)
pf <- sapply(k, function(k){RSQ[k]*(n-k)/((1-RSQ[k])*(k-1))})


 pseudoF<-rep(0,n)
 pseudoF=c()
 for (i in 1:n) {
  Cla<-as.factor(cutree(classif,i))#choix d'avoir i groupes, chaque élément est identifié à son groupe
  pseudoF[i]=(RSQ[i]/(i-1))/((1-RSQ[i])/(n-i))
 }


plot(Pf, pch=20, main='Valeurs de Pf')
lines(Pf)
lines(pf)
lines(pseudoF)



#########################################################
### Adéquation et limites des stratégies d'agrégation ###
#########################################################

serp <- read.csv('serp.csv')[, 2:3]

plot(serp, pch=20)


# Ward

dev.new()
par(mfrow=c(2, 2))
cah <- hclust(dist(serp), method='ward.D2')
cah <- hclust(dist(serp), method='single')

# Coude
plot(rev(cah$height), pch=19, main='Coude')
lines(rev(cah$height))


# Coefficient multiple R2
RSQ <- rep(0, nrow(serp))
sum(scale(serp, scale = FALSE)^2) -> SQTot

for (i in 1:nrow(serp)) {
  Cla <- as.factor(cutree(cah, i))
  sum(t((t(sapply(1:ncol(serp), function(i) tapply(serp[,i], Cla, mean)))-
           apply(serp, 2, mean))**2) * as.vector(table(Cla)))/SQTot -> RSQ[i]
}
plot(RSQ, pch=20, main='Valeurs de R2')
lines(RSQ)

# Pseudo F
k <- 1:length(RSQ)
pf <- sapply(k, function(k){RSQ[k]*(n-k)/((1-RSQ[k])*(k-1))})
plot(pf, pch=20, main='Valeurs de Pf')
lines(pf)


# Points
k=3
plot(serp, col=rainbow(k)[cutree(cah, k=k)], main=paste('découpage en', k, 'classes'))


#############################################
###          Des Amas sphériques          ###
#############################################

set.seed(11101)
library(MASS)
C1 <- mvrnorm(120, c(0,0), matrix(c(.5,0,0,.5),2,2))
C2 <- mvrnorm(40, c(4,2), matrix(c(.15,0,0,.15),2,2))
C3 <- mvrnorm(40, c(4,-2), matrix(c(.15,0,0,.15),2,2))
C4 <- mvrnorm(40, c(-4,3), matrix(c(.15,0,0,.15),2,2))
P <- rbind(C1, C2, C3, C4)
plot(P, pch = 19, cex = 0.75, col = 'navy', main = 'Amas Sphériques',
     xlab = "", ylab = "", las = 1)


serp <- P


dev.new()
par(mfrow=c(2, 2))
cah <- hclust(dist(serp), method='ward.D2')
# cah <- hclust(dist(serp), method='single')

# Coude
plot(rev(cah$height), pch=19, main='Coude')
lines(rev(cah$height))


# Coefficient multiple R2
RSQ <- rep(0, nrow(serp))
sum(scale(serp, scale = FALSE)^2) -> SQTot

for (i in 1:nrow(serp)) {
  Cla <- as.factor(cutree(cah, i))
  sum(t((t(sapply(1:ncol(serp), function(i) tapply(serp[,i], Cla, mean)))-
           apply(serp, 2, mean))**2) * as.vector(table(Cla)))/SQTot -> RSQ[i]
}
plot(RSQ, pch=20, main='Valeurs de R2')
lines(RSQ)

# Pseudo F
k <- 1:length(RSQ)
pf <- sapply(k, function(k){RSQ[k]*(n-k)/((1-RSQ[k])*(k-1))})
plot(pf, pch=20, main='Valeurs de Pf')
lines(pf)


# Points
k=4
plot(serp, col=rainbow(k)[cutree(cah, k=k)], main=paste('Découpage en', k, 'classes'))


#############################################
###                Partie 3               ###
#############################################

library(ade4)
library(adegraphics)
library(parallel)

big <- read.csv(file('P200901_50000.csv'), sep = ';')



# Illustration de la complexité de hclust
T <- sapply(
  seq(from = 1000, to = 15000, by = 1000), 
  function(n) system.time({ cat(n,'\n'); 
    ind <- sort(sample(1:nrow(big), n)); 
    hclust(dist(big[ind,-1]), 
           'ward.D2'
    );
  }))
plot(seq(from = 1000, to = 15000, by = 1000), T[3,], pch = 20, las = 1, main = "Complexité en temps" )


# Régression linéaire
x <- seq(from = 1000, to = 15000, by = 1000)
M <- lm(T[3,] ~ I(x)+I(x^2)+I(x^3))


f <- function(n) return((M$coefficients[4]*n**3 + M$coefficients[3]*n**2 + M$coefficients[1]*n + M$coefficients[1])/(3600*24))






library(parallel)
makeCluster(detectCores(),type="PSOCK")->Clus2 # creation d'un cluster de process R 
clusterEvalQ(Clus2,ls()) # on check l'environnement de chacune des instances de R dans le cluster
big<-read.csv(file('P200901_50000.csv'),h=TRUE,sep=";") # lecture des donnees dans l'instance maitre du cluster
X<-big[1:15000,-56]
n<-100
clusterExport(Clus2,list("X","n")) # Export des variables X et n vers les esclaves
clusterEvalQ(Clus2,dim(X)) # Contrôle de la taille de X dans toutes les instances
system.time(0)->T2 # mesure du temps necessaire sur le maitre
system.time(parLapply(cl=Clus2,rep(n/detectCores(),detectCores()),function(n) kmeans(X,50,nstart=n,iter.max=100))->REP)->T3 # mesure du temps necessaire au cluster pour la réalisation des calculs
clusterCall(Clus2,function() library(parallel)) # chargement de la library parallel dans toutes les instances car la fonction detectCores va etre utilisée
clusterApply(cl=Clus2,1:detectCores(),function(n) kmeans(X,50,nstart=100/detectCores(),iter.max=100))->RES # 
library(snow)
snow.time(clusterApply(cl=Clus2,1:detectCores(),function(n) kmeans(X,50,nstart=100/detectCores(),iter.max=100))->RES)
stopCluster(Clus2)
