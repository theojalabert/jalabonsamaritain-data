## TD 1 ADD / tdr1134 

## 1. LES DONNÉES
## 1.1 Les crimes violents aux U.S.A.
CSD <- read.csv("CrimeStateDate.csv", header=TRUE)

head(CSD)

args(read.csv)
help("read.csv")

## 1.2 La sécurité routière dans les départements français
SR0910 <- read.table("SecRoutiere0910.txt", header=TRUE)

names(SR0910)

## 1.3 Les clients d'une banque
install.packages('ade4')
library(ade4)
data(banque)
names(banque)

dim(banque)

## 2. Visualiser une variable
## 2.1 Variable quantitative

cv2005 <- CSD[CSD$Date=="2005",]
dim(cv2005)

crime <- cv2005$Crime_Violent
class(crime)

length(crime)

## 2.1.1 L'histogramme

hist(crime, col="lightblue", main="Nombre de Crimes Violents dans les 51 états américains")
# Commentaire 
# L'histogramme présente une courbe décroissante du nombre des crimes violents en fonction du nombre d'états concernés

hist(log(crime,base=10), col="green", main="Nbr de Crimes Violents dans les 51 états américains,échelle log")

# Commentaire
# Avec l'échelle logarithmique on obtient un histogramme avec une allure faisant pensant à une gaussienne.


## 2.1.2 Le graphe de Cleveland
par(mfrow=c(1,2))
dotchart(crime, main="Série Brute", pch=20)
dotchart(sort(crime), main="Série Ordonnée", pch=20)

par(mfrow=c(1,2))
dotchart(log(crime,base=10), main="log(crime) brute", pch=20)
dotchart(sort(log(crime,base=10)),main="Cleveland Graph log(crime)", pch=20)

## 2.1.3 La boîte à moustaches

par(mfrow=c(1,1))
boxplot(crime, col="lightblue", main="Nombre de Crimes Violents dans les 51  états américains")

# moustache supérieure
valmax <- quantile(crime)[[4]] + 1.5 * (quantile(crime)[[4]] - quantile(crime)[[2]])
valmax

#moustache inférieure
sort(crime)[41:51]

boxplot(crime, col="lightblue", horizontal=TRUE,
        main="Nombre de Crimes Violents dans les 51 états américains")

boxplot(log(crime,base=10), col="lightblue", horizontal=TRUE,
        main="Nbr de Crimes Violents dans les 51 états américains, echelle log")

quantile(log(crime,base=10)[[2]])
quantile(log(crime,base=10)[[4]])

# 2.2 Variable qualitative
class(banque$csp)
levels(banque$csp)
length(levels(banque$csp))



