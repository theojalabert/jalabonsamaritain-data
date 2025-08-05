securite <- read.table("SecRoutiere0910.txt", header=TRUE)
head(securite)

library(ade4)
acpSR <- dudi.pca(securite[,5:10],center=TRUE, scale=TRUE, scannf=FALSE, nf=2)

## 1 / La sécurité routière dans les départements français
# 1.
summary(acpSR)

barplot(acpSR$eig, las=1, names.arg=1:6) # graphe des valeurs propres

screeplot(acpSR) # graphe des valeurs propres, pas besoin de préciser le nom de l'ACP (indique directement le nbr de valeurs propres à retenir en noir)

# 2.
s.corcircle(acpSR$co, xax=1, yax=2) # Pas obliger de mettre xax et yax car par défaut c'est 1 et 2 mais si on doit représenter l'axe 2 et 3 ça devient utile
?s.corcircle

s.corcircle(acpSR$co, xax=1, yax=2, fullcircle=FALSE) # permet de réduire la zone à l'étude en question
# effet de taille = effet de proportionnalité
"Toutes les variables se projette de manière orthogonale sur l'axe 1 : ça veut dire que l'on a un effet de taille (size effect). ie un effet global. L'axe horizontal est l'axe d'accident de la route tout confondu.
Plus la variable est proche de 1, plus la variable intervient dans la relation
Axe vertical : specifité : le nombre de tué.
En ACP, on interprète jamais ce qu'il se passe au centre(erreur de projection)"

# 3.
s.label(acpSR$li) # rajout de la Corse => les numéros de départements ont été augmentés de 1 car cela affiche le numéro de la ligne

s.label(acpSR$li, label=securite$departement, clabel=0.5) # Paris est en haut => bcp d'accidents mais pas mortels, or bouches-du-rhone moins d'accidents mais plus mortels

# 4.
scatter(acpSR)
scatter(acpSR, posieig="topright")

# 5.
cor(acpSR$li[, 1], securite$population)
"La corrélation est positive, plus il y a du monde, plus il y a d'accident (la relation est positive car l'ACP a donné un résultat en négatif. Il faut regarder les données en fonction de l'ACP), la valeur est négative puisque sur le cercle de corrélation toutes les valeurs sont à gauche"

# 6.
par(mfrow=c(1,2))
s.label(acpSR$li,label=securite$numdep,clabel=0.75)
popu <- scale(securite$population,center=TRUE,scale=TRUE)
s.value(acpSR$li[,1:2],popu) # On voit bien qu'il y un biais lié à la population

## 2 / Le classement des vins par des juges

data(macon)
macon

dim(macon)

#Pour faire une typologie des juges, on utilise les juges en ligne dans une ACP centrée
tmacon <- t(macon)
head(tmacon)

apply(tmacon,1,sum)
apply(tmacon,2,sum)

acpMA <- dudi.pca(tmacon, center = TRUE, scale = FALSE) # Scale = false car on souhaite garder la variance

summary(acpMA)

s.corcircle(acpMA$co) # Attention grosse erreur, les variables doivent être normées les flèches ne peuvent pas dépasser du cercle de corrélation

# Lorsque l'ACP est normée il faut utiliser s.corcircle, lorsque l'ACP n'est pas normée il faut utiliser s.arrow

s.arrow(acpMA$co) # Donc si un juge se trouve à gauche, ça signifie qu'il n'a pas apprécié les vins G,F etc car il leur a mis des notes élevées donc mauvaise.

s.label(acpMA$li) # positions des juges
scatter(acpMA, posieig="none")

# ACP normée
acpn <- dudi.pca(macon)

acpn <- dudi.pca(macon, scannf = FALSE, nf =2)
summary(acpn)

s.label(acpn$li)
s.corcircle(acpn$co)
