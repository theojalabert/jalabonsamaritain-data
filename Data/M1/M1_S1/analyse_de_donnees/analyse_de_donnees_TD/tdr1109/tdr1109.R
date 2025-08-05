#### INTRODUCTION À LA CLASSIFICATION ####

library(ade4)
library(adegraphics)

## 2. DISTANCE ENTRE INDIVIDUS
me <- read.table("http://pbil.univ-lyon1.fr/R/donnees/mortality_Europe.txt", h=TRUE)
names(me)

mame <- me[,c(5:9,11:24)]
rownames(mame) <- me$Code
head(mame)

sqrt(sum((mame[1,]-mame[2,])^2))

dmame <- dist(mame)
as.matrix(dmame)[1,2]

# Pour l'Allemagne et la France
sqrt(sum((mame[8,]-mame[9,])^2))
# Vérification 
dmameAlFr <- dist(mame)
as.matrix(dmameAlFr)[8,9]

## 3. DISTANCES ET DENDOGRAMMES
# 3.2 EXEMPLES
w <- c(0,1,2.1,3.3)
w <- data.frame(w)
w

(dw <- dist(w))

as.matrix(dw)

# 1 - Lien simple
hclust(dw,"single")

res1 <- hclust(dw,"single")
names(res1)

unclass(res1)

plot(hclust(dw,"single"),hang=-1)

# 2 - Lien complet
unclass(hclust(dw,"complete"))

plot(hclust(dw,"complete"))

# 3 - Lien moyen
plot(hclust(dw,"average"),han=-1)
unclass(hclust(dw,"average"))

# 3.3 Exercice
# Q1
boxplot(mame)
boxplot(scale(mame))

# Q2

library(ade4)
library(adegraphics)

dev.new()

par(mfrow=c(2, 3));
k=8;

acp <- dudi.pca(mame, scale = FALSE, scannf = FALSE, nf=2)
plot(acp$li[, 1], acp$li[, 2], col=cutree(hclust(dist(mame), 'single'), k=k)+1, pch=19)
plot(acp$li[, 1], acp$li[, 2], col=cutree(hclust(dist(mame), 'complete'), k=k)+1, pch=19)
plot(acp$li[, 1], acp$li[, 2], col=cutree(hclust(dist(mame), 'average'), k=k)+1, pch=19)



acp <- dudi.pca(mame, scale = FALSE, scannf = FALSE, nf=2)
plot(acp$li[, 1], acp$li[, 2], col=cutree(hclust(dist(scale(mame)), 'single'), k=k)+1, pch=19)
plot(acp$li[, 1], acp$li[, 2], col=cutree(hclust(dist(scale(mame)), 'complete'), k=k)+1, pch=19)
plot(acp$li[, 1], acp$li[, 2], col=cutree(hclust(dist(scale(mame)), 'average'), k=k)+1, pch=19)


# Pas de différence en mode centrée-réduit

dev.new()

par(mfrow=c(1, 3));
plot(hclust(dist(mame),"single"))
plot(hclust(dist(mame),"complete"))
plot(hclust(dist(mame),"average"))

## 4. Une fonction de valuation particulière : le critère de Ward
# 4.2 Principe
x <- c(9,18,24,25,32,34,40)
y <- c(33,7,23,40,47,30,16)
(w <- cbind(x,y))
plot(w,type="n",asp=1, main = "Table 3.1 page 36")
text(w[,1],w[,2],1:7)

dw <- dist(w)
dw^2
hc1 <- hclust(dw^2,"ward.D2")
unclass(hc1)
plot(hc1,hang=-1)

