library(ade4)
Cr <- read.csv("crim15810b1.csv",header=TRUE)

acp = dudi.pca(Cr[,2:4],center=TRUE, scale=TRUE)

## Q3
# Effet taille uniquement sur l'axe 1
par(mfrow=c(1,2)) 
s.corcircle(acp$co,xax=1,yax=2) 

## Q4 
# 3 valeurs propres sans point d'inflexion => 3 plans factoriels 

## Q5
summary(acp)
# => Projected inertia (%):
#    Ax1     Ax2     Ax3 
#   90.625   6.867   2.508 

## Q6
min <- 1
id_min <- 0
for (j in 2:3){
  for (i in 1:9){
    if (cos(Cr[i,j])^2 <= min){
      min <- cos(Cr[i,j])^2
      id_min <- i
    }
  }
}

min
Cr[id_min,1]

## Q8 et Q9

test <- Cr[,-1]
testchi2 = chisq.test(test)
chi2 = as.numeric(testchi2$statistic) 

## Q10
library(ade4)

afc_q10 =dudi.coa(Cr,scannf=FALSE,nf=ncol(Cr)-1)
summary(afc_q10)


## Petit problème
Pb <- read.csv("liais5441ad.csv",header=TRUE)
X <- Pb[,1]
Q <- Pb[,2]

B <- boxplot(X~Q)

## GROUPE 1
S1 <- 0
N1 <- 0
V1 <- 0
for (i in 1:60){
  if (Pb[i,2] == 'G1'){
    S1 <- S1 + Pb[i,1]
    N1 <- N1+1
  }
}

Moy_1 = S1/N1

for (i in 1:60){
  if (Pb[i,2] == 'G1'){
    V1 <- V1 + (Pb[i,1]-Moy_1)^2
  }
}
Var_1 = V1/N1

## GROUPE 2

S2 <- 0
N2 <- 0
V2 <- 0
for (i in 1:60){
  if (Pb[i,2] == 'G2'){
    S2 <- S2 + Pb[i,1]
    N2 <- N2+1
  }
}

Moy_2 = S2/N2

for (i in 1:60){
  if (Pb[i,2] == 'G2'){
    V2 <- V2 + (Pb[i,1]-Moy_2)^2
  }
}
Var_2 = V2/N2

## GROUPE 3

S3 <- 0
N3 <- 0
V3 <- 0
for (i in 1:60){
  if (Pb[i,2] == 'G3'){
    S3 <- S3 + Pb[i,1]
    N3 <- N3+1
  }
}

Moy_3 = S3/N3

for (i in 1:60){
  if (Pb[i,2] == 'G3'){
    V3 <- V3 + (Pb[i,1]-Moy_3)^2
  }
}
Var_3 = V3/N3

## Pour tout
S <- 0
V <- 0
for (i in 1:60){
  S <- S + Pb[i,1]
}
Moy = S/60

for (i in 1:60){
  V <- V + (Pb[i,1]-Moy)^2
}
Var = V/60

coeff_cor_XQ = (20*(Moy_1-Moy)^2+20*(Moy_2-Moy)^2+20*(Moy_3-Moy)^2)/(60*Var)

# ou en utilisant les fonctions de R
(20*(mean((Pb[1:20,1]))-mean(Pb[,1]))^2+20*(mean((Pb[21:40,1]))-mean(Pb[,1]))^2+20*(mean((Pb[41:60,1]))-mean(Pb[,1]))^2)/((60*var(X))*(59/60))


## On va calculer le coeff de détermination pour s'entraîner
regPb = lm(X~Q)

SCT = 60*Var

SCR <- 0
for (i in 1:60){
  if (Pb[i,2] == "G1"){
    SCR <- SCR + (1.018-Moy)^2
  }
  if (Pb[i,2] == "G2"){
    SCR <- SCR + (1.294-Moy)^2
  }
  if (Pb[i,2] == "G3"){
    SCR <- SCR + (4.418-Moy)^2
  }
}

R = 1 - SCR/SCT

## Q8
1-sum(tapply(X,Q,function(X) (length(X)-1)*var(X)))/(length(X)-1*var(X)) # Résultat : 0.6415655
sum(table(Q)*(mean(X)-tapply(X,Q,mean))^2)/(length(X[-1])*var(X)) # Résultat : 0.91111
1-sum(tapply(X,Q,function(X) (length(X)-1)*var(X)))/(length(X[-1])*var(X)) # Résultat : 0.91111
1-sum(tapply(X,Q,function(X) length(X)*var(X)))/(length(X)*var(X)) # Résultat : 0.907991

## Q9

Ind_G1 <- (Q=="G1")+0

rapp_corG1 <- sum(table(Ind_G1)*(mean(X)-tapply(X,Ind_G1,mean))^2)/(length(X[-1])*var(X))

## Q10

Ind_G2 <- (Q=="G2")+0

rapp_corG2 <- sum(table(Ind_G2)*(mean(X)-tapply(X,Ind_G2,mean))^2)/(length(X[-1])*var(X))

## Q11
Ind_G3 <- (Q=="G3")+0

rapp_corG3 <- sum(table(Ind_G3)*(mean(X)-tapply(X,Ind_G3,mean))^2)/(length(X[-1])*var(X))

rapp_corG1 + rapp_corG2 + rapp_corG3

## Q12
Q3_G1 = 1.3784826
Max_G2 = 3.239099
X2 <- ifelse(X<=Q3_G1, "x1", ifelse(X<=Max_G2,"x2","x3"))

tc_X2_Q <- table(X2,Q)
testchi2_X2_Q = chisq.test(tc_X2_Q)
chi2_X2_Q = as.numeric(testchi2_X2_Q$statistic) 

## Q13
tc_X2_IndG1 <- table(X2,Ind_G1)
testchi2_X2_IndG1 = chisq.test(tc_X2_IndG1)
chi2_X2_IndG1 = as.numeric(testchi2_X2_IndG1$statistic) 

## Q14 
tc_X2_IndG2 <- table(X2,Ind_G2)
testchi2_X2_IndG2 = chisq.test(tc_X2_IndG2)
chi2_X2_IndG2 = as.numeric(testchi2_X2_IndG2$statistic) 

## Q15 
tc_X2_IndG3 <- table(X2,Ind_G3)
testchi2_X2_IndG3 = chisq.test(tc_X2_IndG3)
chi2_X2_IndG3 = as.numeric(testchi2_X2_IndG3$statistic) 

## Q16

n=sum(tc) #Nombre total d'individu 

I=nrow(tc) #Nombre de modalité pour la variable 1 (en ligne) 

J=ncol(tc) #Nombre de modalité pour la variable 2 (en colonne) 

ET = (I*J)/n # Effectif théorique

## Q18
# On peut créer une nouvelle indicatrice pour G1 et G2
Ind_G12 = (Q=="G1")+(Q=="G2")+0

# On fait les mêmes calculs 
tc_X2_IndG12 <- table(X2,Ind_G12)
testchi2_X2_IndG12 = chisq.test(tc_X2_IndG12)
chi2_X2_IndG12 = as.numeric(testchi2_X2_IndG12$statistic) 


## Q20
(dftc_X2_Q=data.frame(unclass(tc_X2_Q)))

library(ade4)
afc = dudi.coa(dftc_X2_Q,scannf=FALSE,nf=ncol(dftc_X2_Q)-1)



