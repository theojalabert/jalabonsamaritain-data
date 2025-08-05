### Chargement des libraries 
#-----------
library(dplyr)
library(plotly)
library(prophet)
library(ggplot2)
library(lubridate)
library(copula)
library(stats)
#-----

#telechargement des données 
#-----
dat<-read.table("C:/Users/medze/OneDrive/Documents/R/cac_40.csv",header=T, sep=',')
temp<-read.table("C:/Users/medze/OneDrive/Documents/Projet/Projet ERM/climat_data_paris.csv",
                 header = TRUE,sep=';')
#----
#preprocessing
#------
dat$Date<-as.Date(dat$Date,format = "%Y-%m-%d")
dat$Open<-as.numeric(dat$Open)
dat$High<-as.numeric(dat$High)
dat$Low<-as.numeric(dat$Low)
dat$Close<-as.numeric(dat$Close)
dat <- dat %>%
  mutate(ratio = log(lag(Open) / lead(Open)))
str(dat)
ggplot(dat, aes(x = ratio)) +
  geom_histogram(aes(y = ..density..), bins = 30, color = "black", fill = "skyblue", alpha = 0.7) +
  geom_density(color = "red") +
  labs(title = "Performance du CAC 40", x = "rendement logarithmique", y = "Densité")

temp$Date<-as.Date(temp$DATE,format = "%d/%m/%Y")
temp$TEMPERATURE_NOON_C<-as.numeric(temp$TEMPERATURE_NOON_C)


temp <- temp%>%
  rename(ds=Date,
         y=TEMPERATURE_NOON_C)
temp.p<-temp%>%
  select(ds,y)
View(temp)
str(temp)
plot_ly(data=temp.p,x=~ds,y=~y,type='scatter',color='red')


plot(temp$ds,temp$y)

#forcasting
#Forcasting
#-----
m<- prophet(temp.p,daily.seasonality=F,interval.width=0.99)

future <- make_future_dataframe(m, periods = 5*365)
forecast <- predict(m, future)
View(forecast)
plot(m, forecast)
forecast$trend
prophet_plot_components(m, forecast)
t<-forecast$trend
#----
# Correlation
#----
couple<-inner_join(forecast, dat, by = c("ds" = "Date"))%>%
  select(ds,trend,ratio)
couple<-na.omit(couple)
couple$Year <- year(couple$ds)
couple<-couple[couple$Year>2009,]
ggplot(couple, aes(x = trend, y = ratio)) +
  geom_point() +
  labs(x = "tendance", y = "rendement logarithmique", title = "")
cor(couple$trend,couple$ratio,method='kendall')
cor(couple$trend,couple$ratio,method='spearman')
cor(couple$trend,couple$ratio,method='pearson')
qqplot(couple$trend,couple$ratio)

correlation_by_year <- couple %>%
  group_by(Year) %>%
  summarize(correlation_value = cor(trend, ratio, use = "pairwise.complete.obs"))

print(correlation_by_year)



date0=as.Date("2023-10-31")
#-----
#quantification

#-----

quantif<- function(n,copula_dist){
  moy=0
  var=0
  for(i in 1:n){
  df0<-rMvdc(5*365,copula_dist)
  resultat <- as.data.frame(df0) %>%
    filter(df0[,1]>forecast[forecast$ds>date0,]$trend_upper) %>%
    summarize(moyenne = mean(df0[,2]),variance=var(df0[,2]))
  moy<-moy+resultat[1,1]
  var<-var+resultat[1,2]
  }
  moy<-moy/n
  var<-var/n
  return(c(moy,var))
}
tracer<-function(n){
  moyenne<-c()
  variance<-c()
  for (i in seq(1,n,by=15)){
    res<-quantif(i,copula_dist = copula_dist)
    moyenne<-c(moyenne,res[1])
    variance<-c(variance,res[2])
  }
  par(mfrow=c(1,2))
  plot(seq(1,n,by=15 ),moyenne)
  plot(seq(1,n,by=15 ),variance)
}

# Copule de student

#----
# Copule de Student
#----
model.1<-couple[couple$Year>2009,c(2,3)]
model.1 <- pobs(as.matrix(model.1))
cor.test(model.1[,1], model.1[,2])


plot(model.1[,1], model.1[,2])
normalCop <- ellipCopula('t',dim=2)
fit.1 <- fitCopula(normalCop,model.1,method='ml')
fit.1
coef(fit.1)
rho.1<-0.001979093
df<-22.335888546 

copula_dist <- mvdc(copula=tCopula(rho.1,df=df,dim=2), margins=c("unif","norm"),
                    paramMargins=list(list(min=min(couple$trend), max=19),
                                      list(mean=mean(couple$ratio),sd=sd(couple$ratio))))
copula_dist

quantif(1,copula_dist = copula_dist)
#----

# Copule gaussienne
#----
model.2<-couple[couple$Year>2009,c(2,3)]
x<-(model.2[,1])
z<-(model.2[,2])
qqplot(x,y,main ='Rank- Rank plot',xlab= 'tendance',ylab=' rendement')
model.2 <- pobs(as.matrix(model.2))
cor.test(model.2[,1], model.2[,2])


plot(model.2[,1], model.2[,2])
normalCop <- normalCopula(dim=2)
fit.2<- fitCopula(normalCop,model.2,method='ml')
fit.2
coef(fit.2)
rho<-0.003999657


copula_dist <- mvdc(copula=normalCopula(rho,dim=2), margins=c("unif","norm"),
                    paramMargins=list(list(min=min(couple$trend), max=19),
                                      list(mean=mean(couple$ratio),sd=sd(couple$ratio))))
copula_dist


quantif(1,copula_dist = copula_dist)
#-----

#Comparaison des copules 
#----
AIC(fit.1)
AIC(fit.2)
BIC(fit.1)
BIC(fit.2)

u <- pobs(as.matrix(model.1$trend))
v <- pobs(as.matrix(model.1$ratio))

hist(u)
hist(v)
plot(u,v,type="p",pch=16,col="blue")
#------------------------
#Autocorrolation
#------------------------
temperature<-ts(temp$y)
acf(temperature,lag=1000)

tr<-ts(forecast$additive_terms)
acf(tr,lag=1000)



plot(correlation_by_year$Year,correlation_by_year$correlation_value)


correlation<-rep(0,14)
pl<-as.data.frame(correlation_by_year)%>% 
  mutate(z=correlation,color=factor(sample(letters[1:3], 14, replace = TRUE)))
ggplot(pl, aes(x = Year, y = correlation, size = correlation_value,color=color)) +
  geom_point(shape = 1)+ 
  scale_size_continuous(range = c(5, 15))+
  scale_color_manual(values = c("red", "blue", "green"))
#-----



quantif(100,copula_dist=,copula_dist)


# Extra
#-----
x<-diff(couple$trend)/couple$trend[-1]
hist(x)
ggplot(data.frame(x=x),aes(x=x))+geom_histogram()+
a<-arima(x,order=c(11,1,2))
plot(1:length(a$residuals),a$residuals)
View(a)
s<-predict(a)
acf(s$pred)
library(corrplot)
m<-data.frame(v=a$residuals,u=couple$ratio[-1])
k<-cor(m,method='kendall')
corrplot(k,method='shade')


model <- pobs(as.matrix(m))
cor.test(model[,1], model[,2])


plot(model[,1], model[,2])
normalCop <- normalCopula(dim=2)
fit<- fitCopula(normalCop,model,method='ml')
fit
coef(fit)
par(mfrow=c(1,2))
hist(m[,1],density=T,main='histogramme des résidus de 
     la température',labels='.')
hist(m[,2],density=T,main='histograme des rendements',labels='.')


hist(x)
qqplot(x,rnorm(length(x)))
