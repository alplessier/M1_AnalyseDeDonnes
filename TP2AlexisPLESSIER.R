library(factoextra)
library(FactoMineR)
library(gtsummary)
library(markdown)
library(readxl)

#Exercice 2 
Deca20 <- read_excel("C:/Users/Alexis/Downloads/Deca20.xlsx");
View(Deca20);
seq(2,nrow(Deca20),by=3);

dat20=Deca20[-seq(2,nrow(Deca20),by=3),];

dat20=as.data.frame(dat20);
colnames(dat20)[2];
colnames(dat20)[2]="Athlete";

#Exercice 3.1
dat20Point=dat20[seq(1,nrow(dat20),by=2),]
dat20Perf=dat20[-seq(1,nrow(dat20),by=2),]
dat20Point=as.data.frame(dat20Point)
dat20Perf=as.data.frame(dat20Perf)
View(dat20Point) ; View(dat20Perf);

#Exercice 3.2
dat20Perf[,"Athlete"]=dat20Point[,"Athlete"]
dat20Perf[,"Pays"]=dat20Point[,"Pays"]
dat20Perf[,"Total"]=dat20Point[,"Total"]
dat20Perf$Rang=1 :nrow(dat20Perf)
dat20Point$Rang=1 :nrow(dat20Perf)

#Exercice 3.3
dim(dat20Perf)
dimnames(dat20Perf);rownames(dat20Perf)
rownames(dat20Perf)=as.character(dat20Perf$Athlete)
rownames(dat20Point)=as.character(dat20Perf$Athlete)


#Exercice 4
dat20Perf=na.omit(dat20Perf)
dat20Point=na.omit(dat20Point)
View(dat20Perf) ;View(dat20Point)

#Exercice 5.1

dat20Perf0=dat20Perf
dat20Perf=dat20Perf[,-(1 :4)]
dat20Point0=dat20Point
dat20Point=dat20Point[,-(1 :4)]


#Exercice 5.2

str(dat20Point) ;str(dat20Perf)
str(dat20Perf$Poids)
dat20Perf$Poids
as.numeric(dat20Perf$Poids)
dat20Perf$Poids=as.numeric(dat20Perf$Poids)

#Exercice 6
dat20Perf=apply( dat20Perf,2,as.numeric)
dat20Point=apply(dat20Point,2,as.numeric)

#Exercice 5
dat20Perf[17,8] = 4.6
summary(dat20Perf)

#Exercice 7

#Enlever les Nan
dat20Perfm1500=dat20Perf[,-10]
dat20Perfm1500=na.omit(dat20Perfm1500)

apply(dat20Perfm1500,2,mean)
apply(dat20Perfm1500,2,median)
apply(dat20Perfm1500,2,IQR)
apply(dat20Perfm1500,2,function(x)quantile(x,0.1))
apply(dat20Perfm1500,2,function(x)quantile(x,0.9))


#Caractéristique de Dispersion
apply(dat20Perfm1500,2,sd) #Ecart type
CVx=apply(dat20Perfm1500,2,sd)/apply(dat20Perfm1500,2,mean)

#Ecart 1er et dernier quartile
apply(dat20Perfm1500,2,function(x)quantile(x,0.75)) - apply(dat20Perfm1500,2,function(x)quantile(x,0.25))

#Ecart entre le premier et dernier decile
apply(dat20Perfm1500,2,function(x)quantile(x,0.9)) - apply(dat20Perfm1500,2,function(x)quantile(x,0.1))


#Graphique
colnames(dat20Perf)[1]="100m"
boxplot(dat20Perf[,1],horizontal=T)
title(main="performance au 100 m")
hist(dat20Perf[,1],prob=T)
lines(density(dat20Perf[,1]),col= "red")

#Exercice 8

#Longueur
colnames(dat20Perf)[2]="Long."
boxplot(dat20Perf[,"Long."],horizontal=T)
title(main="performance au saut en longueur")
hist(dat20Perf[,"Long."],prob=T)
lines(density(dat20Perf[,"Long."]),col= "red")

#Poids
colnames(dat20Perf)[3]="Poids"
boxplot(dat20Perf[,"Poids"],horizontal=T)
title(main="performance au lancé de Poids")
hist(dat20Perf[,"Poids"],prob=T)
lines(density(dat20Perf[,"Poids"]),col= "red")

#Haut
colnames(dat20Perf)[4]="Haut."
boxplot(dat20Perf[,4],horizontal=T)
title(main="performance au saut en Hauteur")
hist(dat20Perf[,4],prob=T)
lines(density(dat20Perf[,4]),col= "red")


