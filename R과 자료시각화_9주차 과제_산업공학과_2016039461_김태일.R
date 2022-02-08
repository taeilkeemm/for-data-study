##9주차과제_산업공학과_2016039461_김태일

#1)조건부그림

attach(iris)
setosa = iris[which(iris$Species == 'setosa'),]
versicolor = iris[which(iris$Species == 'versicolor'),]
virginica = iris[which(iris$Species == 'virginica'),]
par(mfrow=c(2,2))
layout(matrix(c(1,0,2,3), nrow=2, ncol=2, byrow=TRUE))

par(mar=c(0,0,0,0))
par(oma=c(6,6,6,6))
plot(setosa$Sepal.Length, setosa$Sepal.Width)
grid(col = "lightgray", lty = "dotted", lwd = par("lwd"))
plot(versicolor$Petal.Length, versicolor$Sepal.Width)
grid(col = "lightgray", lty = "dotted", lwd = par("lwd"))
plot(virginica$Petal.Length, virginica$Sepal.Width)
grid(col = "lightgray", lty = "dotted", lwd = par("lwd"))
title("Given : Species", outer = T)
mtext("Petal.Length", side=1, outer=T)
mtext("Petal.Width", side=2, outer=T)

#2) 3단 밀도 그림

par(mfrow=c(1,3))
par(mar=c(0,0,0,0))
par(oma=c(6,6,6,6))
plot(density(setosa$Sepal.Length), xlim=c(4,9),ylim=c(0,1.3))
mtext("setosa", side=3, outer=F)
plot(density(versicolor$Sepal.Length), xlim=c(4,9),ylim=c(0,1.3))
mtext("versicolor", side=3, outer=F)
plot(density(virginica$Sepal.Length), xlim=c(4,9),ylim=c(0,1.3))
mtext("virginica", side=3, outer=F)


#3) parallel그림
install.packages("MASS")
library('MASS')
par(mfrow=c(1,3))
par(mar=c(0,0,0,0)) 
par(oma=c(6,6,6,6))
parcoord(setosa[,c(1:4)])
mtext("setosa", side=3, outer=F)
parcoord(versicolor[,c(1:4)])
mtext("versicolor", side=3, outer=F)
parcoord(virginica[,c(1:4)])
mtext("virginica", side=3, outer=F)



