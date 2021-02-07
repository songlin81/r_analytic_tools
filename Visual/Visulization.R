iris <- read.csv("Visual/data/Iris.csv")
par(family = "STKaiti",pch = 17)
plot(iris$SepalLengthCm,iris$SepalWidthCm,
     type = "p",col = "red",main = "Scatter plot",
     xlab = "SepalLengthCm",ylab = "SepalWidthCm")

generateRPointShapes<-function(){
  oldPar<-par()
  par(font=2, mar=c(0.5,0,0,0))
  y=rev(c(rep(0.5,9),rep(1,9), rep(1.5,9)))
  x=c(rep(1:9,3))
  x = x[1:26]
  y = y[1:26]
  plot(x, y, pch = 0:25, cex=1.5, ylim=c(0,3), xlim=c(1,9.5), 
       axes=FALSE, xlab="", ylab="", bg="blue")
  text(x, y, labels=0:25, pos=3)
  par(mar=oldPar$mar,font=oldPar$font )
}
generateRPointShapes()

cl <- colors()
length(cl); cl[1:20]

par(family = "STKaiti",mfrow=c(2,2))
layout(matrix(c(1,2,3,3),2,2,byrow = TRUE))
hist(iris$SepalLengthCm,breaks = 20,col = "lightblue",main = "Histogram",
     xlab = "SepalLengthCm")
smoothScatter(iris$PetalLengthCm,iris$PetalWidthCm, nbin = 64,main = "Scatter plot",
              xlab = "PetalLengthCm",ylab = "PetalWidthCm")
boxplot(SepalLengthCm~Species,data = iris,main = "Box plot",ylab = "SepalLengthCm")






