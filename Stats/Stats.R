install.packages("ggplot2")
install.packages("gridExtra")
library(ggplot2)
library(gridExtra)

## standard nomal distribution
nor1<- rnorm(300,mean = 0,sd = 1)
## Poisson Distribution
pois <- rpois(300,lambda = 2)
## Uniform Distribution
unif1 <- runif(300,min = 0, max = 1)
## F distribution
f1 <- rf(300,10,10)

## Visualization
p1<- ggplot()+theme_bw(base_family = "STKaiti")+
  geom_density(aes(nor1),bw = 0.4,fill="red",alpha=0.4)+
  labs(x="",title = "standard nomal distribution")+
  theme(plot.title = element_text(hjust = 0.5))

p2 <- ggplot()+theme_bw(base_family = "STKaiti")+
  geom_density(aes(pois),bw = 0.8,fill = "red",alpha = 0.4)+
  labs(x="",title = "Poisson Distribution")+
  theme(plot.title = element_text(hjust = 0.5))

p3<- ggplot()+theme_bw(base_family = "STKaiti")+
  geom_histogram(aes(unif1),bins = 15,fill = "red",alpha = 0.4)+
  labs(x="",title = "Uniform Distribution")+
  theme(plot.title = element_text(hjust = 0.5))

p4<- ggplot()+theme_bw(base_family = "STKaiti")+
  geom_density(aes(f1),bw = 0.8,fill = "red",alpha = 0.4)+
  labs(x="",title = "F distribution")+
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(p1,p2,p3,p4,nrow=2)


install.packages("MASS")
library(MASS)
## Maximum-likelihood fitting of univariate distributions
fitdistr(nor1,densfun = "normal")
fitdistr(pois,densfun = "Poisson")

#####################################################################

install.packages("MASS")
install.packages("plotly")
library(MASS)
library(plotly)

## Multivariate Normal Distribution
set.seed(123)
sigma2 = matrix(c(10,3,3,4),2,2)
norm2d <- mvrnorm(n=800,mu=c(0,4),Sigma = sigma2)
norm2df <- as.data.frame(norm2d)
colnames(norm2df) <- c("x","y")
ggplot(norm2df,aes(x=x,y=y))+
  theme_bw(base_family = "STKaiti")+
  geom_point(colour="red")+
  geom_density2d()+
  labs(title = "binominal standard distribution")+
  theme(plot.title = element_text(hjust = 0.5))

kde <- kde2d(norm2df$x,norm2df$y,n = 50)
plot_ly(x = kde$x, y = kde$y, z = kde$z,type = "surface") # Export->Save as web page

cov(norm2df)
colMeans(norm2d)
apply(norm2d,2,mean)


install.packages("tmvtnorm")
library(tmvtnorm)
mlefit1 <- mle.tmvnorm(norm2d,lower = c(-Inf,-Inf), upper = c(+Inf,+Inf))
summary(mlefit1)

#################################################################
#Hypothesis Testing
install.packages("energy")
library(energy)

set.seed(12)
nordata<- rnorm(500,mean = 2,sd = 5)
ggplot()+theme_bw(base_family = "STKaiti")+
  geom_histogram(aes(nordata),stat = "density",bins = 40)+
  labs(x="",title = "Normal Distribution")+
  theme(plot.title = element_text(hjust = 0.5))

par(pty="s")
qqnorm(nordata, pch = 1, frame = FALSE)
qqline(nordata, col = "steelblue", lwd = 2)


install.packages("car")
library(car)
par(pty="s")
qqPlot(nordata,distribution="norm")


ks.test(x = nordata,"pnorm")

ks.test(x = nordata,"pnorm",mean = 2,sd=5)

pois <- rpois(100,lambda = 2)
ks.test(x = nordata,y = pois)


library(MASS)
set.seed(1234)
sigma2 = matrix(c(1,0,0,1),2,2)
norm2d <- mvrnorm(n=100,mu=c(0,0),Sigma = sigma2)
library(energy)
## H0: std dist.
mvnorm.etest(norm2d,R = 199)


install.packages("MVN")
library(MVN)
par(pty="s")
result <- mvn(norm2d, mvnTest = "mardia",multivariatePlot = "qq")
result$multivariateNormality
result$univariateNormality


Iris <- read.csv("Stats/data/Iris.csv",header = TRUE)
Iris <- scale(Iris[,2:5],center = T,scale = T)
par(pty="s")
irismvn <- mvn(Iris, mvnTest = "mardia",multivariatePlot = "qq")
irismvn$multivariateNormality
irismvn$univariateNormality

#################################################################
t1<- rnorm(100,mean = 0,sd = 4)
t.test(t1,mu = 0)

t2<- rnorm(100,mean = 4,sd = 4)
t.test(t1,t2,mu = 0)
t.test(t2,t1,mu = 4)

var.test(t1,t2)

t3 <- rnorm(100,mean = 4,sd = 8)
bartlett.test(list(t1,t2,t3))

library(car)
library(ggplot2)
testdata <- data.frame(x = c(t1,t2,t3),
                       group1 = c(rep(c("A","B","C"),c(100,100,100))))
leveneTest(x~group1,data = testdata)

ggplot(testdata,aes(x = group1,y = x))+theme_bw()+
  geom_violin(aes(fill = group1),alpha = 0.2)+
  geom_jitter(aes(colour = group1))+
  theme(legend.position = "none")+
  labs(y = "values")

##########################################################################
Iris <- read.csv("Stats/data/Iris.csv",header = TRUE)
Iris <- Iris[,2:5]
cor.test(Iris$SepalLengthCm,Iris$SepalWidthCm)


install.packages("psych")
library(psych)
result <- corr.test(Iris,method="pearson")
result$r
result$p


install.packages("Hmisc")
library(Hmisc)
sper <- rcorr(as.matrix(Iris),type = "pearson")
sper$r
sper$P


comcor <- function(data,minre1 = 0.8,minre2 = -0.8,maxp = 0.05,type = "pearson"){
  library(Hmisc)
  n <- ncol(data)
  sper <- rcorr(as.matrix(data),type = type)
  hang <- matrix(rownames(sper$r), nrow = n, ncol = n,byrow = FALSE)
  lie <- matrix(colnames(sper$r),nrow = n,ncol = n,byrow = TRUE)
  zuhe <- matrix(paste(hang,lie,sep = "~"),nrow = n)
  lowrel <- sper$r[lower.tri(sper$r)]
  lowp <- sper$P[lower.tri(sper$P)]
  lowzuhe <- zuhe[lower.tri(zuhe)]
  result<-data.frame(zuhe=lowzuhe,r = lowrel,p = lowp)
  index <- which((lowrel >=minre1 | lowrel <=minre2) & lowzuhe !=1 & lowp <= maxp)
  return(result[index,])
}
comcor(Iris,minre1 = 0.8,minre2 = -0.8,maxp = 0.05,type = "pearson")

#################################################################################
# Analysis of Variance 

Iris <- read.csv("Stats/data/Iris.csv",header = TRUE)
colnames(Iris)
boxplot(SepalWidthCm~Species,Iris)

library(gplots)
par(family="STKaiti")
plotmeans(SepalWidthCm~Species,Iris,col = "red",main = "")

irisaov <- aov(SepalWidthCm~Species,Iris)
summary(irisaov)

tky <- TukeyHSD(irisaov)
tky = as.data.frame(tky$Species)
tky$pair = rownames(tky)
ggplot(tky, aes(colour=cut(`p adj`, c(0, 0.01, 0.05, 1), 
                           label=c("p<0.01","p<0.05","Non-Sig")))) +
  theme_bw(base_family = "STKaiti",base_size = 16)+
  geom_hline(yintercept=0, lty="11", colour="grey30",size = 1) +
  geom_errorbar(aes(pair, ymin=lwr, ymax=upr), width=0.2,size = 1) +
  geom_point(aes(pair, diff),size = 2) +
  labs(colour="")+
  theme(axis.text.x = element_text(size = 14))


data(ToothGrowth)
ToothGrowth$dose <- factor(ToothGrowth$dose,levels = c(0.5, 1, 2),
                           labels = c("D0.5", "D1", "D2"))
str(ToothGrowth)

install.packages("ggpubr")
library("ggpubr")
ggviolin(ToothGrowth, x = "dose", y = "len", color = "supp",
         add = "dotplot",palette = c("red", "blue"))



