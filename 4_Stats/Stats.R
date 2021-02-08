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


