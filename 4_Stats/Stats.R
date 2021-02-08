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





