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

###################################################################################
install.packages("ggplot2")
install.packages("GGally")
install.packages("gridExtra")
install.packages("dplyr")
library(ggplot2)
library(GGally)
library(gridExtra)
library(dplyr)

p1 <- ggplot(iris,aes(x = PetalLengthCm,y = PetalWidthCm))+
  theme_bw(base_family = "STKaiti",base_size = 9)+
  geom_point(aes(colour = Species))+
  labs(title = "Scatter plot")
p1

p2 <- ggplot(iris,aes(x = Species,y = SepalLengthCm))+
  theme_gray(base_family = "STKaiti",base_size = 9)+
  geom_violin(aes(fill = Species),show.legend = F)+
  labs(title = "Violin plot")+
  theme(plot.title = element_text(hjust = 0.5))
p2

p3 <- ggplot(iris,aes(SepalWidthCm))+
  theme_minimal(base_family = "STKaiti",base_size = 9)+
  geom_density(aes(colour = Species,fill = Species),alpha = 0.5)+
  labs(title = "Density plot")+
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = c(0.8,0.8))
p3

p4 <- ggplot(iris,aes(x = SepalLengthCm,y = SepalWidthCm))+
  theme_classic(base_family = "STKaiti",base_size = 9)+
  geom_point(shape = 17)+
  geom_density_2d(linemitre = 5)+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("2 dimension density plot")
p4

grid.arrange(p1,p2,p3,p4,nrow = 2)

ggscatmat(data = iris[,2:6],columns = 1:4,color = "Species",alpha = 0.8)+
  theme_bw(base_family = "STKaiti",base_size = 10)+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("scattor matrix")

ggparcoord(data = iris[,2:6],columns = 1:4,
           groupColumn = "Species",scale = "center")+
  theme_bw(base_family = "STKaiti",base_size = 10)+
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")+
  ggtitle("Parallel plot")+labs(x = "")

ggparcoord(data = iris[,2:6],columns = 1:4,
           groupColumn = "Species",scale = "globalminmax",
           splineFactor = 50,order = c(4,1,2,3))+
  theme_bw(base_family = "STKaiti",base_size = 10)+
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")+
  ggtitle("Smooth Parallel plot")+labs(x = "")


library(readr)
athlete_events <- read_csv("Visual/data/athlete_events.csv")
noc_regions <- read_csv("Visual/data/noc_regions.csv")
athletedata <- inner_join(athlete_events,noc_regions[,1:2],by=c("NOC"="NOC"))
summary(athletedata)
head(athletedata)
str(athletedata)

plotdata <- athletedata%>%group_by(region)%>%
  summarise(number=n())%>%
  arrange(desc(number))

plotdata

ggplot(plotdata[1:30,],aes(x=reorder(region,number),y=number))+
  theme_bw(base_family = "STKaiti")+
  geom_bar(aes(fill=number),stat = "identity",show.legend = F)+
  coord_flip()+
  scale_fill_gradient(low = "#56B1F7", high = "#132B43")+
  labs(x="Region",y="# of athletes",title="# of athletes by region")+
  theme(axis.text.x = element_text(vjust = 0.5),
        plot.title = element_text(hjust = 0.5))


install.packages("RColorBrewer")
library(RColorBrewer)

## Top 30 regions
region30 <- athletedata%>%group_by(region)%>%
  summarise(number=n())%>%
  arrange(desc(number))

region30 <- region30$region[1:30]

region30

## Gender: top 15 regions, number of athletes
plotdata <- athletedata[athletedata$region %in%region30[1:15],]%>%
  group_by(region,Year,Sex)%>%
  summarise(number=n())

ggplot(data=plotdata, aes(x=Year,y=region)) + 
  theme_bw(base_family = "STKaiti") +
  geom_tile(aes(fill = number),colour = "white")+
  scale_fill_gradientn(colours=rev(brewer.pal(10,"RdYlGn")))+
  scale_x_continuous(breaks=unique( plotdata$Year)) +
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5))+
  facet_wrap(~Sex,nrow = 2)


install.packages("ggChernoff")
library(ggChernoff)

region6 <- c("USA","Germany","France" ,"UK","Russia","China")
index <- ((athletedata$region %in% region6)&(!is.na(athletedata$Medal))&(athletedata$Season=="Summer"))
plotdata <- athletedata[index,]
plotdata2 <- plotdata%>%group_by(Year,region)%>%
  summarise(Medalnum=n())

ggplot(plotdata2,aes(x=Year,y=Medalnum))+
  theme_bw(base_family = "STKaiti")+
  geom_line()+
  geom_chernoff(fill = 'goldenrod1')+
  facet_wrap(~region,ncol = 2)+
  labs(x="Event Year",y="# of Medals")


install.packages("gganimate")
install.packages("gifski")
library(gganimate)
library(gifski)

## ????????????????????????????????????????????????
index <- (athletedata$region %in% region30[1:20]&(!is.na(athletedata$Medal)))
plotdata <- athletedata[index,]

plotdata2 <- plotdata%>%group_by(Year,region,Medal)%>%
  summarise(Medalnum = n())
head(plotdata2)

