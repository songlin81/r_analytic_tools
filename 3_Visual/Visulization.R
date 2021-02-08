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
index <- (athletedata$region %in% region30[1:20]&(!is.na(athletedata$Medal)))
plotdata <- athletedata[index,]
plotdata2 <- plotdata%>%group_by(Year,region,Medal)%>%
  summarise(Medalnum = n())
head(plotdata2)
plotdata2$Year <- as.integer(plotdata2$Year)
ggplot(plotdata2,aes(x=region,y=Medalnum,fill=Medal))+
  theme_bw()+
  geom_bar(stat = "identity",position = "stack")+
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5))+
  scale_fill_brewer(palette="RdYlGn")+
  transition_time(Year) +
  labs(title = 'Year: {frame_time}')

p2 <- ggplot(plotdata2[plotdata2$Year == 2000,],aes(x=region,y=Medalnum,fill=Medal))+
  theme_bw()+
  geom_bar(stat = "identity",position = "stack")+
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5))+
  scale_fill_brewer(palette="RdYlGn")+
  labs(title = 'Year: 2000')
p2

p3 <- ggplot(plotdata2[plotdata2$Year == 1996,],aes(x=region,y=Medalnum,fill=Medal))+
  theme_bw()+
  geom_bar(stat = "identity",position = "stack")+
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5))+
  scale_fill_brewer(palette="RdYlGn")+
  labs(title = 'Year: 1996')
p3

#############################################################################
install.packages("treemap")
library(treemap)
plotdata <- athletedata%>%
  group_by(region,Sex)%>%
  summarise(number=n())
## Calculate number of medals
plotdata2 <- athletedata[!is.na(athletedata$Medal),]%>%
  group_by(region,Sex)%>%
  summarise(Medalnum=n())
## Join data
plotdata3 <- inner_join(plotdata2,plotdata,by=c("region", "Sex"))

treemap(plotdata3,index = c("Sex","region"),vSize = "number",
        vColor = "Medalnum",type="value",palette="RdYlGn",
        title = "Number of athletes per country by gender",fontfamily.title = "STKaiti",
        title.legend = "# of medals",fontfamily.legend="STKaiti")


install.packages("maps")
install.packages("geosphere")
library(maps)
library(geosphere)
usaairline <- read.csv("Visual/data/usaairline.csv")
airportusa <- read.csv("Visual/data/airportusa.csv")
head(airportusa)
head(usaairline)

map("state",col="palegreen", fill=TRUE, bg="lightblue", lwd=0.1)

points(x=airportusa$Longitude, y=airportusa$Latitude, pch=19, cex=0.4,col="tomato")

col.1 <- adjustcolor("orange", alpha=0.4)

for(i in 1:nrow(usaairline)) {
  node1 <- usaairline[i,c("Latitude.x","Longitude.x")]
  node2 <- usaairline[i,c("Latitude.y","Longitude.y")]
  arc <- gcIntermediate( c(node1$Longitude.x, node1$Latitude.x),
                         c(node2$Longitude.y, node2$Latitude.y),
                         n=1000, addStartEnd=TRUE )
  lines(arc, col=col.1, lwd=0.2)
}


install.packages("igraph")
library(igraph)
vertexdata <- read.csv("Visual/data/vertex.csv")
edgedata <- read.csv("Visual/data/edge.csv")
head(vertexdata)
head(edgedata)

g <- graph_from_data_frame(edgedata,vertices = vertexdata,directed = TRUE)
E(g)$width <- log10(E(g)$connectnumber)
colrs <- c("gray50", "tomato", "gold")
V(g)$color <- colrs[V(g)$vtype]
E(g)$color <- colrs[E(g)$etype]
par(mfrow=c(2,2), mar=c(0,0,0,0),cex = 1) 
plot(g, layout =  layout_in_circle(g),
     edge.arrow.size=0.4,
     vertex.size = 10*log10(V(g)$airportnumber), 
     vertex.label.cex = 0.6)

plot(g, layout =  layout_with_fr(g),
     edge.arrow.size=0.4,
     vertex.size = 10*log10(V(g)$airportnumber), 
     vertex.label.cex = 0.6)

plot(g, layout =  layout_on_sphere(g),
     edge.arrow.size=0.4,
     vertex.size = 10*log10(V(g)$airportnumber), 
     vertex.label.cex = 0.6)
plot(g, layout =  layout_randomly(g),
     edge.arrow.size=0.4,
     vertex.size = 10*log10(V(g)$airportnumber), 
     vertex.label.cex = 0.6)

#############################################################################

install.packages("VennDiagram")
library(VennDiagram)

vcol <- c("red","blue","green","DeepPink")
T<-venn.diagram(list(First =c(1:30),
                     Second=seq(1,50,by = 2),
                     Third =seq(2,50,by = 2),
                     Four = c(20,70)),
                filename = NULL,lwd = 0.5,
                fill = vcol,alpha = 0.5,margin = 0.1)
grid.draw(T)

#############################################################################

install.packages("UpSetR")
library(UpSetR)

one  <- 1:100
two <- seq(1,200,by = 2)
three <- seq(10,300,by = 5)
four <- seq(2,400,by = 4)
five <- seq(10,500,by = 10)
six <- seq(3,400,by = 3)

all <- unique(c(one,two,three,four,five,six))
plotdata <- data.frame(matrix(nrow = length(all),ncol = 7))
colnames(plotdata) <-c("element","one","two","three","four","five","six")
plotdata[,1] <- all
for (i in 1:length(all)) {
  plotdata[i,2] <- ifelse(all[i] %in% one,1,0)
  plotdata[i,3] <- ifelse(all[i] %in% two,1,0)
  plotdata[i,4] <- ifelse(all[i] %in% three,1,0)
  plotdata[i,5] <- ifelse(all[i] %in% four,1,0)
  plotdata[i,6] <- ifelse(all[i] %in% five,1,0)
  plotdata[i,7] <- ifelse(all[i] %in% six,1,0)
}
head(plotdata)

upset(plotdata,
      sets = c("one","two","three","four","five","six"),
      nintersects = 40,
      order.by = "freq",
      matrix.color  = "black",
      main.bar.color = "red",
      sets.bar.color = "tomato",
      point.size  = 2.5,
      line.size = 0.5,
      mb.ratio = c(0.65, 0.35))

#############################################################################

install.packages("plot3D")
library(plot3D)

x <- y <- seq(0,10,by = 0.5)
xy <- mesh(x,y)
z <- sin(xy$x) + cos(xy$y) + sin(xy$x) * cos(xy$y)
par(mfrow = c(1,2))
hist3D(x,y,z,phi = 45, theta = 45,space = 0.1,colkey = F,bty = "g")
surf3D(xy$x,xy$y,z,colkey = F,border = "black",bty = "b2")

# Export to *.html to view the result (Save as web page)
plot_ly(x = xy$x, y = xy$y, z = z,showscale = FALSE)%>% 
  add_surface()
