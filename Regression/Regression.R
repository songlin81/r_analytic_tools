library(ggplot2)
library(tidyr)

onedata <- read.csv("Regression/data/simple linear regression.csv")
ggplot(onedata,aes(x = x,y = y))+
  theme_bw()+
  geom_point(colour = "red")+
  geom_smooth(method='lm',formula=y~x)
summary(lm(y~x,data = onedata))

polydata <- read.csv("Regression/data/Polynomial regression.csv")
ggplot(polydata,aes(x=x,y = y))+geom_point()+theme_bw()

lmp3 <- lm(y~poly(x,3),data = polydata)
summary(lmp3)

poly3 <- predict(lmp3,polydata)

polydata$poly3 <- poly3 
lmp1 <- lm(y~poly(x,1),data = polydata)
poly1 <- predict(lmp1,polydata)
polydata$poly1 <- poly1
lmp2 <- lm(y~poly(x,2),data = polydata)
poly2 <- predict(lmp2,polydata)
polydata$poly2 <- poly2
lmp4 <- lm(y~poly(x,4),data = polydata)
poly4 <- predict(lmp4,polydata)
polydata$poly4 <- poly4
polydatalong <- gather(polydata,key="model",value="value",
                       c("poly1","poly2","poly3","poly4"))
ggplot(polydatalong)+theme_bw()+geom_point(aes(x,y))+
  geom_line(aes(x = x,y = value,linetype = model,colour  = model),size = 0.8)+
  theme(legend.position = c(0.1,0.8))

#######################################################
install.packages("ggcorrplot")
library(ggcorrplot)
library(tidyr)
library(GGally)
house <- read.csv("Regression/data/USA_Housing.csv")
head(house)
colnames(house)
summary(house)

houselong <- gather(house,key="varname",value="value",1:6)
ggplot(houselong)+theme_bw()+
  geom_density(aes(value),fill = "red",alpha = 0.5)+
  facet_wrap(.~varname,scales = "free")+
  theme(axis.text.x = element_text(angle = 30))

house_cor <- cor(house)
ggcorrplot(house_cor,method = "square",lab = TRUE)+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))

lm1 <- lm(AvgPrice~.,data = house)
summary(lm1)

lm2 <- lm(AvgPrice~AvgAreaIncome+AvgAreaHouseAge+AvgAreaNumberRooms
          +AreaPopulation,data = house)
summary(lm2)

ggcoef(lm2,exclude_intercept = T,vline_color = "red",
       errorbar_color = "blue",errorbar_height = 0.1)+
  theme_bw()

par(mfrow = c(2,2))
plot(lm2)

###################################################################
# Stepwise Regression
library(readxl)
library(GGally)
library(car)
install.packages("Metrics")
library(Metrics)

ENB <- read_excel("Regression/data/ENB2012.xlsx")
head(ENB)
summary(ENB)
str(ENB)

ggscatmat(ENB)+theme(axis.text.x = element_text(angle = 60))

set.seed(12)
index <- sample(nrow(ENB),round(nrow(ENB)*0.7))
trainEnb <- ENB[index,]
testENB <- ENB[-index,]
Enblm <- lm(Y1~.,data = trainEnb)
summary(Enblm)

kappa(Enblm,exact=TRUE)

alias(Enblm)

prelm <- predict(Enblm,testENB)
sprintf("MSE: %f",mse(testENB$Y1,prelm))

Enbstep <- step(Enblm,direction = "both")
summary(Enbstep)

kappa(Enbstep,exact=TRUE)
vif(Enbstep)

prestep <- predict(Enbstep,testENB)
sprintf("Mean Squared Error: %f",mse(testENB$Y1,prestep))


index <- order(testENB$Y1)
X <- sort(index)
Y1 <- testENB$Y1[index]
lmpre <- prelm[index]
steppre <- prestep[index]
plotdata <- data.frame(X = X,Y1 = Y1,lmpre =lmpre,steppre = steppre)
head(plotdata)
plotdata <- gather(plotdata,key="model",value="value",c(-X,-Y1))
ggplot(plotdata,aes(x = X))+theme_bw()+
  geom_point(aes(y = Y1),colour = "red",alpha = 0.5)+
  geom_line(aes(y = value,linetype = model,colour = model),size = 0.6)+
  theme(legend.position = c(0.1,0.8))

########################################################################

library(caret)
library(ROCR)
library(tidyr)
library(corrplot)

voice <- read.csv("Regression/data/voice.csv",stringsAsFactors = F)
head(voice)
summary(voice)
table(voice$label)
str(voice)

voice_cor <- cor(voice[,1:20])
ggcorrplot(voice_cor,method = "square")
corrplot.mixed(voice_cor,tl.col="black",tl.pos = "lt",
               tl.cex = 0.8,number.cex = 0.45)

plotdata <- gather(voice,key="variable",value="value",c(-label))
ggplot(plotdata,aes(fill = label))+
  theme_bw()+geom_density(aes(value),alpha = 0.5)+
  facet_wrap(~variable,scales = "free")


voice$label <- factor(voice$label,levels = c("male","female"),labels = c(0,1))
index <- createDataPartition(voice$label,p = 0.7)
voicetrain <- voice[index$Resample1,]
voicetest <- voice[-index$Resample1,]
voicelm <- glm(label~.,data = voicetrain,family = "binomial")
summary(voicelm)


voicelmstep <- step(voicelm,direction = "both")
summary(voicelmstep)
stepanova <- voicelmstep$anova
stepanova$Step <- as.factor(stepanova$Step)
ggplot(stepanova,aes(x = reorder(Step,-AIC),y = AIC))+
  theme_bw(base_family = "STKaiti",base_size = 12)+
  geom_point(colour = "red",size = 2)+
  geom_text(aes(y = AIC-1,label = round(AIC,2)))+
  theme(axis.text.x = element_text(angle = 30,size = 12))+
  labs(x = "Deleted attributes")

install.packages("forecast")
library(forecast)
voicelmpre <- predict(voicelm,voicetest,type = "response")
voicelmpre2 <- as.factor(ifelse(voicelmpre > 0.5,1,0))
voicesteppre <- predict(voicelmstep,voicetest,type = "response")
voicesteppre2 <- as.factor(ifelse(voicesteppre > 0.5,1,0))

table(voicetest$label,voicelmpre2)
table(voicetest$label,voicesteppre2)

pr <- prediction(voicelmpre, voicetest$label)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
prfdf <- data.frame(x = prf@x.values[[1]],logitic = prf@y.values[[1]])
## ROC
pr <- prediction(voicesteppre, voicetest$label)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
prfdf$logiticstep <- prf@y.values[[1]]
prfdf2 <- gather(prfdf,key="model",value="y",2:3)
ggplot(prfdf2,aes(x= x,y = y,colour = model,linetype = model))+
  theme_bw(base_family = "STKaiti")+geom_line(size = 1)+
  theme(aspect.ratio=1)+
  labs(x = "False Positive Rate",y = "True rate")

##################################################################
# Poisson Regression
install.packages("glmnet")
library(glmnet)

poi_sim <- read.csv("Regression/data/poisson_sim.csv")
poi_sim <- poi_sim[,2:4]
poi_sim$prog <- factor(poi_sim$prog,levels=1:3, 
                       labels=c("Average", "Academic", "Professional"))
hist(poi_sim$num_awards)

model <- glm(num_awards~.-1,data = poi_sim,family = poisson(link = "log"))
summary(model)
exp(coef(model))

##################################################################
# Ridge & Lasso Regression








