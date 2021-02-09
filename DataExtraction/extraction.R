# CSV
csvdata <- read.csv("DataExtraction/data/Iris.csv",header = TRUE)
head(csvdata)
str(csvdata)

csvdata <- read.table("DataExtraction/data/Iris.csv",header = TRUE,sep = ",")
head(csvdata)

install.packages("readr")
library(readr)
csvdata <- read_csv("DataExtraction/data/Iris.csv",col_names = TRUE,
                    col_types = list("d","d","d","d","d","c"))
head(csvdata,2)

write_csv(csvdata,"DataExtraction/data/dump/IrisWrite_1.csv")
write.csv(csvdata,"DataExtraction/data/dump/IrisWrite_2.csv",quote = FALSE)

#################################################
# Excel
install.packages("readxl")
library(readxl)
exceldata <- read_excel("DataExtraction/data/Iris.xlsx",sheet = "Iris")
head(exceldata,2)
str(exceldata)

#################################################
# SPSS
install.packages("foreign")
library(foreign)
spssdata <- read.spss("DataExtraction/data/Iris_spss.sav",to.data.frame = TRUE)
head(spssdata,2)

install.packages("haven")
library(haven)
spssdata <- read_sav("DataExtraction/data/Iris_spss.sav")
head(spssdata,2)

#################################################
# SAS
sasdata <- read_sas("DataExtraction/data/iris.sas7bdat")
head(sasdata,2)

#################################################
# Stata
dtadata <- read_dta("DataExtraction/data/iris.dta")
head(dtadata,2)
dtadata <- read_stata("DataExtraction/data/iris.dta")
head(dtadata,2)

#################################################
# Matlab
install.packages("R.matlab")
library(R.matlab)
A <- matrix(1:27, ncol = 3)
#A2DF <- as.data.frame(A)
#A2DF
writeMat(con="DataExtraction/data/ABC.mat", x=A)
matdata <- readMat("DataExtraction/data/ABC.mat")
matdata
str(matdata)
head(matdata$x,2)

#################################################
# Image
install.packages("png")
library(png)
impng <- readPNG("DataExtraction/data/images/Rlogo.png")
r <- nrow(impng) / ncol(impng)
plot(c(0,1), c(0,r), type = "n", xlab = "", ylab = "", asp=1)
rasterImage(impng, 0, 0, 1, r) 

install.packages("imager")
library(imager)
imjpg <- load.image("DataExtraction/data/images/image.jpg")
imdim <- dim(imjpg)
plot(imjpg,xlim = c(1,width(imjpg)),ylim = c(height(imjpg), 1))

#################################################
# Scrawler
install.packages("XML")
library(XML)
fileURL <- "https://www.r-project.org/"
fileURLnew <- sub("https", "http", fileURL)
links <- getHTMLLinks(fileURLnew)
length(links)

install.packages("RCurl")
library(RCurl)
link1 <- "https://www.westmetall.com/en/markdaten.php?action=show_table&field=LME_Cu_cash"
html <- getURL(link1)
table1 <-readHTMLTable(html,stringsAsFactors = FALSE)
length(table1)
t1 <- table1[[1]]
head(t1)
t2 <- table1[[2]]
head(t2)


# Below will fail from proxy connection, run from MacBook instead
install.packages("rvest")
install.packages("stringr")
library(rvest)
library(stringr)
top250 <- read_html("https://movie.douban.com/top250")
title <-top250 %>% html_nodes("span.title") %>% html_text()
head(title)
title <- title[is.na(str_match(title,"/"))]
head(title)
score <-top250 %>% html_nodes("span.rating_num") %>% html_text()
filmdf <- data.frame(title = title,score = as.numeric(score))
term <-top250 %>% html_nodes("span.inq") %>% html_text()
filmdf$term <- term
head(filmdf)
# Above will fail from proxy connection, run from MacBook instead

#################################################
# NUll handling
myair <- read.csv("DataExtraction/data/airquality.csv")
dim(myair)
summary(myair)

install.packages("VIM")
library(VIM)
aggr(myair)

mynadata <- myair[!complete.cases(myair),]
dim(mynadata)
head(mynadata)
matrixplot(mynadata)

newdata <- na.omit(myair)
dim(newdata)
head(newdata)

# mean to fill the NAs
myair2 <- myair
myair2$Ozone[is.na(myair$Ozone)] <- mean(myair$Ozone,na.rm = TRUE)
which(is.na(myair2$Ozone))
head(myair2$Ozone)

# mode to fill the NAs
which(is.na(myair$Solar.R))
myair2$Solar.R[which(is.na(myair$Solar.R))] <- median(myair2$Solar.R,na.rm = TRUE)
which(is.na(myair2$Solar.R))

# previous/next to fill the NA
install.packages("zoo")
library(zoo)
myair2$Wind <- na.locf(myair$Wind)
myair2$Temp <- na.locf(myair$Temp,fromLast = TRUE)
which(is.na(myair2$Wind))
which(is.na(myair2$Temp))

naindex <- which(is.na(myair$Month))
newnamonth <- round((myair$Month[naindex-1] + myair$Month[naindex+1]) / 2)
myair2$Month[naindex] <- newnamonth
which(is.na(myair2$Month))

naindex <- which(is.na(myair$Day))
newnaday <- myair$Day[naindex-1] + 1
myair2$Day[naindex] <- newnaday
which(is.na(myair2$Day))

table(myair$Type)
install.packages("Hmisc")
library(Hmisc)
myair2$Type <- impute(myair$Type,"C")
aggr(myair2)

#################################################
# K-means
colnames(myair)
myair <- myair[,c(1:4)]
install.packages("DMwR")
library(DMwR)
myair2 <- knnImputation(myair,k=5,scale = TRUE,meth = "weighAvg")
aggr(myair2)

# Random Forest
install.packages("missForest")
library(missForest)
myair2 <- missForest(myair,ntree = 50)
myair2$ximp
myair2$OOBerror

# Monte Carlo
install.packages("mice")
library(mice)
impdta <- mice(myair,m = 5,method=c("norm.predict","pmm","rf","norm"))
summary(impdta)

#################################################
# Data Operations
install.packages("tidyr")
library(tidyr)
Iris <- read.csv("DataExtraction/data/Iris.csv",header = TRUE)
head(Iris,2)
str(Iris)
Irislong = gather(Iris,key="varname",value="value",SepalLengthCm:PetalWidthCm)
head(Irislong,2)

IrisWidth <- spread(Irislong,key="varname",value="value")
head(IrisWidth,2)
str(IrisWidth)


install.packages("reshape2")
library(reshape2)
Irislong = melt(Iris,id = c("Id","Species"),variable.name = "varname",
                value.name="value")
head(Irislong,2)
str(Irislong)

IrisWidth <- dcast(Irislong,Id+Species~varname)
head(IrisWidth,2)
str(IrisWidth)


# Data Category
install.packages("dplyr")
library(dplyr)
Irisgroup <- Iris%>%
  group_by(Species)%>%
  summarise(meanSL = mean(SepalLengthCm),
            medianSW = median(SepalWidthCm),
            sdPL = sd(PetalLengthCm),
            IQRPW = IQR(PetalWidthCm),
            num = n()) %>%
  arrange(desc(sdPL))%>%
  filter(num==50)%>%
  mutate(varPL = sdPL^2)

Irisgroup


# Data Standardization
Iris <- read.csv("DataExtraction/data/Iris.csv",header = TRUE)
Iris <- Iris[2:5]
head(Iris,2)
str(Iris)

Irisc <- scale(Iris,center = TRUE, scale = FALSE)
apply(Irisc,2,range)

Iriss <- scale(Iris,center = TRUE, scale = TRUE)
apply(Iriss,2,range)

minmax <- function(x){
  x <- (x-min(x))/(max(x)-min(x))
}
Iris01 <- apply(Iris,2,minmax)
apply(Iris01,2,range)


install.packages("caret")
library(caret)

center <- preProcess(Iris,method = "center")
Irisc <- predict(center,Iris)
head(Irisc,2)
apply(Irisc,2,range)

scal <- preProcess(Iris,method = c("center","scale"))
Iriss <- predict(scal,Iris)
head(Iriss,2)
apply(Iriss,2,range)

minmax01 <- preProcess(Iris,method = "range",rangeBounds = c(0,1))
Iris01 <- predict(minmax01,Iris)
apply(Iris01,2,range)

#################################################
# Data Separation
Iris <- read.csv("DataExtraction/data/Iris.csv",header = TRUE)
Iris <- Iris[2:6]
head(Iris,2)

num <- round(nrow(Iris)*0.7)
index <- sample(nrow(Iris),size = num)
index
Iris_train <- Iris[index,]
Iris_test <- Iris[-index,]
dim(Iris_train)
dim(Iris_test)

index = createDataPartition(Iris$Species,p=0.7)
Iris_train <- Iris[index$Resample1,]
Iris_test <- Iris[-index$Resample1,]
dim(Iris_train)
dim(Iris_test)

index2 <- createFolds(Iris$Species,k = 3)
index2

#################################################
# Data Description
iris <- read.csv("DataExtraction/data/Iris.csv")
apply(iris[,c(2:5)],2,mean)
apply(iris[,c(2:5)],2,median)

apply(iris[,c(2:5)],2,var)
apply(iris[,c(2:5)],2,sd)
apply(iris[,c(2:5)],2,mad)
apply(iris[,c(2:5)],2,sd) / apply(iris[,c(2:5)],2,mean)
apply(iris[,c(2:5)],2,quantile)
apply(iris[,c(2:5)],2,fivenum)
apply(iris[,c(2:5)],2,range)
apply(iris[,c(2:5)],2,IQR)

install.packages("moments")
library(moments)
apply(iris[,c(2:5)],2,skewness)
apply(iris[,c(2:5)],2,kurtosis)

library(ggplot2)
library(tidyr)
irislong = gather(iris[,c(2:5)],key="varname",
                  value="value",SepalLengthCm:PetalWidthCm)
ggplot(irislong,aes(colour = varname,linetype = varname))+
  theme_bw()+geom_density(aes(value),bw = 0.5)
ggplot(irislong,aes(colour = varname,fill = varname,linetype = varname))+
  theme_bw()+geom_density(aes(value),bw = 0.5,alpha = 0.4)

plot(density(iris$SepalWidthCm))
skewness(iris$SepalWidthCm)
#################################################
# Similarity
cor(iris[,c(2:5)])

library(dplyr)
newdata <- iris%>%group_by(Species)%>%
  summarise(SepalLengthMean = mean(SepalLengthCm),
            SepalWidthMean = mean(SepalWidthCm),
            PetalLengthMean = mean(PetalLengthCm),
            PetalWidthMean = mean(PetalWidthCm))

newdata 
rownames(newdata) <- newdata$Species
newdata
newdata$Species <- NULL
newdata

dist(newdata,method = "euclidean",upper = T,diag = T)

dist(newdata,method = "manhattan",upper = T,diag = T)

dist(newdata,method = "maximum",upper = T,diag = T)

dist(newdata,method = "canberra",upper = T,diag = T)

dist(newdata,method = "minkowski",upper = T,diag = T,p = 0.5)
