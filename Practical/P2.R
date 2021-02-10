# R Data Interfaces #

################################################################
### CSV Files ###

print(getwd())
setwd("C:/repo/2021/DataR/Roc/analytictool/Practical")
print(getwd())

data <- read.csv("data/input.csv")
print(data)

print(is.data.frame(data))
print(ncol(data))
print(nrow(data))

sal <- max(data$salary)
print(sal)

retval <- subset(data, salary == max(salary))
print(retval)

retval <- subset( data, dept == "IT")
print(retval)

info <- subset(data, salary > 600 & dept == "IT")
print(info)

retval <- subset(data, as.Date(start_date) > as.Date("2014-01-01"))
print(retval)

retval <- subset(data, as.Date(start_date) > as.Date("2014-01-01"))
write.csv(retval,"data/output.csv")
newdata <- read.csv("data/output.csv")
print(newdata)

# Drop X:
write.csv(retval,"data/outputX.csv", row.names = FALSE)
newdata <- read.csv("data/outputX.csv")
print(newdata)

################################################################
### Excel File ###

install.packages("xlsx")

# Verify the package is installed.
any(grepl("xlsx",installed.packages()))
# Load the library into R workspace.
library("xlsx")

data <- read.xlsx("data/excelInput.xlsx", sheetIndex = 1)
print(data)
city <- read.xlsx("data/excelInput.xlsx", sheetIndex = 2)
print(city)

################################################################
### Binary Files ###

data()
head(mtcars, 6)

result <- mtcars[, c("cyl","am","gear")]
head(result)
write.table(result, file = "data/mtcars.csv",row.names = TRUE, na = "", 
            col.names = NA, sep = ",")
new.mtcars <- read.table("data/mtcars.csv",sep = ",",header = TRUE,nrows = 5)
new.mtcars
colnames(new.mtcars)
write.filename = file("data/binmtcars.dat", "wb")
write.filename 
writeBin(colnames(new.mtcars), write.filename)
writeBin(c(new.mtcars$cyl,new.mtcars$am,new.mtcars$gear), write.filename)
close(write.filename)


read.filename <- file("data/binmtcars.dat", "rb")
column.names <- readBin(read.filename, character(),  n = 4)
column.names
read.filename <- file("data/binmtcars.dat", "rb")
bindata <- readBin(read.filename, integer(),  n = 4)
print(bindata)

#####################################################################
### XML Files ###

library("XML")
library("methods")
result <- xmlParse(file = "data/emp.xml")
print(result)

rootnode <- xmlRoot(result)
rootsize <- xmlSize(rootnode)
print(rootsize)
print(rootnode[1])
print(rootnode[[1]][[1]])
print(rootnode[[1]][[5]])
print(rootnode[[3]][[2]])

xmldataframe <- xmlToDataFrame("data/emp.xml")
print(xmldataframe)

#####################################################################
### JSON Files ###

install.packages("rjson")
library("rjson")

result <- fromJSON(file = "data/input.json")
print(result)

json_data_frame <- as.data.frame(result)
print(json_data_frame)

