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


################################################################
###  ###

