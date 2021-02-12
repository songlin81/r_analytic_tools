################ Mean, Median and Mode ########################

x <- c(12,7,3,4.2,18,2,54,-21,8,-5)
result.mean <- mean(x)
print(result.mean)

x <- c(12,7,3,4.2,18,2,54,-21,8,-5)
# When trim = 0.3, 3 values from each end will be dropped from the calculations to find mean.
result.mean <- mean(x,trim = 0.3)
print(result.mean)

x <- c(12,7,3,4.2,18,2,54,-21,8,-5,NA)
result.mean <-  mean(x)
print(result.mean)
# Find mean dropping NA values.
result.mean <-  mean(x,na.rm = TRUE)
print(result.mean)


x <- c(12,7,3,4.2,18,2,54,-21,8,-5)
median.result <- median(x)
print(median.result)


getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

v <- c(2,1,2,3,1,2,3,4,1,5,5,3,2,3)
result <- getmode(v)
print(result)

charv <- c("o","it","the","it","it")
result <- getmode(charv)
print(result)

################ Linear Regression ########################
# One of these variable is called predictor variable whose value is gathered through experiments. 
# The other variable is called response variable whose value is derived from the predictor variable.
# y = ax + b
#   y is the response variable.
#   x is the predictor variable.
#   a and b are constants which are called the coefficients.

x <- c(151, 174, 138, 186, 128, 136, 179, 163, 152, 131)
y <- c(63, 81, 56, 91, 47, 57, 76, 72, 62, 48)
relation <- lm(y~x)
print(relation)
print(summary(relation))

a <- data.frame(x = 170)
result <-  predict(relation,a)
print(result)

png(file = "linearregression.png")
plot(y,x,col = "blue",main = "Height & Weight Regression",
     abline(lm(x~y)),cex = 1.3,pch = 16,xlab = "Weight in Kg",ylab = "Height in cm")
dev.off()


################ Multiple Regression ########################

# y = a + b1x1 + b2x2 +...bnxn
#   y is the response variable.
#   a, b1, b2...bn are the coefficients.
#   x1, x2, ...xn are the predictor variables.

input <- mtcars[,c("mpg","disp","hp","wt")]
print(head(input))
model <- lm(mpg~disp+hp+wt, data = input)
print(model)
cat("# # # # The Coefficient Values # # # ","\n")
a <- coef(model)[1]
print(a)
Xdisp <- coef(model)[2]
Xhp <- coef(model)[3]
Xwt <- coef(model)[4]
print(Xdisp)
print(Xhp)
print(Xwt)

# Y = 37.15+(-0.000937)*x1+(-0.0311)*x2+(-3.8008)*x2.91
# For a car with disp = 221, hp = 102 and wt = 2.91 the predicted mileage is ???
Y = a+(Xdisp)*221+(Xhp)*102+(Xwt)*2.91
Y


################ Logistic Regression ########################
# The Logistic Regression is a regression model in which the response variable (dependent variable) 
# has categorical values such as True/False or 0/1.
# The general mathematical equation for logistic regression is ???
# y = 1/(1+e^-(a+b1x1+b2x2+b3x3+...))
#   y is the response variable.
#   x is the predictor variable.
#   a and b are the coefficients which are numeric constants.
input <- mtcars[,c("am","cyl","hp","wt")]
print(head(input))
am.data = glm(formula = am ~ cyl + hp + wt, data = input, family = binomial)
print(summary(am.data))
# Conclusion
# In the summary as the p-value in the last column is more than 0.05 for the variables "cyl" and "hp", 
# we consider them to be insignificant in contributing to the value of the variable "am". 
# Only weight (wt) impacts the "am" value in this regression model.


################ Normal Distribution ########################
# R has four in built functions to generate normal distribution. They are described below.
#   dnorm(x, mean, sd)
#   pnorm(x, mean, sd)
#   qnorm(p, mean, sd)
#   rnorm(n, mean, sd)

#dnorm
# Create a sequence of numbers between -10 and 10 incrementing by 0.1.
x <- seq(-10, 10, by = .1)
# Choose the mean as 2.5 and standard deviation as 0.5.
y <- dnorm(x, mean = 2.5, sd = 0.5)
png(file = "dnorm.png")
plot(x,y)
dev.off()


#pnorm
# Create a sequence of numbers between -10 and 10 incrementing by 0.2.
x <- seq(-10,10,by = .2)
# Choose the mean as 2.5 and standard deviation as 2. 
y <- pnorm(x, mean = 2.5, sd = 2)
png(file = "pnorm.png")
plot(x,y)
dev.off()


#qnorm
# Create a sequence of probability values incrementing by 0.02.
x <- seq(0, 1, by = 0.02)
# Choose the mean as 2 and standard deviation as 3.
y <- qnorm(x, mean = 2, sd = 1)
png(file = "qnorm.png")
plot(x,y)
dev.off()


#rnorm
# Create a sample of 50 numbers which are normally distributed.
y <- rnorm(50)
# Give the chart file a name.
png(file = "rnorm.png")
hist(y, main = "Normal DIstribution")
dev.off()


###################### Binomial Distribution ######################################
# R has four in-built functions to generate binomial distribution. They are described below.
#   dbinom(x, size, prob)
#   pbinom(x, size, prob)
#   qbinom(p, size, prob)
#   rbinom(n, size, prob)


#dbinom
# Create a sample of 50 numbers which are incremented by 1.
x <- seq(0,50,by = 1)
# Create the binomial distribution.
y <- dbinom(x,50,0.5)
png(file = "dbinom.png")
plot(x,y)
dev.off()


#pbinom
# Probability of getting 26 or less heads from a 51 tosses of a coin.
x <- pbinom(26,51,0.5)
print(x)


#qbinom
# How many heads will have a probability of 0.25 will come out when a coin
# is tossed 51 times.
x <- qbinom(0.25,51,1/2)
print(x)


#rbinom
# Find 8 random values from a sample of 150 with probability of 0.4.
x <- rbinom(8,150,.4)
print(x)


###################### Poisson Regression ######################################
# Poisson Regression involves regression models in which the response variable is 
# in the form of counts and not fractional numbers. 
# For example, the count of number of births or number of wins in a football match series.

# The general mathematical equation for Poisson regression is ???
#    log(y) = a + b1x1 + b2x2 + bnxn.....
# y is the response variable.
# a and b are the numeric coefficients.
# x is the predictor variable.

input <- warpbreaks
print(head(input))

output <-glm(formula = breaks ~ wool+tension, data = warpbreaks, family = poisson)
print(summary(output))

# In the summary we look for the p-value in the last column to be less than 0.05 
# to consider an impact of the predictor variable on the response variable. 
# As seen the wooltype B having tension type M and H have impact on the count of breaks.


######################### Analysis of Covariance ###########################################

# ANCOVA Analysis

# The simple regression analysis gives multiple results for each value of the categorical variable. 
# In such scenario, we can study the effect of the categorical variable 
# by using it along with the predictor variable and comparing the regression lines 
# for each level of the categorical variable. Such an analysis is termed as Analysis of Covariance 
# also called as ANCOVA.

#    We create a regression model taking "hp" as the predictor variable 
#    and "mpg" as the response variable taking into account the interaction between "am" and "hp".


# Model with interaction between categorical variable and predictor variable
input <- mtcars[,c("am","mpg","hp")]
print(head(input))
result <- aov(mpg~hp*am,data = input)
print(summary(result))
# This result shows that both horse power and transmission type 
# has significant effect on miles per gallon as the p value in both cases is less than 0.05. 
# But the interaction between these two variables is not significant as the p-value is more than 0.05.


# Model without interaction between categorical variable and predictor variable
result <- aov(mpg~hp+am,data = input)
print(summary(result))
# This result shows that both horse power and transmission type has significant effect 
# on miles per gallon as the p value in both cases is less than 0.05.


# Comparing Two Models
#Now we can compare the two models to conclude if the interaction of the variables is truly in-significant. 
#For this we use the anova() function.
result1 <- aov(mpg~hp*am,data = input)
result2 <- aov(mpg~hp+am,data = input)
print(anova(result1,result2))
# As the p-value is greater than 0.05 we conclude that the interaction between horse power 
# and transmission type is not significant. So the mileage per gallon will depend in a similar manner 
# on the horse power of the car in both auto and manual transmission mode.


############################ Time Series Analysis #########################################

# Get the data points in form of a R vector.
rainfall <- c(799,1174.8,865.1,1334.6,635.4,918.5,685.5,998.6,784.2,985,882.8,1071)
# Convert it to a time series object.
rainfall.timeseries <- ts(rainfall,start = c(2012,1),frequency = 12)
print(rainfall.timeseries)
png(file = "rainfall.png")
plot(rainfall.timeseries)
dev.off()

rainfall1 <- c(799,1174.8,865.1,1334.6,635.4,918.5,685.5,998.6,784.2,985,882.8,1071)
rainfall2 <- 
  c(655,1306.9,1323.4,1172.2,562.2,824,822.4,1265.5,799.6,1105.6,1106.7,1337.8)
combined.rainfall <-  matrix(c(rainfall1,rainfall2),nrow = 12)
rainfall.timeseries <- ts(combined.rainfall,start = c(2012,1),frequency = 12)
print(rainfall.timeseries)
png(file = "rainfall_combined.png")
plot(rainfall.timeseries, main = "Multiple Time Series")
dev.off()


################################### Nonlinear Least Square ##########################################


























