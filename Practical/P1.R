myString <- "Hello, World!"
print ( myString)


# v0cn140@CNTSNW10317253 MINGW64 /c/repo/2021/DataR/Roc/analytictool/Practical (main)
# $ RScript P1.R
# [1] "Hello, World!"


# R does not support multi-line comments 
# but you can perform a trick which is something as follows
if(FALSE) {
  "This is a demo for multi-line comments and it should be put inside either a 
      single OR double quote"
}

###############################################################################
### Date Types ###

# Vector object
v <- TRUE 
print(class(v))
v <- 23.5
print(class(v))
v <- 2L
print(class(v))
v <- 2+5i
print(class(v))
v <- "TRUE"
print(class(v))
v <- charToRaw("Hello")
print(class(v))

# Vectors
apple <- c('red','green',"yellow")
print(apple)
print(class(apple))

# list.
list1 <- list(c(2,5,3),21.3,sin)
print(list1)

# matrix.
M = matrix( c('a','a','b','c','b','a'), nrow = 2, ncol = 3, byrow = TRUE)
print(M)

# array.
a <- array(c('green','yellow'),dim = c(3,3,2))
print(a)

# Factor
apple_colors <- c('green','green','yellow','red','red','red','green')
factor_apple <- factor(apple_colors)
print(factor_apple)
print(nlevels(factor_apple))

# data frame.
BMI <- 	data.frame(
  gender = c("Male", "Male","Female"), 
  height = c(152, 171.5, 165), 
  weight = c(81,93, 78),
  Age = c(42,38,26)
)
print(BMI)

###############################################################################
### Variables ###

# Assignment using equal operator.
var.1 = c(0,1,2,3)           
# Assignment using leftward operator.
var.2 <- c("learn","R")   
# Assignment using rightward operator.   
c(TRUE,1) -> var.3           
print(var.1)
cat ("var.1 is ", var.1 ,"\n")
cat ("var.2 is ", var.2 ,"\n")
cat ("var.3 is ", var.3 ,"\n")

print(ls())
print(ls(pattern = "var")) 
print(ls(all.name = TRUE))

rm(var.3)
print(var.3)

rm(list = ls())
print(ls())

###############################################################################
### Operators ###

# Arithmetic Operators
v <- c(2,5.5,6)
t <- c(8, 3, 4)
print(v+t)
v <- c(2,5.5,6)
t <- c(8, 3, 4)
print(v-t)
v <- c(2,5.5,6)
t <- c(8, 3, 4)
print(v*t)
v <- c(2,5.5,6)
t <- c(8, 3, 4)
print(v/t)
v <- c(2,5.5,6)
t <- c(8, 3, 4)
print(v%%t)
v <- c(2,5.5,6)
t <- c(8, 3, 4)
print(v%/%t)
v <- c(2,5.5,6)
t <- c(8, 3, 4)
print(v^t)

# Relational Operators
v <- c(2,5.5,6,9)
t <- c(8,2.5,14,9)
print(v>t)
v <- c(2,5.5,6,9)
t <- c(8,2.5,14,9)
print(v < t)
v <- c(2,5.5,6,9)
t <- c(8,2.5,14,9)
print(v == t)
v <- c(2,5.5,6,9)
t <- c(8,2.5,14,9)
print(v<=t)
v <- c(2,5.5,6,9)
t <- c(8,2.5,14,9)
print(v>=t)
v <- c(2,5.5,6,9)
t <- c(8,2.5,14,9)
print(v!=t)

# Logical Operators
v <- c(3,1,TRUE,2+3i)
t <- c(4,1,FALSE,2+3i)
print(v&t)
v <- c(3,0,TRUE,2+2i)
t <- c(4,0,FALSE,2+3i)
print(v|t)
v <- c(3,0,TRUE,2+2i)
print(!v)
v <- c(3,0,TRUE,2+2i)
t <- c(1,3,TRUE,2+3i)
print(v&&t)
v <- c(0,0,TRUE,2+2i)
t <- c(0,3,TRUE,2+3i)
print(v||t)

# Assignment Operators
v1 <- c(3,1,TRUE,2+3i)
v2 <<- c(3,1,TRUE,2+3i)
v3 = c(3,1,TRUE,2+3i)
print(v1)
print(v2)
print(v3)
c(3,1,TRUE,2+3i) -> v1
c(3,1,TRUE,2+3i) ->> v2 
print(v1)
print(v2)

# Miscellaneous Operators
v <- 2:8
print(v) 
v1 <- 8
v2 <- 12
t <- 1:10
print(v1 %in% t) 
print(v2 %in% t) 
M = matrix(c(2,6,5,1,10,4), nrow = 2,ncol = 3,byrow = TRUE)
M
t(M)
t = M %*% t(M)
print(t)

##########################################################################
### Decision making ###
x <- 30L
if(is.integer(x)) {
  print("X is an Integer")
}

x <- c("what","is","truth")

if("Truth" %in% x) {
  print("Truth is found")
} else {
  print("Truth is not found")
}

x <- c("what","is","truth")
if("Truth" %in% x) {
  print("Truth is found the first time")
} else if ("truth" %in% x) {
  print("truth is found the second time")
} else {
  print("No truth found")
}

x <- switch(
  3,
  "first",
  "second",
  "third",
  "fourth"
)
print(x)

########################################################################
### Loops ###
v <- c("Hello","loop")
cnt <- 2
repeat {
  print(v)
  cnt <- cnt+1
  if(cnt > 5) {
    break
  }
}

v <- c("Hello","while loop")
cnt <- 2
while (cnt < 7) {
  print(v)
  cnt = cnt + 1
}

v <- LETTERS[1:4]
for (i in v) {
  print(i)
}

v <- LETTERS[1:6]
for (i in v) {
  
  if (i == "D") {
    next
  }
  print(i)
}

#############################################################################
### Functions ###

# Built-in Function
print(seq(32,44))
print(mean(25:82))
print(sum(41:68))

# User-defined Function
new.function <- function(a) {
  for(i in 1:a) {
    b <- i^2
    print(b)
  }
}
new.function(6)

new.function <- function(a = 3, b = 6) {
  result <- a * b
  print(result)
}
new.function()
new.function(9,5)

#############################################################################
### Strings ###

a <- 'Start and end with single quote'
print(a)
b <- "Start and end with double quotes"
print(b)
c <- "single quote ' in between double quotes"
print(c)
d <- 'Double quotes " in between single quote'
print(d)

a <- "Hello"
b <- 'How'
c <- "are you? "
print(paste(a,b,c))
print(paste(a,b,c, sep = "-"))

# Total number of digits displayed. Last digit rounded off.
result <- format(23.123456789, digits = 9)
print(result)
# Display numbers in scientific notation.
result <- format(c(6, 13.14521), scientific = TRUE)
print(result)
# The minimum number of digits to the right of the decimal point.
result <- format(23.47, nsmall = 5)
print(result)
# Format treats everything as a string.
result <- format(6)
print(result)
# Numbers are padded with blank in the beginning for width.
result <- format(13.7, width = 6)
print(result)
# Left justify strings.
result <- format("Hello", width = 8, justify = "l")
print(result)
# Justfy string with center.
result <- format("Hello", width = 8, justify = "c")
print(result)

result <- nchar("Count the number of characters")
print(result)

# Changing to Upper case.
result <- toupper("Changing To Upper")
print(result)
# Changing to lower case.
result <- tolower("Changing To Lower")
print(result)

# Extract characters from 5th to 7th position.
result <- substring("Extract", 5, 7)
print(result)

###################################################################
### Vectors ###

v <- 5:13
print(v)
# Creating a sequence from 6.6 to 12.6.
v <- 6.6:12.6
print(v)
# If the final element specified does not belong to the sequence then it is discarded.
v <- 3.8:11.4
print(v)

# Create vector with elements from 5 to 9 incrementing by 0.4.
print(seq(5, 9, by = 0.4))

# The logical and numeric values are converted to characters.
s <- c('apple','red',5,TRUE)
print(s)

# Accessing vector elements using position.
t <- c("Sun","Mon","Tue","Wed","Thurs","Fri","Sat")
u <- t[c(2,3,6)]
print(u)
# Accessing vector elements using logical indexing.
v <- t[c(TRUE,FALSE,FALSE,FALSE,FALSE,TRUE,FALSE)]
print(v)
# Accessing vector elements using negative indexing.
x <- t[c(-2,-5)]
print(x)
# Accessing vector elements using 0/1 indexing.
t <- c("Sun","Mon","Tue","Wed","Thurs","Fri","Sat")
y <- t[c(0,0,0,0,0,0,1)]
print(y)

# Create two vectors.
v1 <- c(3,8,4,5,0,11)
v2 <- c(4,11,0,8,1,2)
# Vector addition.
add.result <- v1+v2
print(add.result)
# Vector subtraction.
sub.result <- v1-v2
print(sub.result)
# Vector multiplication.
multi.result <- v1*v2
print(multi.result)
# Vector division.
divi.result <- v1/v2
print(divi.result)

# Vector Element Recycling
v1 <- c(3,8,4,5,0,11)
v2 <- c(4,11)
# V2 becomes c(4,11,4,11,4,11)
add.result <- v1+v2
print(add.result)
sub.result <- v1-v2
print(sub.result)

v <- c(3,8,4,5,0,11, -9, 304)
# Sort the elements of the vector.
sort.result <- sort(v)
print(sort.result)
# Sort the elements in the reverse order.
revsort.result <- sort(v, decreasing = TRUE)
print(revsort.result)
# Sorting character vectors.
v <- c("Red","Blue","yellow","violet")
sort.result <- sort(v)
print(sort.result)
# Sorting character vectors in reverse order.
revsort.result <- sort(v, decreasing = TRUE)
print(revsort.result)

##################################################################################
### Lists ###

# Create a list containing strings, numbers, vectors and a logical values.
list_data <- list("Red", "Green", c(21,32,11), TRUE, 51.23, 119.1)
print(list_data)

# Create a list containing a vector, a matrix and a list.
list_data <- list(c("Jan","Feb","Mar"), matrix(c(3,9,5,1,-2,8), nrow = 2),
                  list("green",12.3))
# Give names to the elements in the list.
names(list_data) <- c("1st Quarter", "A_Matrix", "A Inner list")
# Show the list.
print(list_data)
# Access the first element of the list.
print(list_data[1])
# Access the thrid element. As it is also a list, all its elements will be printed.
print(list_data[3])
# Access the list element using the name of the element.
print(list_data$A_Matrix)

# Add element at the end of the list.
list_data[4] <- "New element"
print(list_data[4])
# Remove the last element.
list_data[4] <- NULL
# Print the 4th Element.
print(list_data[4])
# Update the 3rd Element.
list_data[3] <- "updated element"
print(list_data[3])

# Create two lists.
list1 <- list(1,2,3)
list2 <- list("Sun","Mon","Tue")
# Merge the two lists.
merged.list <- c(list1,list2)
# Print the merged list.
print(merged.list)

# Create lists.
list1 <- list(1:5)
print(list1)
list2 <-list(10:14)
print(list2)
# Convert the lists to vectors.
v1 <- unlist(list1)
v2 <- unlist(list2)
print(v1)
print(v2)
# Now add the vectors
result <- v1+v2
print(result)

##################################################################################
### Matrices ###

# Elements are arranged sequentially by row.
M <- matrix(c(3:14), nrow = 4, byrow = TRUE)
print(M)
# Elements are arranged sequentially by column.
N <- matrix(c(3:14), nrow = 4, byrow = FALSE)
print(N)
# Define the column and row names.
rownames = c("row1", "row2", "row3", "row4")
colnames = c("col1", "col2", "col3")
P <- matrix(c(3:14), nrow = 4, byrow = TRUE, dimnames = list(rownames, colnames))
print(P)
# Access the element at 3rd column and 1st row.
print(P[1,3])
# Access the element at 2nd column and 4th row.
print(P[4,2])
# Access only the  2nd row.
print(P[2,])
# Access only the 3rd column.
print(P[,3])

# Create two 2x3 matrices.
matrix1 <- matrix(c(3, 9, -1, 4, 2, 6), nrow = 2)
print(matrix1)
matrix2 <- matrix(c(5, 2, 0, 9, 3, 4), nrow = 2)
print(matrix2)
# Add the matrices.
result <- matrix1 + matrix2
cat("Result of addition","\n")
print(result)
# Subtract the matrices
result <- matrix1 - matrix2
cat("Result of subtraction","\n")
print(result)
# Multiply the matrices.
result <- matrix1 * matrix2
cat("Result of multiplication","\n")
print(result)
# Divide the matrices
result <- matrix1 / matrix2
cat("Result of division","\n")
print(result)

#################################################################################
### Arrays ###

# Create two vectors of different lengths.
vector1 <- c(5,9,3)
vector2 <- c(10,11,12,13,14,15)
# Take these vectors as input to the array.
result <- array(c(vector1,vector2),dim = c(3,3,2))
print(result)

# Create two vectors of different lengths.
vector1 <- c(5,9,3)
vector2 <- c(10,11,12,13,14,15)
column.names <- c("COL1","COL2","COL3")
row.names <- c("ROW1","ROW2","ROW3")
matrix.names <- c("Matrix1","Matrix2")
# Take these vectors as input to the array.
result <- array(c(vector1,vector2),dim = c(3,3,2),dimnames = list(row.names,column.names,
                                                                  matrix.names))
print(result)
# Print the third row of the second matrix of the array.
print(result[3,,2])
# Print the element in the 1st row and 3rd column of the 1st matrix.
print(result[1,3,1])
# Print the 2nd Matrix.
print(result[,,2])

# Create two vectors of different lengths.
vector1 <- c(5,9,3)
vector2 <- c(10,11,12,13,14,15)
# Take these vectors as input to the array.
array1 <- array(c(vector1,vector2),dim = c(3,3,2))
array1
# Create two vectors of different lengths.
vector3 <- c(9,1,0)
vector4 <- c(6,0,11,3,14,1,2,6,9)
array2 <- array(c(vector3,vector4),dim = c(3,3,2))
array2
# create matrices from these arrays.
matrix1 <- array1[,,2]
matrix1
matrix2 <- array2[,,2]
matrix2
# Add the matrices.
result <- matrix1+matrix2
print(result)

# Create two vectors of different lengths.
vector1 <- c(5,9,3)
vector2 <- c(10,11,12,13,14,15)
# Take these vectors as input to the array.
new.array <- array(c(vector1,vector2),dim = c(3,3,2))
print(new.array)
# Use apply to calculate the sum of the rows across all the matrices.
result <- apply(new.array, c(1), sum)
print(result)

#################################################################################
### Factors ###

# Create a vector as input.
data <- c("East","West","East","North","North","East","West","West","West","East","North")
print(data)
print(is.factor(data))
# Apply the factor function.
factor_data <- factor(data)
print(factor_data)
print(is.factor(factor_data))

# Create the vectors for data frame.
height <- c(132,151,162,139,166,147,122)
weight <- c(48,49,66,53,67,52,40)
gender <- c("male","male","female","female","male","female","male")
gender <- factor(gender)
print(is.factor(gender))
input_data <- data.frame(height,weight,gender)
print(input_data)
print(is.factor(input_data$gender))
print(input_data$gender)

data <- c("East","West","East","North","North","East","West",
          "West","West","East","North")
factor_data <- factor(data)
print(factor_data)
new_order_data <- factor(factor_data,levels = c("East","West","North"))
print(new_order_data)

# Generating Factor Levels
v <- gl(3, 4, labels = c("Tampa", "Seattle","Boston"))
print(v)
v <- gl(2, 4, labels = c("Tampa", "Seattle","Boston"))
print(v)

##############################################################################
### Data Frames ###
emp.data <- data.frame(
  emp_id = c (1:5), 
  emp_name = c("Rick","Dan","Michelle","Ryan","Gary"),
  salary = c(623.3,515.2,611.0,729.0,843.25), 
  start_date = as.Date(c("2012-01-01", "2013-09-23", "2014-11-15", "2014-05-11",
                         "2015-03-27")),
  stringsAsFactors = FALSE
)		
print(emp.data) 
str(emp.data)
print(summary(emp.data))  

result <- data.frame(emp.data$emp_name,emp.data$salary)
print(result)
result <- emp.data[1:2,]
print(result)
result <- emp.data[c(3,5),c(2,4)]
print(result)

emp.data$dept <- c("IT","Operations","IT","HR","Finance")
v <- emp.data
print(v)

# Create the first data frame.
emp.data <- data.frame(
  emp_id = c (1:5), 
  emp_name = c("Rick","Dan","Michelle","Ryan","Gary"),
  salary = c(623.3,515.2,611.0,729.0,843.25), 
  
  start_date = as.Date(c("2012-01-01", "2013-09-23", "2014-11-15", "2014-05-11",
                         "2015-03-27")),
  dept = c("IT","Operations","IT","HR","Finance"),
  stringsAsFactors = FALSE
)
# Create the second data frame
emp.newdata <- 	data.frame(
  emp_id = c (6:8), 
  emp_name = c("Rasmi","Pranab","Tusar"),
  salary = c(578.0,722.5,632.8), 
  start_date = as.Date(c("2013-05-21","2013-07-30","2014-06-17")),
  dept = c("IT","Operations","Fianance"),
  stringsAsFactors = FALSE
)
# Bind the two data frames.
emp.finaldata <- rbind(emp.data,emp.newdata)
print(emp.finaldata)

##############################################################################
### Packages ###
.libPaths()
library()

library(R.matlab)
#Get all packages currently loaded in the R environment
search()




































