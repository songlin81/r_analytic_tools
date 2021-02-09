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


















