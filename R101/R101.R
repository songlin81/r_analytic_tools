# Author: Lin, Song
# Description: Data Analytics with R
# Section 1. R language basics
#
# 1. Init section
.libPaths()
###########################################
# 2. Generate vector
A <- 1:7
B <- c(1,3,5,7,9)
C <- seq(from=2, to=10, by=2)
D <- seq(from=2, to=10, length.out=5)
E <- rep(1:2, 5)
F <- rep(1:2, c(2,3))

vchar <- c("A","B","C","D","E")
class(vchar)

v_log <- rep(c(TRUE, FALSE),c(2,3))
class(v_log)

v_fac <- factor(x=c("A","B","C","A","C"), 
                levels=c("A","B","C"), 
                labels = c("A","B","C"))
v_fac
levels(v_fac)
#################################################
# 3. Vector computation
vec <- seq(1:7)
vec / 2
vec / (2*vec)

cumprod(1:5)
cumsum(vec)
length(vec)
#################################################
# 4. Retrieve element in vector
x <- c(3, 6, 1, NA, 2)
x[x > 2]
x>2

vec[c(1,3,5,7,9)]
vec %% 3 == 0
vec[vec %% 3 == 0]
vec[c(-1:-5)]

rev(vec)
#################################################
# 5. Conversion
vec_num <- seq(from=2, to=10, by=2)
str(vec_num)
vec_char <- as.character(vec_num)
str(vec_char)
vec_num <- as.numeric(vec_char)
is.numeric(vec_num)
#################################################
# 6. Factor vector
vec_fac <- factor(c("A","B","C","A","C"))
str(vec_fac)
vec_fac2char <- as.character(vec_fac)
str(vec_fac2char)

unique(vec_fac2char)
table(vec_fac2char)
#################################################
# 7. Vector ops
union(c(1:5), seq(2,10,2))
setdiff(c(1:5), seq(2,10,2))
intersect(c(1:5), seq(2,10,2))
is.element(c(1:5), seq(2,10,2))
c(1:5) %in% seq(2,10,2)
#################################################
# 8. Matrix
vec <- seq(1,12)
mat <- matrix(vec, nrow = 2)
mat

mat <- matrix(vec, nrow = 2, ncol = 4, byrow = TRUE)
mat

mat <- cbind(c(1,3,5,7), c(2,4,6,8), c(1:4))
mat

mat <- rbind(c(1,3,5,7), c(2,4,6,8), c(1:4))
mat

diag(4)
diag(c(1:4))

colnames(mat) <- c("A","B","C","D")
rownames(mat) <- c("a","b","c")
mat

dim(mat)
nrow(mat)
ncol(mat)
length(mat)

mat[2,3]
mat[,2]
mat[1,]
mat[, c("A","C")]
mat %% 2 == 0
mat[mat %% 2 == 0]

mat <- matrix(c(1:12), nrow = 3)
mat
t(mat)

rowSums(mat)
apply(mat, 1, sum)

colSums(mat)
apply(mat, 2, sum)

rowMeans(mat)
colMeans(mat)

mat * mat
mat %*% t(mat)

mat[lower.tri(mat)] <- 0
mat

mat2 <- cbind(1, 2:4, c(2,4,1))
mat2
det(mat2)

set.seed(123)
solve(matrix(runif(16), 4, 4))
#################################################
# 9. array
arr <- array(1:24, dim = c(3,4,2))
arr
arr[2,,2]
arr[which(arr %% 5 == 0)]
dim(arr)

apply(arr, 3, mean)
apply(arr, 2, sum)
#################################################
# 10. Data Frame
df <- data.frame(
  id = c("A","B","C","D"),
  age = c(10,15,9,12),
  sex = c("F","M","M","F"),
  score = (17:20),
  stringsAsFactors = FALSE
)
df
head(df)
summary(df)

df$sex <- factor(df$sex)
str(df)

with(df, age>10)
df$newvar <- df$score * 2

mat <- rbind(c(1,3,5,7),c(2,4,6,8),c(1:4))
mat2df <- as.data.frame(mat)
colnames(mat2df) <- c("A","B","C","D")
mat2df

df[,2]
df$id
df$id[3]
df[c("id","age")]
df[df$age>10,]
df[df$id %in% c("B","D","F"), 1:3]
#################################################
# 11. list
A <- factor(c("A","B","C","C","B"))
B <- matrix(seq(1:8),nrow = 2)
C <- "Type"
D <- data.frame(
  id=c("A","B","C","D"),
  age=c(10,15,9,12)
)
mylist <- list(A,B,C,D)
mylist
str(mylist)
mylist[1]
mylist[[1]]
mylist[[2]][2,1:3]
mylist[[4]]$age[1:3]

names(mylist) <- c("one","two","three","four")
names(mylist)
mylist$one
#################################################
# 12. Flow
num <- 9
if(num %% 3 == 0) print("dividable by 3") else print("non-dividable by 3")

num <- 10
ifelse(num %% 3 == 0, num, NA)

id=c("A","B","C","D")
switch (id[2],
  A = 10,
  B = 15,
  C = 9,
  D = 12)

vec <- seq(1:20)
result1 <- result2 <- vector()
for(ii in 1:length(vec)){
  if(vec[ii] %% 2 == 0){
    result1 <- c(result1, vec[ii])
  }else{
    result2 <- c(result2, vec[ii])
  }
}
result1
result2

set.seed(12)
vec <-sample(seq(1:100), 40)
ii <- 1
result1 <- vector()
while (ii) {
  if(vec[ii]%%2==0){
    result1 <- c(result1, vec[ii])
  }
  if(length(result1)==5){
    break
  }
  ii<-ii+1
}
result1
#################################################
# 13. Function
source("R101/Quadratic.R")
answ <- twosol(0,3,ee=10^(-5))
answ
