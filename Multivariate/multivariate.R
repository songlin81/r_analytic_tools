library(R.matlab)

ETHdata <- readMat("Multivariate/data/ETH_8class_object_8_big_classes_32_32_1024D.mat")
ETHims <- ETHdata$A / 255.0
dim(ETHims)

set.seed(123)
index <- sample(ncol(ETHims),40)
par(mfrow = c(5,8),mai=c(0.05,0.05,0.05,0.05))
for(ii in seq_along(index)){
  im <- matrix(ETHims[,index[ii]],nrow=32,ncol = 32,byrow = TRUE)
  image(im,col = gray(seq(0, 1, length = 256)),xaxt= "n", yaxt= "n")
}

