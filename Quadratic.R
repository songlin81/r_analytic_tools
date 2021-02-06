solvefunction <- function(x){
  x^3-2*x^2-1
}

twosol <- function(a,b,ee=10^(-5)){
  if (solvefunction(a)*solvefunction(b) > 0 | a > b)
    print("Change boundary")
  else
    while(abs(a-b)>=ee) {
      c <- (a+b)/2
      if (solvefunction(c) == 0)
        return(c)
      if (solvefunction(a)*solvefunction(c)<0)
        b <- c
      if (solvefunction(c)*solvefunction(b)<0)
        a <- c
    }
  return(c)
}