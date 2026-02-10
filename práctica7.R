#InterpolaciónLagrange -------------
ax0 <- c(2, 2.5, 4)
aY0 <- function(x0){
  n <- length(x0)
  y0 <- c()
  for (i in 1:n){
    fx <- 1/x0[i] 
    y0 <- c(y0,fx)
  }
  return(y0)
}
ay0 <- Y0(ax0)

aIL <- function(y,x0,y0){
  Px <- 0
  n <- length(x0)
  
  for (i in 1:n){
    Lx <- 1
    for (k in 1:n){
      if(k!=i){
        Lx <- Lx*((y-x0[k])/(x0[i]-x0[k]))
      }
      
    }
  Px <- Px + y0[i]*Lx 
  }
 return(Px)
}
IL(3, ax0, ay0)