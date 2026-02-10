
#Punto 1
trapecio_ncc <- function(f,x0,x1){
  h <- (x1-x0)/1
  fxdx <- (h/2)*(f(x0) + f(x1))
  return(fxdx)
}

f1a <- function(x){
  y <- 2/(x-4)
  y
}
trapecio_ncc(f1a,0,0.5)    #1a)

f1b <- function(x){
  y <- 2/(x^2-4)
  y
}
trapecio_ncc(f1b,0,0.35)   #1b)
------------------------------------------------------------------------------
simpson_ncc <- function(f,x0,x2){
  h <- (x2-x0)/2
  x1 <- x0 + h
  fxdx <- (h/3)*(f(x0) + 4*f(x1)+ f(x2))
  return(fxdx)
}

f2a <- function(x){
  y <- x^2*exp(-x)
  y
}
simpson_ncc(f2a,0,1)

f2b <- function(x){
  y <- exp(3*x)*sin(2*x)
}
simpson_ncc(f2b,0,pi/4)
------------------------------------------------------------------------------
tresoct_ncc <- function(f,x0,x3){
  h <- (x3-x0)/3
  x1 <- x0 + h
  x2 <- x0 + 2*h
  fxdx <- (3*h/8)*(f(x0) + 3*f(x1)+ 3*f(x2) + f(x3))
  return(fxdx)
}
  
f3a <- function(x){
  y <- x^4
  y
}
tresoct_ncc(f3a,0.5,1)

f3b <- function(x){
  y <- (2*x)/(x^2-4)
  y
}
tresoct_ncc(f3b,1,1.6)
---------------------------------------------------------------------------------------
n4_ncc <- function(f,x0,x4){
  h <- (x4-x0)/4
  x1 <- x0 + h
  x2 <- x0 + 2*h
  x3 <- x0 + 3*h
  fxdx <- (2*h/45)*(7*f(x0) + 32*f(x1)+ 12*f(x2) + 32*f(x3) + 7*f(x4))
  return(fxdx)
}

f4a <- function(x){
  y <- x^2*log(x)
  y
}
n4_ncc(f4a,1,1.5)

f4b <- function(x){
  y <- sin(x)*x
  y
}
n4_ncc(f4b,0,pi/4)
-------------------------------------------------------------------------------------------
#Punto 2 Integracion numerica
puntomedio_nca <- function(f,x0,x1){
  h <- (x1-x0)/(2)
  fxdx <- 2*h*f(x0+h)
  return(fxdx)
}

n1_nca <- function(f,x0,x2){
  h <- (x2-x0)/(1+2)
  x0 <- x0 + h
  x1 <- x0 + h
  fxdx <- 3*(h/2)*(f(x0)+f(x1))
  return(fxdx)
}

n2_nca <- function(f,x0,x3){
  h <- (x3-x0)/(2+2)
  x0 <- x0 + h
  x1 <- x0 + h
  x2 <- x0 + 2*h
  fxdx <- 4*(h/3)*(2*f(x0)-f(x1)+2*f(x2))
  return(fxdx)
}

n3_nca <- function(f,x0,x4){
  h <- (x4-x0)/(3+2)
  x0 <- x0 + h
  x1 <- x0 + h
  x2 <- x0 + 2*h
  x3 <- x0 + 3*h
  fxdx <- 5*(h/24)*(11*f(x0)+f(x1)+f(x2)+11*f(x3))
  return(fxdx)
}

f5a <- function(x){
  y <- (x+1)^(1/2)
  y
}
puntomedio_nca(f5a,0,0.1)
n1_nca(f5a,0,0.1)
n2_nca(f5a,0,0.1)
n3_nca(f5a,0,0.1)

f5b <- function(x){
  y <- (sin(x)^2)
  y
}
puntomedio_nca(f5b,0,pi/2)
n1_nca(f5b,0,pi/2)
n2_nca(f5b,0,pi/2)
n3_nca(f5b,0,pi/2)

f5c <- function(x){
  y <- exp(x)
  y
}
puntomedio_nca(f5c,1.1,1.5)
n1_nca(f5c,1.1,1.5)
n2_nca(f5c,1.1,1.5)
n3_nca(f5c,1.1,1.5)

f5d <- function(x){
  y <- 1/x
  y
}
puntomedio_nca(f5d,1,10)
n1_nca(f5d,1,10)
n2_nca(f5d,1,10)
n3_nca(f5d,1,10)

f5e <- function(x){
  y <- 1/x
  y
}
puntomedio_nca(f5e,1,5.5) + puntomedio_nca(f5e,5.5,10)
n1_nca(f5e,1,5.5) + n1_nca(f5e,5.5,10)
n2_nca(f5e,1,5.5) + n2_nca(f5e,5.5,10)
n3_nca(f5e,1,5.5) + n3_nca(f5e,5.5,10)

f5f <- function(x){
  y <- (x)^(1/3)
  y
}
puntomedio_nca(f5f,0,1)
n1_nca(f5f,0,1)
n2_nca(f5f,0,1)
n3_nca(f5f,0,1)
--------------------------------------------------------------------------------
#Integracion numerica compuesta
IC_simpson <- function(f,a,b,n){
  h <- (b-a)/n
  XI0 <- f(a) + f(b)
  XI1 <- 0
  XI2 <- 0
  
  for(j in 1:(n/2-1)) {
    x <- a + 2*j*h
    XI1 <- XI1 + f(x)
  }
  
  for(j in 1:(n/2)) {
    x <- a + (2*j-1)*h
    XI2 <- XI2 + f(x)
  }
  
  dx <- (h/3)*(XI0 + 2*XI1 + 4*XI2)
  return(dx)
}

IC_trapecio <- function(f,a,b,n){
  h <- (b-a)/n
  XI0 <- f(a) + f(b)
  XI1 <- 0
  
  for(j in 1:(n-1)) {
    x <- a + j*h
    XI1 <- XI1 + f(x)
  }
  
  dx <- (h/2)*(XI0 + 2*XI1)
  return(dx)
} 

IC_puntomedio <- function(f,a,b,n){
  h <- (b-a)/(n+2)
  XI1 <- 0
  
  for(j in 0:(n/2)) {
    x <- a + (2*j+1)*h
    XI1 <- XI1 + f(x)
  }
  
  dx <- 2*h*XI1
  return(dx)
}

f1 <- function(x){
  y <- x*log(x)
  y
}
IC_simpson(f1,1,2,4)
IC_trapecio(f1,1,2,4)
IC_puntomedio(f1,1,2,4)

f2 <- function(x){
  y <- x^3*exp(x)
  y
}
IC_simpson(f2,-2,2,4)
IC_trapecio(f2,-2,2,4)
IC_puntomedio(f2,-2,2,4)

f3 <- function(x){
  y <- 2/(x^2+4)
  y
}
IC_trapecio(f3,0,2,6)
IC_simpson(f3,0,2,6)
IC_puntomedio(f3,0,2,6)

f4 <- function(x){
  y <- x^2*cos(x)
  y
}
IC_trapecio(f4,0,pi,6)
IC_simpson(f4,0,pi,6)
IC_puntomedio(f4,0,pi,6)

f5 <- function(x){
  y <- exp(2*x)*sin(3*x)
  y
}
IC_trapecio(f5,0,2,8)
IC_simpson(f5,0,2,8)
IC_puntomedio(f5,0,2,8)

f6 <- function(x){
  y <- 1*(x^2-4)^(-1/2)
  y
}
IC_trapecio(f6,3,5,8)
IC_simpson(f6,3,5,8)
IC_puntomedio(f6,3,5,8)

f7 <- function(x){
  y <- tan(x)
  y
}
IC_trapecio(f7,0,3*pi/8,8)
IC_simpson(f7,0,3*pi/8,8)
IC_puntomedio(f7,0,3*pi/8,8)
---------------------------------------------------------------------------------
gammadensi <- function(x){
  fx <- x^(9.23-1) * exp(-x)
  fx
}
IC_simpson(gammadensi,0,100,100) #no da uno porque es la función gamma, n son la cantidad de subintervalos

gammadistr <- function(x){
  fx <- (0.43*exp(-0.43*x)*(0.43*x)^(9.23-1))/gamma(9.23)
  fx
}
IC_simpson(gammadistr,0,1000,1000)       #da uno porque es la distribucion gamma

Gammaesperanza <- 9.23/0.43
Gammaesperanza

gammaesp <- function(x){
  fx <- x*(0.43*exp(-0.43*x)*(0.43*x)^(9.23-1))/gamma(9.23)
  fx
}
gammaespx <- IC_trapecio(gammaesp,0,1000,1000)
Gammaesperanza - gammaespx  #menor a 0.00001

Gammavarianza <- 9.23/(0.43)^2
Gammavarianza

gammacuadr <- function(x){
  fx <- x^2*(0.43*exp(-0.43*x)*(0.43*x)^(9.23-1))/gamma(9.23)
  fx
}
gammacuadrado <- IC_simpson(gammacuadr,0,1000,1000)
gammavar <- gammacuadrado - (gammaespx)^2
gammavar

Gammavarianza - gammavar  #menor a 0.00001

------------------------------------------------------------------------
normaldistr <- function(x){
  fx <- exp((-1/2)*((x-24)/18)^2)/(18*(2*pi)^(1/2))
  fx
}
IC_simpson(normaldistr,-1000,1000,1000)

normalesperanza <- 24
normalesperanza

normalesp <- function(x){
  fx <- x*exp((-1/2)*((x-24)/18)^2)/(18*(2*pi)^(1/2))
  fx
}
normalespx <- IC_simpson(normalesp,-1000,1000,1000)
normalesp
normalesperanza - normalespx  #menor a 0.00001

normalvarianza <- 18^2
normalvarianza

normalcuadr <- function(x){
  fx <- x^2*exp((-1/2)*((x-24)/18)^2)/(18*(2*pi)^(1/2))
  fx
}
normalcuadrado <- IC_simpson(normalcuadr,-1000,1000,1000)
normalvar <- normalcuadrado - (normalespx)^2
normalvar

Gammavarianza - gammavar  #menor a 0.00001
