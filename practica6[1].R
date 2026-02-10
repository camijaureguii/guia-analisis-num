# Norma Vectorial -------------
Norma <- function(y, metodo){
  suma = 0
  if (metodo == 2){
    for (i in y){
      suma = suma + i^2
    }
    norma_eucl <- suma^(1/2)
    return(norma_eucl)
  }
  if (metodo == Inf){
    norma_inf <- max(abs(y))
    return(norma_inf)
  }
}

Norma(c(2, 1, 3, -2, -1), 2)
#------------------------------------------
elim_gauss <- function(n,Aa){
  for (i in 1:(n-1)){
    pp <-0
    for (p in i:n){
      if (Aa[p,i] != 0){
        pp <-p
        break
      }
    }
    if (pp == 0){
      return("no hay solución única")
    }
    if (pp != i){
      perm <- Aa[i,]
      Aa[i,] <- Aa[pp,]
      Aa[pp,] <- perm
    }
    for (e in (i+1):n){
      mj <- Aa[e,i] / Aa[i,i]
      Aa[e,] <- Aa[e,] - mj*Aa[i,]
    }
  }
  if(Aa[n,n]==0){
    return("no hay solución única")
  }
  print(Aa)
  x <- numeric(n)
  x[n] = Aa[n,n+1]/Aa[n,n]
  for (i in (n-1):1){
    aux <- 0
    for (j in (i+1):n){
      aux <- aux + Aa[i,j]*x[j]
    }
    x[i] <- (Aa[i,(n+1)] - aux)/Aa[i,i]
  }  
  return(x)
}

#---------------------------------------------
#Metodo de Newton--------
F0 <- function(x) {
  F1 <- x[1]^2 + x[2]^2 - 10
  F2 <- x[1]^2 - x[2]^2 - 1
  y <- matrix(c(F1, F2), nrow = 2)
  z <- (-1)*y
  return(z)
}
#F0(c(2.3452, 2.1213))

J0 <- function(x) {
  J1 <- 2*x[1] 
  J2 <- 2*x[2]
  J3 <- 2*x[1]
  J4 <- -2*x[2]
  return(matrix(c(J1, J2, 
                  J3, J4),byrow = TRUE, nrow = 2))
}
#J0(c(2.3452, 2.1213))

Newton <- function(x, tol, N){
  k <- 1
  while(k<=N){
    y <- solve(J0(x))%*%F0(x)
    if (norm(y)<= tol){
      x <- x + y
      return(x)
    }
    else{
      x <- x + y
      k = k + 1
    }
  }
  return(paste("El método falló luego de", N, "iteraciones")) 
}
Newton(c(2.3452, 2.1213), 0.0001, 600)

Newton2 <- function(x, tol, N) {
  k <- 1
  while(k <= N) {
    y <- cbind(J0(x), F0(x))  #une las matrices
    z <- elim_gauss(2, y)
    if (Norma(z, "2") <= tol) {
      x <- x + z
      return(x)
    } else {
      x <- x + y
      k <- k + 1
    }
  }
  return(paste("El método falló luego de", N, "iteraciones")) 
}
Newton2(c(2.3452, 2.1213), 0.0001, 600)
#----------------------------------------------------------------
F0e <- function(x) {
  F1 <- 3*x[1] - cos(x[2]*x[3]) - 1/2
  F2 <- x[1]^2 - 81*(x[2] + 0.1)^2 + sin(x[3]) + 1.06
  F3 <- exp(-x[1]*x[2]) + 20*x[3] + (10*pi - 3)/3
  y <- matrix(c(F1, F2, F3), nrow = 3)
  z <- (-1)*y
  return(z)
}

J0e <- function(x) {
  J1 <- 3
  J2 <- sin(x[2]*x[3]) * x[3]
  J3 <- sin(x[2]*x[3]) * x[2]
  J4 <- 2*x[1]
  J5 <- -162*(x[2]+0.1)
  J6 <- cos(x[3])
  J7 <- -x[2]*exp(-x[1]*x[2])
  J8 <- -x[1]*exp(-x[1]*x[2])
  J9 <- 20
  return(matrix(c(J1, J2, J3, 
                  J4, J5, J6,
                  J7, J8, J9), byrow = TRUE, nrow = 3))
}

Newton0 <- function(x, tol, N){
  k <- 1
  while(k<=N){
    y <- solve(J0e(x))%*%F0e(x)
    if (norm(y)<= tol){
      x <- x + y
      return(x)
    }
    else{
      x <- x + y
      k = k + 1
    }
  }
  return(paste("El método falló luego de", N, "iteraciones")) 
}
Newton0(c(0.1,0.1,-0.1), 10^-6, 600)

#----------------------------------------------------------------#Newton 1 --------------

F1 <- function(x) {
  F1 <- 3*x[1]^2 - x[2]^2
  F2 <- 3*x[1]*x[2]^2 - x[1]^3 - 1
  y <- matrix(c(F1, F2), nrow = 2)
  z <- (-1)*y
  return(z)
}

J1 <- function(x) {
  J1 <- 6*x[1] 
  J2 <- - 2*x[2]
  J3 <- 3*x[2]^2 - 3*x[1]^2
  J4 <- 6*x[1]*x[2]
  return(matrix(c(J1, J2, 
                  J3, J4), byrow = TRUE, nrow = 2))
}

Newton1<- function(x, tol, N){
  k <- 1
  while(k<=N){
    y <- solve(J1(x))%*%F1(x)
    if (norm(y)<= tol){
      x <- x + y
      return(x)
    }
    else{
      x <- x + y
      k = k + 1
    }
  }
  return(paste("El método falló luego de", N, "iteraciones")) 
}
Newton1(c(0.1,0.1), 10^-6, 600)

#Newton2 -------------------------

F2 <- function(x) {
  F1 <- 3*x[1] - cos(x[2]*x[3]) - 1/2
  F2 <- 4*x[1]^2 - 625*x[2]^2 + 2*x[2] - 1
  F3 <- 20*x[3] + (10*pi - 3)/3 + exp(-x[1]*x[2])
  y <- matrix(c(F1, F2, F3), nrow = 3)
  z <- (-1)*y
  return(z)
}

J2 <- function(x) {
  J1 <- 3
  J2 <- sin(x[2]*x[3]) * x[3]
  J3 <- sin(x[2]*x[3]) * x[2]
  J4 <- 8*x[1]
  J5 <- -1250*x[2] + 2
  J6 <- 0
  J7 <- -x[2]*exp(-x[1]*x[2])
  J8 <- -x[1]*exp(-x[1]*x[2])
  J9 <- 20
  return(matrix(c(J1, J2, J3, 
                  J4, J5, J6,
                  J7, J8, J9), byrow = TRUE, nrow = 3))
}

Newton2 <- function(x, tol, N){
  k <- 1
  while(k<=N){
    y <- solve(J2(x))%*%F2(x)
    if (norm(y)<= tol){
      x <- x + y
      return(x)
    }
    else{
      x <- x + y
      k = k + 1
    }
  }
  return(paste("El método falló luego de", N, "iteraciones")) 
}
Newton2(c(0.1,0.1,-0.1), 10^-6, 600)

#----------------------------------------------------------
#Newton3
F3 <- function(x) {
  F1 <- x[1]^3 + x[2]*x[1]^2 - x[1]*x[3] + 6
  F2 <- exp(x[1]) + exp(x[2]) - x[3]
  F3 <- x[2]^2 - 2*x[1]*x[3]
  y <- matrix(c(F1, F2, F3), nrow = 3)
  z <- (-1)*y
  return(z)
}

J3 <- function(x) {
  J1 <- 3*x[1]^2 + 2*x[2]*x[1] - x[3]
  J2 <- x[1]^2 
  J3 <- -x[1]
  J4 <- exp(x[1])
  J5 <- exp(x[2])
  J6 <- -1
  J7 <- -2*x[3]
  J8 <- 2*x[2]
  J9 <- -2*x[1]
  return(matrix(c(J1, J2, J3, 
                  J4, J5, J6,
                  J7, J8, J9), byrow = TRUE, nrow = 3))
}

Newton3 <- function(x, tol, N){
  k <- 1
  while(k<=N){
    y <- solve(J3(x))%*%F3(x)
    if (norm(y)<= tol){
      x <- x + y
      return(x)
    }
    else{
      x <- x + y
      k = k + 1
    }
  }
  return(paste("El método falló luego de", N, "iteraciones")) 
}
Newton3(c(0.1,0.1,-0.1), 10^-6, 600)

#-------------------------------------------------------------
#Newton4
F4 <- function(x) {
  F1 <- 5*x[1]^2 - x[2]^2
  F2 <- x[2] - 1/4*(sin(x[1]) + cos(x[2]))
  y <- matrix(c(F1, F2), nrow = 2)
  z <- (-1)*y
  return(z)
}

J4 <- function(x) {
  J1 <- 10*x[1] 
  J2 <- -2*x[2]
  J3 <- -1/4*cos(x[1])
  J4 <- 1 + 1/4*sin(x[2])
  return(matrix(c(J1, J2, 
                  J3, J4), byrow = TRUE, nrow = 2))
}

Newton4<- function(x, tol, N){
  k <- 1
  while(k<=N){
    y <- solve(J4(x))%*%F4(x)
    if (norm(y)<= tol){
      x <- x + y
      return(x)
    }
    else{
      x <- x + y
      k = k + 1
    }
  }
  return(paste("El método falló luego de", N, "iteraciones")) 
}
Newton4(c(0.1,0.1), 10^-6, 600)
#-------------------------------------------------------------------
#Newton 5 --------
F5 <- function(x) {
  F1 <- log(x[1]^2 + x[2]^2) - sin(x[1]*x[2]) - log(2)
  F2 <- exp(x[1]-x[2]) + cos(x[1]*x[2])
  y <- matrix(c(F1, F2), nrow = 2)
  z <- (-1)*y
  return(z)
}

J5 <- function(x) {
  J1 <- 1/(x[1]^2 + x[2]^2) * 2*x[1] - cos(x[1]*x[2])*x[2]
  J2 <- 1/(x[1]^2 + x[2]^2) * 2*x[2] - cos(x[1]*x[2])*x[1]
  J3 <- exp(x[1]-x[2]) - sin(x[1]*x[2])*x[2]
  J4 <- -exp(x[1]-x[2]) - sin(x[1]*x[2])*x[1]
  return(matrix(c(J1, J2, 
                  J3, J4), byrow = TRUE, nrow = 2))
}

Newton5<- function(x, tol, N){
  k <- 1
  while(k<=N){
    y <- solve(J5(x))%*%F5(x)
    if (norm(y)<= tol){
      x <- x + y
      return(x)
    }
    else{
      x <- x + y
      k = k + 1
    }
  }
  return(paste("El método falló luego de", N, "iteraciones")) 
}
Newton5(c(0.1,0.1), 10^-6, 600)

#----------------------------------------------------------------------
#Broyden1--------------------------------------
F1 <- function(x) {
  F1 <-  3 * x[1] - cos(x[2] * x[3]) - 0.5
  F2 <- x[1]^2 - 81 * (x[2] + 0.1)^2 + sin(x[3]) + 1.06
  F3 <- exp(-x[1] * x[2]) + 20 * x[3] + (10 * pi - 3) / 3
  y <- matrix(c(F1, F2, F3), nrow = 3)
  return(y)
}

J1 <- function(x) {
  J1 <- 3
  J2 <- x[3]*sin(x[2]*x[3])
  J3 <- x[2]*sin(x[2]*x[3])
  J4 <- 2*x[1]
  J5 <- -162*(x[2]+0.1)
  J6 <- cos(x[3])
  J7 <- -x[2]*exp(-x[1]*x[2])
  J8 <- -x[1]*exp(-x[1]*x[2])
  J9 <- 20
  return(matrix(c(J1, J2, J3, 
                  J4, J5, J6,
                  J7, J8, J9), byrow = TRUE, nrow = 3))
}

Broyden1 <- function(x, tol, N){
  v = F1(x)
  A0inv <- solve(J1(x))
  s = -A0inv%*%v
  x = x + s
  k = 2
  while(k <= N){
    w = v
    v = F1(x)
    y = v - w
    z = A0inv %*% y
    r = t(s) %*% z
    r = r[1,1]
    A0inv <- A0inv + (s - z) %*% t(s) %*% A0inv / r
    s <- -A0inv %*% v
    x <- x + s

    if (Norma(s, "2")<= tol){
      return(x)
    }
    else {
      k = k + 1
      }
  }
  return(paste("El método falló luego de", N, "iteraciones"))
}

Broyden1(c(0.1,0.1,-0.1), 10^-6, 600)

#Broyden2----------------------------------------------
F2 <- function(x) {
  F1 <-  x[1]^3 + x[2]*x[1]^2 - x[1]*x[3] + 6
  F2 <- exp(x[1]) + exp(x[2]) - x[3]
  F3 <- x[2]^2 - 2*x[1]*x[3] - 4
  y <- matrix(c(F1, F2, F3), nrow = 3)
  return(y)
}

J2 <- function(x) {
  J1 <- 3*x[1]^2 + 2*x[2]*x[1] - x[3]
  J2 <- x[1]^2
  J3 <- -x[1]
  J4 <- exp(x[1])
  J5 <- exp(x[2])
  J6 <- -1
  J7 <- -2*x[3]
  J8 <- 2*x[2]
  J9 <- -2*x[1]
  return(matrix(c(J1, J2, J3, 
                  J4, J5, J6,
                  J7, J8, J9), byrow = TRUE, nrow = 3))
}

Broyden2 <- function(x, tol, N){
  v = F2(x)
  A0inv <- solve(J2(x))
  s = -A0inv%*%v
  x = x + s
  k = 2
  while(k <= N){
    w = v
    v = F2(x)
    y = v - w
    z = A0inv %*% y
    r = t(s) %*% z
    r = r[1,1]
    A0inv <- A0inv + (s - z) %*% t(s) %*% A0inv / r
    s <- -A0inv %*% v
    x <- x + s
    
    if (Norma(s, "2")<= tol){
      return(x)
    }
    else {
      k = k + 1
    }
  }
  return(paste("El método falló luego de", N, "iteraciones"))
}

Broyden2(c(-1, -2, 1), 10^-6, 600)

#Broyden3------------------------------------------------
F3 <- function(x) {
  F1 <-  6*x[1] - 2*cos(x[2]*x[3]) - 1
  F2 <- 9*x[2] + (x[1]^2 + sin(x[3]) + 1.06)^(1/2) + 0.9
  F3 <- 60*x[3] + 3*exp(-x[1]*x[2]) + 10*pi - 3
  y <- matrix(c(F1, F2, F3), nrow = 3)
  return(y)
}

J3 <- function(x) {
  J1 <- 6
  J2 <- 2*sin(x[2]*x[3])*x[3]
  J3 <- 2*sin(x[2]*x[3])*x[2]
  J4 <- (x[1]^2+sin(x[3]) + 1.06)^(-1/2)*x[1]
  J5 <- 9 
  J6 <- (1/2)*(x[1]^2+sin(x[3]) + 1.06)^(-1/2)*cos(x[3])
  J7 <- -3*x[2]*exp(-x[1]*x[2])
  J8 <- -3*x[1]*exp(-x[1]*x[2])
  J9 <- 60
  return(matrix(c(J1, J2, J3, 
                  J4, J5, J6,
                  J7, J8, J9), byrow = TRUE, nrow = 3))
}

Broyden3 <- function(x, tol, N){
  v = F3(x)
  A0inv <- solve(J3(x))
  s = -A0inv%*%v
  x = x + s
  k = 2
  while(k <= N){
    w = v
    v = F3(x)
    y = v - w
    z = A0inv %*% y
    r = t(s) %*% z
    r = r[1,1]
    A0inv <- A0inv + (s - z) %*% t(s) %*% A0inv / r
    s <- -A0inv %*% v
    x <- x + s
    
    if (Norma(s, "2")<= tol){
      return(x)
    }
    else {
      k = k + 1
    }
  }
  return(paste("El método falló luego de", N, "iteraciones"))
}

Broyden3(c(0, 0, 0), 10^-6, 600)

#Broyden4 -------------------------------

F4 <- function(x) {
  F1 <-  log(x[1]^2 + x[2]^2) - sin(x[1]*x[2]) - log(2) - log(pi)
  F2 <- exp(x[1]-x[2]) + cos(x[1]*x[2])
  y <- matrix(c(F1, F2), nrow = 2)
  return(y)
}

J4 <- function(x) {
  J1 <- 1/((x[1]^2 + x[2]^2) - sin(x[1]*x[2])) * 2*x[1] - cos(x[1]*x[2]) * x[2]
  J2 <- 1/((x[1]^2 + x[2]^2) - sin(x[1]*x[2])) * 2*x[2] - cos(x[1]*x[2]) * x[1]
  J3 <- exp(x[1]-x[2]) - sin(x[1]*x[2])*x[2]
  J4 <- -exp(x[1]-x[2]) - sin(x[1]*x[2])*x[1]
  return(matrix(c(J1, J2, 
                  J3, J4), byrow = TRUE, nrow = 2))
}

Broyden4 <- function(x, tol, N){
  v = F4(x)
  A0inv <- solve(J4(x))
  s = -A0inv%*%v
  x = x + s
  k = 2
  while(k <= N){
    w = v
    v = F4(x)
    y = v - w
    z = A0inv %*% y
    r = t(s) %*% z
    r = r[1,1]
    A0inv <- A0inv + (s - z) %*% t(s) %*% A0inv / r
    s <- -A0inv %*% v
    x <- x + s
    
    if (Norma(s, "2")<= tol){
      return(x)
    }
    else {
      k = k + 1
    }
  }
  return(paste("El método falló luego de", N, "iteraciones"))
}

Broyden4(c(2,2), 10^-6, 600)

#Broyden5 -----------------------------------
F5 <- function(x) {
  F1 <-  4*x[1]^2 - 20*x[1] + (1/4)*x[2]^2 + 8
  F2 <- (1/2)*x[1]*x[2]^2 + 2*x[1] - 5*x[2] + 8
  y <- matrix(c(F1, F2), nrow = 2)
  return(y)
}


J5 <- function(x) {
  J1 <- 8*x[1] - 20
  J2 <- (1/2)*x[2]
  J3 <- (1/2)*x[2]^2 + 2 
  J4 <- x[1]*x[2] - 5
  return(matrix(c(J1, J2, 
                  J3, J4), byrow = TRUE, nrow = 2))
}

Broyden5 <- function(x, tol, N){
  v = F5(x)
  A0inv <- solve(J5(x))
  s = -A0inv%*%v
  x = x + s
  k = 2
  while(k <= N){
    w = v
    v = F5(x)
    y = v - w
    z = A0inv %*% y
    r = t(s) %*% z
    r = r[1,1]
    A0inv <- A0inv + (s - z) %*% t(s) %*% A0inv / r
    s <- -A0inv %*% v
    x <- x + s
    
    if (Norma(s, "2")<= tol){
      return(x)
    }
    else {
      k = k + 1
    }
  }
  return(paste("El método falló luego de", N, "iteraciones"))
}

Broyden5(c(0,0), 10^-6, 600)
