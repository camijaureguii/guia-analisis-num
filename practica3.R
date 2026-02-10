# Eliminación de Gauss -------
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
A <- matrix(c(1, -1, 2, -1, -8, 
              2, -2, 3, -3, -20, 
              1, 1, 1, 0, -2,
              1, -1, 4, 3, 4), byrow = TRUE, nrow = 4)
elim_gauss(4,A)
x <- matrix(c(-7,3,2,2),nrow=4)
print(x)

A[,1:4]%*%x

B <- matrix(c(1, 1, 0, 3, 1, 
              2, 1, -1, 1, 1, 
              3, -1, -1, 2, -3,
              -1, 2, 3, -1, 4), byrow = TRUE, nrow = 4)
elim_gauss(4,B)

#Lu Ejemplo --------------
#LU, en vez de hacer sustitución para atrás, consigo la matriz L
LU <- function(n, Aa){
  L <- diag(n)
  U <- matrix(0, nrow = n, ncol = n)
  U[1,1] = Aa[1,1]
  for (j in 2:n){
    U[1,j] <- Aa[1,j]
    L[j,1] <- Aa[j,1]/Aa[1,1]
  }
  for (i in 2:(n-1)){
    for (k in 1:(i-1)){
    U[i,i] <- Aa[i,i] - L[i,k]*U[k,i]
    }
    for (j in (i+1):n){
      sum <- 0
      res <- 0
      for (k in 1:(i-1)){
        sum <- sum + L[i,k]*U[k,j]
        res <- res + L[j,k]*U[k,i]
      }
      U[i,j] <- Aa[i,j] - sum
      L[j,i] <- (1/U[i,i])*(Aa[j,i]-res)
    }
    
  }
  nn = 0
  for (k in 1:(n-1)){
    nn = nn +L[n,k]*U[k,n]
  }
  U[n,n] = Aa[n,n] - nn
 list(L = L, U = U)
}

C <- matrix(c(1, 1, 0, 3, 
              2, 1, -1, 1, 
              3, -1, -1, 2,
              -1, 2, 3, -1), byrow = TRUE, nrow = 4)
L <- LU(4,C)$L
U <- LU(4,C)$U
L%*%U

#Lu ejercicios ------
#LU1 -----------
LU1 <- function(n, Aa){
  L <- diag(n)
  U <- matrix(0, nrow = n, ncol = n)
  U[1,1] = Aa[1,1]
  for (j in 2:n){
    U[1,j] <- Aa[1,j]
    L[j,1] <- Aa[j,1]/Aa[1,1]
  }
  for (i in 2:(n-1)){
    for (k in 1:(i-1)){
      U[i,i] <- Aa[i,i] - L[i,k]*U[k,i]
    }
    for (j in (i+1):n){
      sum <- 0
      res <- 0
      for (k in 1:(i-1)){
        sum <- sum + L[i,k]*U[k,j]
        res <- res + L[j,k]*U[k,i]
      }
      U[i,j] <- Aa[i,j] - sum
      L[j,i] <- (1/U[i,i])*(Aa[j,i]-res)
    }
    
  }
  nn = 0
  for (k in 1:(n-1)){
    nn = nn +L[n,k]*U[k,n]
  }
  U[n,n] = Aa[n,n] - nn
  list(L = L, U = U)
}

D <- matrix(c(4, -1, 1, 
              2, 5, 2, 
              2, 5, 2), byrow = TRUE, nrow = 3)
LU1(3,D)$L
LU1(3,D)$U
#LU2 --------
LU2 <- function(n, Aa){
  L <- diag(n)
  U <- matrix(0, nrow = n, ncol = n)
  U[1,1] = Aa[1,1]
  for (j in 2:n){
    U[1,j] <- Aa[1,j]
    L[j,1] <- Aa[j,1]/Aa[1,1]
  }
  for (i in 2:(n-1)){
    for (k in 1:(i-1)){
      U[i,i] <- Aa[i,i] - L[i,k]*U[k,i]
    }
    for (j in (i+1):n){
      sum <- 0
      res <- 0
      for (k in 1:(i-1)){
        sum <- sum + L[i,k]*U[k,j]
        res <- res + L[j,k]*U[k,i]
      }
      U[i,j] <- Aa[i,j] - sum
      L[j,i] <- (1/U[i,i])*(Aa[j,i]-res)
    }
    
  }
  nn = 0
  for (k in 1:(n-1)){
    nn = nn +L[n,k]*U[k,n]
  }
  U[n,n] = Aa[n,n] - nn
  list(L = L, U = U)
}

E <- matrix(c(4, 1, 2, 
              2, 4, -1, 
              1, 1, -3), byrow = TRUE, nrow = 3)
LU2(3,E)$L
LU2(3,E)$U
#LU3 ------
LU3 <- function(n, Aa){
  L <- diag(n)
  U <- matrix(0, nrow = n, ncol = n)
  U[1,1] = Aa[1,1]
  for (j in 2:n){
    U[1,j] <- Aa[1,j]
    L[j,1] <- Aa[j,1]/Aa[1,1]
  }
  for (i in 2:(n-1)){
    for (k in 1:(i-1)){
      U[i,i] <- Aa[i,i] - L[i,k]*U[k,i]
    }
    for (j in (i+1):n){
      sum <- 0
      res <- 0
      for (k in 1:(i-1)){
        sum <- sum + L[i,k]*U[k,j]
        res <- res + L[j,k]*U[k,i]
      }
      U[i,j] <- Aa[i,j] - sum
      L[j,i] <- (1/U[i,i])*(Aa[j,i]-res)
    }
    
  }
  nn = 0
  for (k in 1:(n-1)){
    nn = nn +L[n,k]*U[k,n]
  }
  U[n,n] = Aa[n,n] - nn
  list(L = L, U = U)
}

F <- matrix(c(2, -1, 1, 
              3, 3, 9, 
              3, 3, 5), byrow = TRUE, nrow = 3)
LU3(3,F)$L
LU3(3,F)$U
#LU4 -------
LU4 <- function(n, Aa){
  L <- diag(n)
  U <- matrix(0, nrow = n, ncol = n)
  U[1,1] = Aa[1,1]
  for (j in 2:n){
    U[1,j] <- Aa[1,j]
    L[j,1] <- Aa[j,1]/Aa[1,1]
  }
  for (i in 2:(n-1)){
    for (k in 1:(i-1)){
      U[i,i] <- Aa[i,i] - L[i,k]*U[k,i]
    }
    for (j in (i+1):n){
      sum <- 0
      res <- 0
      for (k in 1:(i-1)){
        sum <- sum + L[i,k]*U[k,j]
        res <- res + L[j,k]*U[k,i]
      }
      U[i,j] <- Aa[i,j] - sum
      L[j,i] <- (1/U[i,i])*(Aa[j,i]-res)
    }
    
  }
  nn = 0
  for (k in 1:(n-1)){
    nn = nn +L[n,k]*U[k,n]
  }
  U[n,n] = Aa[n,n] - nn
  list(L = L, U = U)
}

G <- matrix(c(2, 0, 0, 0, 
              1, 1.5, 0, 0, 
              0, -3, 0.5, 0,
              2, -2, 1, 1), byrow = TRUE, nrow = 4)
LU4(4,G)$L
LU4(4,G)$U
#LU5 -----
LU5 <- function(n, Aa){
  L <- diag(n)
  U <- matrix(0, nrow = n, ncol = n)
  U[1,1] = Aa[1,1]
  for (j in 2:n){
    U[1,j] <- Aa[1,j]
    L[j,1] <- Aa[j,1]/Aa[1,1]
  }
  for (i in 2:(n-1)){
    for (k in 1:(i-1)){
      U[i,i] <- Aa[i,i] - L[i,k]*U[k,i]
    }
    for (j in (i+1):n){
      sum <- 0
      res <- 0
      for (k in 1:(i-1)){
        sum <- sum + L[i,k]*U[k,j]
        res <- res + L[j,k]*U[k,i]
      }
      U[i,j] <- Aa[i,j] - sum
      L[j,i] <- (1/U[i,i])*(Aa[j,i]-res)
    }
    
  }
  nn = 0
  for (k in 1:(n-1)){
    nn = nn +L[n,k]*U[k,n]
  }
  U[n,n] = Aa[n,n] - nn
  list(L = L, U = U)
}

H <- matrix(c(1.012, -2.132, 3.104,
              -2.132, 4.906, -7.013, 
              3.104, -7.013, 0.014), byrow = TRUE, nrow = 3)
LU5(3,H)$L
LU5(3,H)$U

#Choleski Ejemplo ---------
Choleski <- function(Aa) {
  n <- nrow(Aa)
  L <- matrix(0, nrow = n, ncol = n)
  L[1,1] <- sqrt(Aa[1,1])
  
  for (j in 2:n) {
    L[j,1] <- Aa[j,1] / L[1,1]
  }
  
  for (i in 2:n) {  # Cambié esto a 2:n para incluir el cálculo de L[n,n]
    sum <- 0
    for (k in 1:(i-1)) {
      sum <- sum + (L[i,k])^2
    }
    L[i,i] <- sqrt(Aa[i,i] - sum)
    
    if (i < n) {  # Este control evita la división en el último elemento diagonal
      for (e in (i+1):n) {
        res <- 0
        for (g in 1:(i-1)) {
          res <- res + L[e,g] * L[i,g]
        }
        L[e,i] <- (Aa[e,i] - res) / L[i,i]
      }
    }
  }
  return(L)
}

A <- matrix(c(4, -1, 1,
              -1, 4.25, 2.75,
              1, 2.75, 3.5), byrow = TRUE, nrow = 3)
Choleski(A)
L <-Choleski(A)

#verifico L*L(transp) = A
L%*%t(L)
  
#Choleski ejercicios 
#Choleski1 --------
Choleski1 <- function(Aa) {
  n <- nrow(Aa)
  L <- matrix(0, nrow = n, ncol = n)
  L[1,1] <- sqrt(Aa[1,1])
  
  for (j in 2:n) {
    L[j,1] <- Aa[j,1] / L[1,1]
  }
  
  for (i in 2:n) { 
    sum <- 0
    for (k in 1:(i-1)) {
      sum <- sum + (L[i,k])^2
    }
    L[i,i] <- sqrt(Aa[i,i] - sum)
    
    if (i < n) {  
      for (e in (i+1):n) {
        res <- 0
        for (g in 1:(i-1)) {
          res <- res + L[e,g] * L[i,g]
        }
        L[e,i] <- (Aa[e,i] - res) / L[i,i]
      }
    }
  }
  return(L)
}

B <- matrix(c(2, -1, 0,
              -1, 2, -1,
              0, -1, 2), byrow = TRUE, nrow = 3)
L1 <- Choleski1(B)
L1

#verifico L*L(transp) = A
L1%*%t(L1)
#Choleski2 ----------
Choleski2 <- function(Aa) {
  n <- nrow(Aa)
  L <- matrix(0, nrow = n, ncol = n)
  L[1,1] <- sqrt(Aa[1,1])
  
  for (j in 2:n) {
    L[j,1] <- Aa[j,1] / L[1,1]
  }
  
  for (i in 2:n) { 
    sum <- 0
    for (k in 1:(i-1)) {
      sum <- sum + (L[i,k])^2
    }
    L[i,i] <- sqrt(Aa[i,i] - sum)
    
    if (i < n) {  
      for (e in (i+1):n) {
        res <- 0
        for (g in 1:(i-1)) {
          res <- res + L[e,g] * L[i,g]
        }
        L[e,i] <- (Aa[e,i] - res) / L[i,i]
      }
    }
  }
  return(L)
}

C <- matrix(c(6, 2, 1, -1,
              2, 4, 1, 0,
              1, 1, 4, -1,
              -1, 0, -1, 3), byrow = TRUE, nrow = 4)
L2 <- Choleski1(C)
L2

#verifico L*L(transp) = A
L2%*%t(L2)

#Choleski3 -----------
Choleski3 <- function(Aa) {
  n <- nrow(Aa)
  L <- matrix(0, nrow = n, ncol = n)
  L[1,1] <- sqrt(Aa[1,1])
  
  for (j in 2:n) {
    L[j,1] <- Aa[j,1] / L[1,1]
  }
  
  for (i in 2:n) { 
    sum <- 0
    for (k in 1:(i-1)) {
      sum <- sum + (L[i,k])^2
    }
    L[i,i] <- sqrt(Aa[i,i] - sum)
    
    if (i < n) {  
      for (e in (i+1):n) {
        res <- 0
        for (g in 1:(i-1)) {
          res <- res + L[e,g] * L[i,g]
        }
        L[e,i] <- (Aa[e,i] - res) / L[i,i]
      }
    }
  }
  return(L)
}

C <- matrix(c(4, 1, 1, 1,
              1, 3, -1, 1,
              1, -1, 2, 0,
              1, 1, 0, 2), byrow = TRUE, nrow = 4)
L3 <- Choleski1(C)
L3

#verifico L*L(transp) = A
L3%*%t(L3)
#Choleski4 ------------
Choleski4 <- function(Aa) {
  n <- nrow(Aa)
  L <- matrix(0, nrow = n, ncol = n)
  L[1,1] <- sqrt(Aa[1,1])
  
  for (j in 2:n) {
    L[j,1] <- Aa[j,1] / L[1,1]
  }
  
  for (i in 2:n) { 
    sum <- 0
    for (k in 1:(i-1)) {
      sum <- sum + (L[i,k])^2
    }
    L[i,i] <- sqrt(Aa[i,i] - sum)
    
    if (i < n) {  
      for (e in (i+1):n) {
        res <- 0
        for (g in 1:(i-1)) {
          res <- res + L[e,g] * L[i,g]
        }
        L[e,i] <- (Aa[e,i] - res) / L[i,i]
      }
    }
  }
  return(L)
}

D <- matrix(c(1, 2, 4, 7,
              2, 13, 23, 38,
              4, 23, 77, 122,
              7, 38, 122, 294), byrow = TRUE, nrow = 4)
L4 <- Choleski1(D)
L4

#verifico L*L(transp) = A
L4%*%t(L4)

