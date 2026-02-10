f1 <- function(x){
  x^2
}
n <-10000
U <- runif(n) #generacion de n num uniformes

Integral.MC <- 1/n*sum(f1(U)) #estimo la integral
sf.MC <- sqrt(1/(n-1)*sum((f1(U)-Integral.MC)^2)) #estimación de sigma
Error.MC <- sf.MC/sqrt(n) #error en la integral de MC

Integral.MC    #solucion exacta es 1/3
Error.MC
--------------------------------------------------------------
f2 <- function(x) { x^2 }

# Set the sample size
n <- 10000

# Define the interval limits
a <- 10
b <- 15
U2 <- a + (b - a) * runif(n) # Generate n uniform random numbers between a and b

Altura.promedio <- 1/n*sum(f2(U2))  #valor esperado de la altura media
Ancho.base <- b-a
Integral.MC <- Altura.promedio*Ancho.base
sf.MC <- sqrt(1/(n-1)*sum((f2(U2)*(b-a)-Integral.MC)^2)) #estimación de sigma
Error.MC <- sf.MC/sqrt(n) #error en la integral de MC
Error.MC
--------------------------------------------------------------------------------
f1 <- function(x) {
    1 / sqrt(2 * pi * 10^2) * exp(-((x - 5)^2) / (2 * 10^2))    #Normal con U=5 y sigma=10
  }

n <- 100 # Set the number of samples for Monte Carlo simulation

# Define the integration interval [a, b]
a <- 10
b <- 20

U <- a + (b - a) * runif(n)
Altura.Promedio <- 1/n * sum(f1(U))

Ancho.Base <- b - a # Calculate the width of the base (interval length)

Integral.MC <- Altura.Promedio * Ancho.Base # Compute the Monte Carlo integral estimation

# Calculate the standard error of the Monte Carlo integral
sf.MC <- sqrt(1 / (n - 1) * sum((f1(U)*(b-a) - Integral.MC)^2))
Error.MC <- sf.MC / sqrt(n)

Integral.MC 
pnorm(20,5,10)-pnorm(10,5,10) #0.2417 la exacta
Error.MC

#Notar que el intervalo de confianza contiene al valor calculado exacto:
Integral.MC+2*Error.MC
Integral.MC-2*Error.MC
------------------------------------------------------------------------------
# Generate a single Poisson random variable
N <- rpois(1, lambda = 50) #n= cant de variables q queres que devuelva

# Generate N Gamma-distributed random variables
Xi <- rgamma(N, shape = 10, scale = 5)

# Calculate the sum of the Gamma-distributed variables
S <- sum(Xi)
------------------------------------------------------------------------------
M <- 10000

Resultado <- matrix(NA, nrow = M, ncol = 2) # Create the matrix to store the output

# Loop over the number of simulations
for (m in 1:M) {
  # Generate a random number from a Poisson distribution with lambda = 50
  N <- rpois(1, lambda = 50)
  
  # Store the result in the first column of the matrix
  Resultado[m, 1] <- N
  
  # Generate N random numbers from a Gamma distribution
  Xi <- rgamma(N, shape = 10, scale = 5)
  
  # Sum the Gamma-distributed variables
  S <- sum(Xi)
  
  # Store the sum in the second column of the matrix
  Resultado[m, 2] <- S
}

# Calculate the mean and variance of N
N.mean <- mean(Resultado[, 1])
N.var <- var(Resultado[, 1])

# Calculate the mean and standard deviation of S
S.mean <- mean(Resultado[, 2])
S.sd <- sd(Resultado[, 2])

# Create a histogram of the sum values from the simulation
hist(Resultado[, 2], main = "Histogram of Sum S", xlab = "Sum S", col = "blue")  
--------------------------------------------------------------------------
#Punto 1 de la guía
MonteCarlo <- function(f,n,a,b){
  U <- a + (b - a) * runif(n)
  Altura.promedio <- 1/n*sum(f(U))  #valor esperado de la altura media
  Ancho.base <- b-a
  Integral.MC <- Altura.promedio*Ancho.base
  sf.MC <- sqrt(1/(n-1)*sum((f(U)*(b-a)-Integral.MC)^2)) #estimación de sigma
  Error.MC <- sf.MC/sqrt(n) #error en la integral de MC
  print(paste("la integral es",Integral.MC,"y el error es",Error.MC))
}
library(ggplot2)
areagraf <- function(f,a,b){
  x<- seq(a,b,by=0.1)
  y <- f(x)
  df <- data.frame(x,y)
  plot <- ggplot(data=df, aes(x=x,y=y)) + geom_line(linetype=1, colour="blue") + geom_area(fill="skyblue",alpha = 0.5)
  plot <- plot + geom_hline(linetype=1, yintercept=0) + geom_vline(xintercept = 0,linetype=1)
  print(plot)
}

f1a <- function(x) { 
  y <- sqrt(x+5)*sin(x) 
  y
}
set.seed(123)
MonteCarlo(f1a,10000,2,6)
areagraf(f1a,2,6)

f1b <- function(x) { 
  y <- 1 / sqrt(2 * pi * 5^2) * exp(-((x - 7)^2) / (2 * 5^2))
  y
}
set.seed(123)
MonteCarlo(f1b,10000,5,8)
areagraf(f1b,5,8)

f1c <- function(x) { 
  y <- x^3 + 4*x^2 + 2
  y
}
MonteCarlo(f1c,1000000,-2,5)
areagraf(f1c,-2,5)


f1d <- function(x) { 
  y <- x*log(x^3) + 12*cos(x)
  y
}
MonteCarlo(f1d,1000000,12,20)
areagraf(f1d,12,20)
-------------------------------------------------------------------------------
#Punto 2
M <- 10000

Resultado <- matrix(NA, nrow = M, ncol = 2) # Create the matrix to store the output

# Loop over the number of simulations
for (m in 1:M) {
  N <- rbinom(1,1200,0.7984)
  
  # Store the result in the first column of the matrix
  Resultado[m, 1] <- N
  
  # Generate N random numbers from a Gamma distribution
  Xi <- rchisq(N,df=2)
  
  # Sum the Gamma-distributed variables
  S <- sum(Xi)
  
  # Store the sum in the second column of the matrix
  Resultado[m, 2] <- S
}

# Calculate the mean and variance of N
N.mean <- mean(Resultado[, 1])
N.var <- var(Resultado[, 1])

# Calculate the mean and standard deviation of S
S.mean <- mean(Resultado[, 2])
S.sd <- sd(Resultado[, 2])

N.mean
N.var
S.mean
S.sd

# Create a histogram of the sum values from the simulation
hist(Resultado[, 2], main = "Histogram of Sum S", xlab = "Sum S", col = 175)   
------------------------------------------------------------------------------
#Simulación de precios
SP <- function(p0,mu,sigma,T,n,m){
  dt <- T/n
  
  # Crear matriz para almacenar los caminos de los precios
  Pt <- matrix(NA, nrow = m, ncol = n+1)
  Pt[,1] <- p0  # Establecer el precio inicial en la primera columna
  
  # Bucle para simular cada camino de precio
  for (i in 1:m) {
    for (t in 2:(n+1)) {
      Pt[i,t] <- Pt[i,t-1] * exp((mu - 0.5 * sigma^2) * dt + sigma * sqrt(dt) * rnorm(1))
    }
  }
  return(Pt)
}
SP(45,0.1,0.2,0.5,182,1000)

Grafprecios <- function(p0,mu,sigma,T,n,m){
  Pt <- SP(p0,mu,sigma,T,n,m)
  t <- rep(0:n,m)  # Crea el vector de tiempo de 0 a "n" y lo repite "m" veces
  t <- matrix(t, nrow = m, ncol = n+1, byrow = TRUE)  # Ordena el vector anterior en una matriz
  # Grafica el primer camino de precios
  plot(t[1,], Pt[1,], type = "l", ylim = c(min(Pt), max(Pt)))
  
  # Grafica los restantes caminos
  for (i in 2:m) {
    lines(t[i,], Pt[i,], col = trunc(runif(1) * m)) #se trunca a un entero con trunc.
  }
  print(plot)
  
  # Calcular el promedio de cada columna
  M <- matrix(NA, nrow = 1, ncol = n+1)
  for (i in 1:(n+1)) {
    M[i] <- mean(Pt[,i])
  }
  
  prob <- 0.95  # Nivel de confianza
  # Crear matrices para almacenar los percentiles superior e inferior
  LS <- matrix(NA, nrow = 1, ncol = n+1)  # Límite Superior
  LI <- matrix(NA, nrow = 1, ncol = n+1)  # Límite Inferior
  # Calcular los percentiles para cada columna
  for (i in 1:(n+1)) {
    LS[i] <- quantile(Pt[,i], prob)
    LI[i] <- quantile(Pt[,i], 1-prob)
  }
  
  lines(t[1,], M, col = 'black', lwd = 5)
  lines(t[1,], LS, col = 'red', lwd = 5)
  lines(t[1,], LI, col = 'red', lwd = 5)
  
  #ultimo día valores:
  print(paste("en el dia 183, la media fue",M[183],"el percentil superior",LS[183],"y el inferior",LI[183]))
}
Grafprecios(45,0.1,0.2,0.5,182,1000)

# Simulación del Precio Final
SPF <- function(p0,mu,sigma,T,m){
  epsilon <- rnorm(m) # Generar m valores aleatorios de una distribución normal estándar
  
  PT <- matrix(NA, nrow = m, ncol = 1) # Crear una matriz para almacenar los precios finales
  PT <- p0 * exp((mu - 0.5 * sigma^2) * T + sigma * sqrt(T) * epsilon)
  M <- mean(PT)
  print(M)
  hist(PT) # Crear un histograma de los precios finales
}
SPF(45,0.1,0.2,0.5,1000)
-----------------------------------------------------------------------------------------------
#Simulación de precios de acciones con errores independientes
p0 <- c(45, 50, 108)
mu <- c(0.10, 0.15, 0.08)
sigma <- c(0.20, 0.25, 0.30)
T <- 0.5  # seis meses
m <- 1000  # Cantidad de caminos de precios

# Generar números con distribución Normal Estándar independientes
z <- matrix(rnorm(3 * m), nrow = m, ncol = 3)
cor(z)
#no hay correlación, tienden a cero los valores

# Creación de la matriz de precios
P <- matrix(NA, nrow = m, ncol = 3)  # 'm' simulaciones, 3 activos
# Simulación de precios para cada activo
for (i in 1:m) {  
  for (k in 1:3) {
    P[i, k] <- p0[k] * exp((mu[k] - 0.5 * sigma[k]^2) * T + sigma[k] * sqrt(T) * z[i, k])
  }
}

P0.m <- matrix(rep(p0, m), nrow = m, ncol = 3, byrow = TRUE)

RL <- log(P / P0.m) # Cálculo de rendimientos logarítmicos
corRL <- cor(RL) # Cálculo de correlaciones de los rendimientos logarítmicos
-------------------------------------------------------------------------------------
#Simulación de precios de acciones con errores dependientes (precios correlacionados)
Rho <- diag(3)  # Crea una matriz diagonal de 3x3 con unos en la diagonal
Rho[1, 2] <- Rho[2, 1] <- 0.90
Rho[1, 3] <- Rho[3, 1] <- 0.70
Rho[2, 3] <- Rho[3, 2] <- 0.60

CH <- chol(Rho) # Cálculo de la factorización de Cholesky de la matriz de correlaciones
z <- matrix(rnorm(3 * m), nrow = m, ncol = 3) # Generación de números aleatorios estándar independientes
e <- z %*% CH # Aplicación de la matriz de Cholesky para obtener números correlacionados
cor_e <- cor(e)
Pc <- matrix(NA, nrow = m, ncol = 3)  # 'm' simulaciones, 3 activos

# Bucle para simular los precios de cada activo, teniendo en cuenta las correlaciones
for (i in 1:m) {  
  for (k in 1:3) {  
    Pc[i, k] <- p0[k] * exp((mu[k] - 0.5 * sigma[k]^2) * T + sigma[k] * sqrt(T) * e[i, k])
  }
}

P0.m <- matrix(rep(p0, m), nrow = m, ncol = 3, byrow = TRUE)
RLc <- log(Pc / P0.m)
corRLc <- cor(RLc) #misma correlacion que los errores dependientes
mediaRLc <- mean(RLc[,1])
mediaRLc

par(mar = c(3,3,3,3)) #agarnda margenes para que entren los 6 graficos
par(mfrow = c(2, 3)) #configura el área de trazado para mostrar una matriz de gráficos con 2 filas y 3 columnas. 

# Gráficos de dispersión para precios no correlacionados
plot(P[, 1], P[, 2], main = "P1 vs P2", xlab = "P1", ylab = "P2")
plot(P[, 1], P[, 3], main = "P1 vs P3", xlab = "P1", ylab = "P3")
plot(P[, 2], P[, 3], main = "P2 vs P3", xlab = "P2", ylab = "P3")

# Gráficos de dispersión para precios correlacionados
plot(Pc[, 1], Pc[, 2], main = "Pc1 vs Pc2", xlab = "Pc1", ylab = "Pc2")
plot(Pc[, 1], Pc[, 3], main = "Pc1 vs Pc3", xlab = "Pc1", ylab = "Pc3")
plot(Pc[, 2], Pc[, 3], main = "Pc2 vs Pc3", xlab = "Pc2", ylab = "Pc3")

#calculo los precios que van a tener en el ultimo dia
Q <- c(100, 150, 120)  # Cantidades de cada activo

P0 <- c(45, 50, 108)  # Precios iniciales de cada activo
V0 <- Q * P0  # Valor de cada activo
VI <- sum(V0)  # Valor inicial total de la cartera

Q.m <- matrix(rep(Q, m), nrow = m, ncol = 3, byrow = TRUE) #cantidades de cada activo repetida m veces

# Simulación de los valores de los activos al final del periodo
VT <- Q.m * P  # Matriz de valores en el horizonte T de los activos independientes
VF <- matrix(rowSums(VT), ncol = 1)  # Vector de valores finales de la cartera, suma total de la cartera en cada simulacion

RL.V <- log(VF/VI)
par(mfrow = c(1, 1))
hist(RL.V)

VTc <- Q.m * Pc  # Matriz de valores en el horizonte T de los activos dependientes
VFc <- matrix(rowSums(VTc), ncol = 1)  # Vector de valores finales de la cartera, suma total de la cartera en cada simulacion
RL.Vc <- log(VFc/VI)
par(mfrow = c(1, 1))
hist(RL.Vc)

#grafico para ver ambos histogramas juntos
par(mar = c(2,3,2,3))
par(mfrow = c(2, 1))

# Histograma para los retornos de valores independientes
tmp <- hist(RL.V, xlim = c(-0.5, 1), ylim = c(0, 300), axes = FALSE, main = "Histogram of RV")
axis(side = 1, at = seq(-0.5, 1, by = 0.1))
axis(side = 2)

# Histograma para los retornos de valores correlacionados
tmp <- hist(RL.Vc, xlim = c(-0.5, 1), ylim = c(0, 300), axes = FALSE, main = "Histogram of RVc")
axis(side = 1, at = seq(-0.5, 1, by = 0.1))
axis(side = 2)

-----------------------------------------------------------------------------------------------
#Punto 3
library(quantmod)
getSymbols("YPFD.BA", auto.assign = TRUE, src = "yahoo") 
getSymbols("MELI.BA", auto.assign = TRUE, src = "yahoo") 
getSymbols("LOMA.BA", auto.assign = TRUE, src = "yahoo")

set.seed(123)
mu <- c(0.15,0.12,0.3)
sigma <- c(0.2,0.19,0.42)

p0YPF <- as.numeric(YPFD.BA['2020-11-06', "YPFD.BA.Adjusted"])
p0MELI <- as.numeric(MELI.BA['2020-11-06',"MELI.BA.Adjusted"])
p0LOMA <- as.numeric(LOMA.BA['2020-11-06',"LOMA.BA.Adjusted"])
p0 <- c(p0YPF,p0MELI,p0LOMA)

SPF1 <- function(p0,mu,sigma,T,m){
  epsilon <- rnorm(m) # Generar m valores aleatorios de una distribución normal estándar
  
  PT <- matrix(NA, nrow = m, ncol = 1) # Crear una matriz para almacenar los precios finales
  PT <- p0 * exp((mu - 0.5 * sigma^2) * T + sigma * sqrt(T) * epsilon)
  M <- mean(PT)
  qs <- quantile(PT,0.975)
  qi <- quantile(PT,0.025)
  Rend <- log(PT/p0)
  rendmedia <- mean(Rend)
  print(paste("la media es",M,", el quantil superior es",qs,"y el inferior",qi," y el rendimiento esperado",rendmedia))
}
set.seed(123)
SPF1(p0YPF,mu[1],sigma[1],1,10000) 
SPF1(p0MELI,mu[2],sigma[2],1,10000)
SPF1(p0LOMA,mu[3],sigma[3],1,10000)

#punto d
# Generar números con distribución Normal Estándar dependientes
correlacionP <- function(p0,mu,sigma,T,m,Rho12,Rho13,Rho23){
  z <- matrix(rnorm(3 * m), nrow = m, ncol = 3)
  
  Rho <- diag(3)  # Crea una matriz diagonal de 3x3 con unos en la diagonal
  Rho[1, 2] <- Rho[2, 1] <- Rho12
  Rho[1, 3] <- Rho[3, 1] <- Rho13
  Rho[2, 3] <- Rho[3, 2] <- Rho23
  
  CH <- chol(Rho) # Cálculo de la factorización de Cholesky de la matriz de correlaciones
  e <- z %*% CH # Aplicación de la matriz de Cholesky para obtener números correlacionados
  
  Pc <- matrix(NA, nrow = m, ncol = 3)  # 'm' simulaciones, 3 activos
  
  # Bucle para simular los precios de cada activo, teniendo en cuenta las correlaciones
  for (i in 1:m) {  
    for (k in 1:3) {  
      Pc[i, k] <- p0[k] * exp((mu[k] - 0.5 * sigma[k]^2) * T + sigma[k] * sqrt(T) * e[i, k])
    }
  }
  
  P0.m <- matrix(rep(p0, m), nrow = m, ncol = 3, byrow = TRUE)
  RLc <- log(Pc / P0.m)
  mediaRLc1 <- mean(RLc[,1])
  mediaRLc2 <- mean(RLc[,2])
  mediaRLc3 <- mean(RLc[,3])
  mediaPc1 <- mean(Pc[,1])
  mediaPc2 <- mean(Pc[,2])
  mediaPc3 <- mean(Pc[,3])
  print(paste("el precio esperado de YPF es",mediaPc1,", el de MELI",mediaPc2," y el de LOMA",mediaPc3))
  print(paste("el rendimiento esperado de YPF es",mediaRLc1,", el de MELI",mediaRLc2," y el de LOMA",mediaRLc3))
}
set.seed(123)
correlacionP(p0,mu,sigma,1,10000,0.9,0.7,0.6)

IncorrelacionP <- function(p0,mu,sigma,T,m){
  z <- matrix(rnorm(3 * m), nrow = m, ncol = 3)
  P <- matrix(NA, nrow = m, ncol = 3)  # 'm' simulaciones, 3 activos
  for (i in 1:m) {  
    for (k in 1:3) {
      P[i, k] <- p0[k] * exp((mu[k] - 0.5 * sigma[k]^2) * T + sigma[k] * sqrt(T) * z[i, k])
    }
  }
  P0.m <- matrix(rep(p0, m), nrow = m, ncol = 3, byrow = TRUE)
  RL <- log(P / P0.m)
  mediaP1 <- mean(P[,1])
  mediaP2 <- mean(P[,2])
  mediaP3 <- mean(P[,3])
  print(paste("el precio esperado de YPF es",mediaP1,", el de MELI",mediaP2," y el de LOMA",mediaP3))
}    #no lo pide 
set.seed(123)
IncorrelacionP(p0,mu,sigma,1,10000)

histogramaprecios <- function(p0,mu,sigma,T,m,Rho12,Rho13,Rho23){
  z <- matrix(rnorm(3 * m), nrow = m, ncol = 3)
  Rho <- diag(3)  # Crea una matriz diagonal de 3x3 con unos en la diagonal
  Rho[1, 2] <- Rho[2, 1] <- Rho12
  Rho[1, 3] <- Rho[3, 1] <- Rho13
  Rho[2, 3] <- Rho[3, 2] <- Rho23
  CH <- chol(Rho)
  e <- z %*% CH 
  Pc <- matrix(NA, nrow = m, ncol = 3)  # 'm' simulaciones, 3 activos
  
  for (i in 1:m) {  
    for (k in 1:3) {  
      Pc[i, k] <- p0[k] * exp((mu[k] - 0.5 * sigma[k]^2) * T + sigma[k] * sqrt(T) * e[i, k])
    }
  }
  
  z <- matrix(rnorm(3 * m), nrow = m, ncol = 3)
  P <- matrix(NA, nrow = m, ncol = 3)  # 'm' simulaciones, 3 activos
  for (i in 1:m) {  
    for (k in 1:3) {
      P[i, k] <- p0[k] * exp((mu[k] - 0.5 * sigma[k]^2) * T + sigma[k] * sqrt(T) * z[i, k])
    }
  }
  
  par(mar = c(3,3,3,3)) #agarnda margenes para que entren los 6 graficos
  par(mfrow = c(2, 3))
  plot(P[, 1], P[, 2], main = "P1 vs P2", xlab = "P1", ylab = "P2")
  plot(P[, 1], P[, 3], main = "P1 vs P3", xlab = "P1", ylab = "P3")
  plot(P[, 2], P[, 3], main = "P2 vs P3", xlab = "P2", ylab = "P3")
  
  # Gráficos de dispersión para precios correlacionados
  plot(Pc[, 1], Pc[, 2], main = "Pc1 vs Pc2", xlab = "Pc1", ylab = "Pc2")
  plot(Pc[, 1], Pc[, 3], main = "Pc1 vs Pc3", xlab = "Pc1", ylab = "Pc3")
  plot(Pc[, 2], Pc[, 3], main = "Pc2 vs Pc3", xlab = "Pc2", ylab = "Pc3")
}
histogramaprecios(p0,mu,sigma,1,10000,0.9,0.7,0.6)

#punto f
Q <- c(200, 120,0)  # Cantidades de cada activo
set.seed(123)
YPF_MELI_precios <- function(p0,mu,sigma,T,m,Rho12,Rho13,Rho23,Q){
  z <- matrix(rnorm(3 * m), nrow = m, ncol = 3)
  Rho <- diag(3)  # Crea una matriz diagonal de 3x3 con unos en la diagonal
  Rho[1, 2] <- Rho[2, 1] <- Rho12
  Rho[1, 3] <- Rho[3, 1] <- Rho13
  Rho[2, 3] <- Rho[3, 2] <- Rho23
  CH <- chol(Rho)
  e <- z %*% CH 
  Pc <- matrix(NA, nrow = m, ncol = 3)  # 'm' simulaciones, 3 activos
  
  for (i in 1:m) {  
    for (k in 1:3) {  
      Pc[i, k] <- p0[k] * exp((mu[k] - 0.5 * sigma[k]^2) * T + sigma[k] * sqrt(T) * e[i, k])
    }
  }
  
  P <- matrix(NA, nrow = m, ncol = 3)  # 'm' simulaciones, 3 activos
  for (i in 1:m) {  
    for (k in 1:3) {
      P[i, k] <- p0[k] * exp((mu[k] - 0.5 * sigma[k]^2) * T + sigma[k] * sqrt(T) * z[i, k])
    }
  }
  
  V0 <- Q * p0  
  VI <- sum(V0)  
  
  Q.m <- matrix(rep(Q, m), nrow = m, ncol = 3, byrow = TRUE) #cantidades de cada activo repetida m veces
  
  # Simulación de los valores de los activos al final del periodo
  VT <- Q.m * P  
  VTmYPF <- mean(VT[,1])
  VTmMELI <- mean(VT[,2])
  VF <- matrix(rowSums(VT), ncol = 1)
  VFm <- mean(VF)
  RL.V <- log(VF/VI)
  
  VTc <- Q.m * Pc  
  VTcmYPF <- mean(VTc[,1])
  VTcmMELI <- mean(VTc[,2])
  VFc <- matrix(rowSums(VTc), ncol = 1)
  VFcm <- mean(VFc)
  RL.Vc <- log(VFc/VI)
  print(paste("el retorno esperado independiente para YPF es", VTmYPF,"y para MELI",VTmMELI,"y el total",VFm,"y dependiente es para YPF",VTcmYPF,"y para MELI",VTcmMELI, "y el total",VFcm))
}
YPF_MELI_precios(p0,mu,sigma,1,10000,0.9,0.7,0.6,Q)
-----------------------------------------------------------------------------------
#Metodo de Euler
Euler <- function(f,a,b,N,alpha){
  h <- (b-a)/N
  t <- matrix(seq(a,b,h),ncol=1)
  w <- matrix(rep(NA,N+1), ncol=1)
  w[1] <- alpha
  for(i in 1:N){
    w[i+1] <- w[i] + h*f(t[i],w[i])
  }
  dy <- cbind(t,w)
  return(dy)
}

f1a <- function(t,y){
  dt <- t - y + 2
  dt
}
E1a <- Euler(f1a,0,1,10,2)
E1A <- data.frame(E1a)

f1b <- function(t,y){
  dydt <- y - t^2 +1 
  dydt
}
E1b <- Euler(f1b,0,2,10,0.5)
E1B <- data.frame(E1b)

#install.packages("deSolve")
library(deSolve)
solexacta <- function(f,a,b,h,alpha){
  y_initial <- alpha
  times <- seq(a, b, length.out = h+1)  # 10 intervalos, 11 puntos
  solution <- ode(y = y_initial, times = times, func = f)
  return(solution)
}

f1a_ode <- function(t, y, params) {
  dydt <- t - y + 2
  list(dydt)
}
f1aode <- solexacta(f1a_ode,0,1,10,2)

plot(f1aode,type='l',col='blue')
lines(E1A,type='b',col='red')

f1b_ode <- function(t, y, params) {
  dydt <- y - t^2 + 1
  list(dydt)
}
f1bode <- solexacta(f1b_ode,0,2,10,0.5)

dev.off()
plot(f1bode, type = 'l', col = 'blue')
lines(E1B, type='b',col='red')
legend('topleft', legend=c("Exacta", "Euler"), col=c("blue", "red"), lty=c(1,1), pch=c(NA,1))

----------------------------------------------------------------------------------------
RK <- function(f,a,b,N,alpha){
  h <- (b-a)/N
  t <- matrix(seq(a,b,h),ncol=1)
  w <- matrix(rep(NA,N+1), ncol=1)
  w[1]= alpha
  for(i in 1:N){
    K1 = h*f(t[i],w[i])
    K2 = h*f(t[i]+h/2, w[i]+K1/2)
    K3 = h*f(t[i]+h/2, w[i]+K2/2)
    K4 = h*f(t[i]+h, w[i]+K3)
    w[i+1] = w[i] + (K1 + 2*K2 + 2*K3 + K4)/6
  }
  dy <- cbind(t,w)
  return(dy)
}
e1b <- RK(f1b,0,2,10,0.5)
e1B <- data.frame(e1b)

lines(e1b,type="c",col="green")

f2b <- function(t,y){
  dydt <- -20*y + 7*exp(-t/2)
  dydt
}
RK(f2b,0,0.1,10,5)

----------------------------------------------------------------------------------
#Punto 3
f3 <- function(t,y){
  dydt <- exp(sin(t*y))+ sin(log(y))
  dydt
}  
e3 <- Euler(f3,0,4*pi,100,0.01)
rk3 <- RK(f3,0,4*pi,100,0.01)
E3 <- data.frame(e3)
RK3 <- data.frame(rk3)

f3_ode <- function(t, y, params) {
  dydt <- exp(sin(t*y))+ sin(log(y))
  list(dydt)
}
f3ode <- solexacta(f3_ode,0,4*pi,100,0.01)

plot(RK3,type='l', col="red")
lines(E3, type='b',col='darkgreen')
lines(f3ode,col="blue")

#Punto 4
f4 <- function(t,y){
  dydt <- 1 + (t-y)^2
  dydt
}  
e4a <- Euler(f4,2,3,20,1)
e4b <- Euler(f4,2,3,80,1)
e4a[21,2]
e4b[81,2]

rk4a <- RK(f4,2,3,20,1)
rk4b <- RK(f4,2,3,80,1)
rk4a[21,2]
rk4b[81,2]

#Punto 5
N <- (2-1)/0.1

f5 <- function(t,y){
  dydt <- 2*y/t + (t^2)*exp(t)
  dydt
}  
e5 <- Euler(f5,1,2,N,0)
e5

f5exact <- function(t){
  dydt <- (t^2)*(exp(t)-exp(1))
  dydt
}  
x <- seq(1,2,0.1)
exact5 <- f5exact(x)

yder <- f5(e5[,1],e5[,2])

tabla5 <- cbind(e5,yder,exact5)
tabla5

