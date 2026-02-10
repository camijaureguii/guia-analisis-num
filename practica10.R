xp <- c(1.0,1.3,1.6,1.9,2.2)
fp <- c(0.7651977 , 0.6200860, 0.2554022, 0.2818186, 0.1103623)
Datos <- data.frame(xp,fp)

MCO4 <- lm(fp~xp+I(xp^2)+I(xp^3)+I(xp^4), data=Datos)

l <- length(MCO4$coefficients)

MCOP4 <- paste("f(x)=",round(MCO4$coefficients[1],4))
MCOP4               
for(i in 2:l){
  MCOP4 <- paste(MCOP4,"+", round(MCO4$coefficients[i],4),"x^",(i-1))
}               
MCOP4 

MCOpunto <- function(x,lm,xp,yp){
  l <- length(lm$coefficients)
  Px <- lm$coefficients[1]
  for (i in 2:l){
    Px <- Px + lm$coefficients[i]*x^(i-1)
  }
  return(Px)
}
MCOpunto(2.2,MCO4,c(1.0,1.3,1.6,1.9,2.2),c(0.7651977 , 0.6200860, 0.2554022, 0.2818186, 0.1103623))

fpx <- seq(from = min(xp), to =max(fp), length.out = 1000)
fxLM <- rep(NA,1000)
fxIL <- rep(NA,1000)
fxLM3 <- rep(NA,1000)
for (i in 1:1000){
  fxLM[i] <- MCOpunto(fpx[i],MCO4,c(1.0,1.3,1.6,1.9,2.2),c(0.7651977 , 0.6200860, 0.2554022, 0.2818186, 0.1103623))
  fxIL[i] <- a1IL(fpx[i],c(1.0,1.3,1.6,1.9,2.2),c(0.7651977 , 0.6200860, 0.2554022, 0.2818186, 0.1103623))
  fxLM3[i] <- MCOpunto(fpx[i],MCO3,c(1.0,1.3,1.6,1.9,2.2),c(0.7651977 , 0.6200860, 0.2554022, 0.2818186, 0.1103623))
}
plot(fpx,fxLM,col = "yellow")
lines(fpx,fxIL,col="blue")
plot(fpx,fxLM3, col="darkviolet")

summary(MCO4) #Los residuos son cero, entonces el polinomio está interpolando los valores de la función
---------------------------------------------------------------------------------------------------------
MCO3 <- lm(fp~xp+I(xp^2)+I(xp^3), data=Datos)
l2 <- length(MCO3$coefficients)

MCOP3 <- paste("f(x)=",round(MCO3$coefficients[1],4))
MCOP3               
for(i in 2:l2){
  MCOP3 <- paste(MCOP3,"+", round(MCO3$coefficients[i],4),"x^",(i-1))
}               
MCOP3             
summary(MCO3)
---------------------------------------------------------------------------------------
#Nelson-Siegel
#install.packages("YieldCurve")
library(YieldCurve)
NS = Nelson.Siegel(fp,xp)
NSCurve <- function(x){
  b0 <- NS[1]
  b1 <- NS[2]
  b2 <- NS[3]
  m <- NS[4]
  return(b0 + b1*(1-exp(-m*x))/(m*x)+b2*(1-exp(-m*x))/(m*x)-exp(-m*x))
}
curve(NSCurve, 0, 30, add=T, col='blue')

----------------------------------------------------------------------------
  #AJUSTAR DATOS Y GRAFICO----
xp <- c(0.0227, 0.0817, 0.3147, 0.5258, 0.7502, 0.8877, 1.3583, 1.3716, 1.5854, 1.6288, 2.8558, 3.0106, 5.1854)
fp <- c(0.0769, 0.1538, 0.2308, 0.3077, 0.3846, 0.4615, 0.5385, 0.6154, 0.6923, 0.7692, 0.8462, 0.9231, 1.0000)
Datos <- data.frame(xp, fp)
Datos

# Ajuste de la distribución normal usando nls
Modelo <- nls(fp ~ pnorm(xp, mu, sigma),
              data = Datos,
              start = list(mu = 1.5, sigma = 0.5))

# Resumen del modelo
summary(Modelo)

ajusteNL <- data.frame(x = seq(from = min(Datos$x), to = max(Datos$x), length.out = 100))
ajusteNL$fit <- predict(Modelo, newdata = list(xp = ajusteNL$x))


# Gráfico
plot(Datos$x, Datos$fp, pch = 16, col = "black", xlab = "x", ylab = "F(x)")
lines(ajusteNL$x, ajusteNL$fit, col = 'red', lty = 'dashed')
mu_muestra <- mean(Datos$xp)
sigma_muestra <- sd(Datos$xp)
curve(pnorm(x, mu_muestra, sigma_muestra), col = 'blue', add = TRUE)
legend('bottomright', legend = c("datos", "NLS","Momentos"), 
       lty = c(0, 2,1), pch = c(16, NA,NA),
       col = c("black", "red","blue"))
title('Ajuste de datos a Distribución Normal', cex = 0.8)

#APROXIMACION EN EL PUNTO

x_aprox <- 0.0522
F_aprox_NLS <- pnorm(x_aprox, coef(Modelo)[1], coef(Modelo)[2])
F_aprox_NLS




