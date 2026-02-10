install.packages("ggplot2")
library(ggplot2)

f = function(x){
  cos(x)-sqrt(x)
}
x <- seq(0,2,by = 0.1) #Generamos un vector "x" para crear los puntos en F(x)
fx <- f(x) #Creamos los valores de f(x)

df <- data.frame(x, fx)
gg_fx <- ggplot(data = df, aes(x = x, y = fx)) + geom_line(linetype=1,colour="darkblue")
gg_fx = gg_fx + geom_hline(yintercept = 0,linetype=1)
gg_fx = gg_fx + geom_vline(xintercept = 0,linetype=1)
gg_fx = gg_fx + scale_x_continuous(name = "x", breaks = seq(0,2, by = 1))
gg_fx = gg_fx + scale_y_continuous(name = "y = f(x)", breaks = seq(-2.5,1, by = 0.5))
gg_fx = gg_fx + geom_vline(xintercept = c(0.5,1),linetype=2, col = "red")
gg_fx = gg_fx + ggtitle("Función con raíz entre 0.5 y 1")
gg_fx = gg_fx + geom_abline(intercept = 0,slope=1,linetype=2)

print(gg_fx)

#-------------------------------------------------------------------------------------
g2 <- function(x) {
  y <- (1+x)^(1/3)
  return(y)
}

f2 <- function(x){
  y <- x^3 - x - 1
  y
}
x2pf <- seq(0,4,by = 0.1)
y2pf <- g2(x2pf)
yf2pf <- f2(x2pf)
data2pf <- data.frame(x2pf,y2pf)
data2pff <- data.frame(x2pf,yf2pf)
grafico2pf <- ggplot(data = data2pf, aes(x=x2pf, y=y2pf)) + geom_line(colour="darkgreen", linetype=1)
grafico2pf <- grafico2pf + geom_hline(yintercept=0) + geom_vline(xintercept=0)
grafico2pf <- grafico2pf + scale_x_continuous(name = "x", breaks = seq(0,4, by = 1)) + scale_y_continuous(name = "y = f(x)", breaks = seq(0,2, by = 0.5))
grafico2pf <- grafico2pf + geom_abline(intercept = 0, slope =1, linetype =2,col="blue")
grafico2pf <- grafico2pf + geom_point(aes(x=puntofijo2(1, 10^-2, 600), y=g2(puntofijo2(1, 10^-2, 600))), col="red")
grafico2pff <- ggplot(data=data2pff, aes(x=x2pf, y=yf2pf)) + geom_line(colour = "pink")

grafico2pff
                                                                                     
                                                                                                         
                                                                                                         
                                                                                                         
                                                                                                         