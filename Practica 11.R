
Tabla <- data.frame(x=seq(from=0, to=3*pi, by=pi/4), fx=sin(seq(from=0,to=3*pi,by=pi/4)))
plot(Tabla, type='p')
title('fx=seno(x)')
abline(h=0)
curve(sin,min(Tabla$x),max(Tabla$x),lwd=2,col="grey",add=T) #grafico la funcion

#funcion derivada: f'(x) = cos(x)
curve(cos,min(Tabla$x),max(Tabla$x),lwd=2,col="magenta")
title("f'(x)=coseno(x)")
abline(h=0)
f1= cos(Tabla$x)
points(Tabla$x,f1,col="black")

#aproximación de la derivada
h=0.5 #aproximo la derivada con 0.5 y grafico
x = Tabla$x
f1.aprox_0.5 = (sin(x+h) - sin(x))/h
lines(x,f1.aprox_0.5,col="red")

h2=0.1 #aproximo la derivada con 0.5 y grafico
f1.aprox_0.1 = (sin(x+h2) - sin(x))/h2
points(x,f1.aprox_0.1,col="blue")

h3=0.01 #aproximo la derivada con 0.5 y grafico
f1.aprox_0.01 = (sin(x+h3) - sin(x))/h3
points(x,f1.aprox_0.01,col="green")

#Aproximo la derivada en base a los datos
h4= diff(Tabla$x)
delta_f = diff(Tabla$fx)
f1.DATOS = c(delta_f/h4,NA)
points(x,f1.DATOS,col="black", pch=11, cex=1.2)

Resultados = data.frame(x=Tabla$x,fx=Tabla$fx,f1,f1.aprox_0.5,f1.aprox_0.1,f1.aprox_0.01,f1.DATOS)

-------------------------------------------------------------------------------------------------
#Derivación numérica
#Punto 1 
dif_numh <- function(x0,x1,fx0,fx1){
  h <- x1-x0
  fder <- (fx1-fx0)/h
  return(fder)
} 
#1a)
dif_numh(0.5,0.6,0.4794,0.5646)
dif_numh(0.6,0.5,0.5646,0.4794) #regresivo pto medio
dif_numh(0.6,0.7,0.5646,0.6442) #progresivo pto medio
dif_numh(0.7,0.6,0.6442,0.5646)

#1b)
dif_numh(0,0.2,0,0.74140)
dif_numh(0.2,0,0.74140,0) #regresivo pto medio
dif_numh(0.2,0.4,0.74140,1.3718) #progresivo pto medio
dif_numh(0.4,0.2,1.3718,0.74140)
-----------------------------------------------------------------------------------
#Punto 2
p3_ext <- function(xe,x0,fxe,fx0,fx1){
  h <- abs(xe-x0)
  if(xe<x0){
    fder <- (1/(2*h))*(-3*fxe+4*fx0-fx1)
    return(fder)
  }
  else{
    fder <- (1/(2*h))*(3*fxe-4*fx0+fx1)
    return(fder)
  }
}
  
p3_medio <- function(x0,xm,fx0,fx1){
  h <- xm-x0
  fder <- (1/(2*h))*(-1*fx0+fx1)
  return(fder)
} 

#2a)
p3_ext(2.9,3,-4.827866,-4.240058,-3.496909)
p3_medio(2.9,3,-4.827866,-3.496909)
p3_medio(3,3.1,-4.240058,-2.596792)
p3_ext(3.2,3.1,-2.596792,-3.496909,-4.240058)  

#2b)
p3_ext(8.1,8.3,16.94410,17.56492,18.19056)
p3_medio(8.1,8.3,16.94410,18.19056)
p3_medio(8.3,8.5,17.56492,18.82091)
p3_ext(8.7,8.5,18.82091,18.19056,17.56492) 
------------------------------------------------------------------------------------
#Punto 3
p5_ext <- function(xe,x0,fxe,fx0,fx1,fx2,fx3){
  h <- abs(xe-x0)
  if(xe<x0){
    fder <- (1/(12*h))*(-25*fxe+48*fx0-36*fx1+16*fx2-3*fx3) 
    return(fder)
  }
  else{
    fder <- (1/(12*h))*(25*fxe-48*fx0+36*fx1-16*fx2+3*fx3) 
    return(fder)
  }
}  
 
p5_medio <- function(xm,x1,fx0,fx1,fx2,fx3){
  h <- xm-x1
  fder <- (1/(12*h))*(fx0-8*fx1+8*fx2-fx3)
  return(fder)
}

p5_ext(0.2,0.4,0.9798652,0.9177710,0.8080348,0.6386093,0.3843735)  #3a)
p5_ext(1,0.8,0.3843735,0.6386093,0.8080348,0.9177710,0.9798652)    #3b)
p5_medio(0.6,0.4,0.9798652,0.9177710,0.6386093,0.3843735)          #3c)
--------------------------------------------------------------------------------------
#Punto 4
dif_numh <- function(x0,x1,fx0,fx1){
   h <- x1-x0
   fder <- (fx1-fx0)/h
   return(fder)
}
#4a) Progresiva    
dif_numh(6.76,6.41,5.1989,4.0951)
dif_numh(7.11,6.76,6.1303,5.1989)
dif_numh(7.46,7.11,6.7893,6.1303)
dif_numh(7.81,7.46,7.1079,6.7893)
dif_numh(8.16,7.81,7.0591,7.1079)
dif_numh(8.51,8.16,6.6598,7.0591)
#NA

#4b) Regresiva
#NA
dif_numh(6.41,6.76,4.0951,5.1989)
dif_numh(6.41,6.76,4.0951,5.1989)
dif_numh(6.76,7.11,5.1989,6.1303)
dif_numh(7.11,7.46,6.1303,6.7893)
dif_numh(7.46,7.81,6.7893,7.1079)
dif_numh(7.81,8.16,7.1079,7.0591)
dif_numh(8.16,8.51,7.0591,6.6598)
  
p3_ext <- function(xe,x0,fxe,fx0,fx1){
  h <- abs(xe-x0)
  if(xe<x0){
    fder <- (1/(2*h))*(-3*fxe+4*fx0-fx1)
    return(fder)
  }
  else{
    fder <- (1/(2*h))*(3*fxe-4*fx0+fx1)
    return(fder)
  }
}
p3_medio <- function(x0,xm,fx0,fx1){
  h <- xm-x0
  fder <- (1/(2*h))*(-1*fx0+fx1)
  return(fder)
}

#4c)  
p3_ext(6.41,6.76,4.0951,5.1989,6.1303) 
p3_medio(6.41,6.76,4.0951,6.1303)  
p3_medio(6.76,7.11,5.1989,6.7893)
p3_medio(7.11,7.46,6.1303,7.1079)
p3_medio(7.46,7.81,6.7893,7.0591)
p3_medio(7.81,8.16,7.1079,6.6598)
p3_ext(8.51,8.16,6.6598,7.0591,7.1079) 

p5_ext <- function(xe,x0,fxe,fx0,fx1,fx2,fx3){
  h <- abs(xe-x0)
  if(xe<x0){
    fder <- (1/(12*h))*(-25*fxe+48*fx0-36*fx1+16*fx2-3*fx3) 
    return(fder)
  }
  else{
    fder <- (1/(12*h))*(25*fxe-48*fx0+36*fx1-16*fx2+3*fx3) 
    return(fder)
  }
}  
p5_medio <- function(xm,x1,fx0,fx1,fx2,fx3){
  h <- xm-x1
  fder <- (1/(12*h))*(fx0-8*fx1+8*fx2-fx3)
  return(fder)
}

#4d)
p5_ext(6.41,6.76,4.0951,5.1989,6.1303,6.7893,7.1079)
p5_ext(6.76,7.11,5.1989,6.1303,6.7893,7.1079,7.0591)
p5_ext(7.11,7.46,6.1303,6.7893,7.1079,7.0591,6.6598)
#no se puede calcular ni con valores hacia arriba ni hacia abajo
p5_ext(8.51,8.16,6.6598,7.0591,7.1079,6.7893,6.1303)
p5_ext(8.16,7.81,7.0591,7.1079,6.7893,6.1303,5.1989)
p5_ext(7.81,7.46,7.1079,6.7893,6.1303,5.1989,4.0951)
-------------------------------------------------------------------------------------
#Punto5
der_segunda <- function(x0,xm,fx0,fxm,fx1){
  h <- xm-x0
  fderseg <- (1/(h^2))*(fx0-2*fxm+fx1)
  return(fderseg)
}
der_segunda(3.51,4.11,1.4308,0.3549,0.0994) #con h=0.6

der_segunda(2.91,4.11,2.8249,0.3549,0.8591) #con h=1.2
 
