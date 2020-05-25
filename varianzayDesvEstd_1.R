#Varianza y Desviación Estándar
datos<-c(0,2,4,5,8,10,10,15,38)
media<-mean(datos)
Res_mean<-datos-media
pot_mean<-Res_mean^2
var<-sum(pot_mean)/9
DesvSta<-sqrt(var)

# regresion linear

mts<-c(5,15,20,25)
precio<-c(375,487,450,500)
casas<-t(rbind(mts,precio))
casas<-as.data.frame(casas)
plot(t(casas))
sum(mts*precio)
sum(precio)
sum(mts)
sum(mts^2)

((4*sum(mts*precio))-(sum(precio)*sum(mts)))/((4*sum(mts^2))-(sum(mts)^2))
cor(casas)

regresion<-lm(precio~mts,dat=casas)
summary(regresion)
precioNuevo<-361.257+5.646*35
