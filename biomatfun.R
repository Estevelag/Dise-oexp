#codigo r biomat ##557 obs
library(data.table)
datarb = fread('datarb.csv')
area= fread('dathoj.csv')
pm2.5=datarb$`Peso  PM 2.5 (g)`
pm10=datarb$`Peso PM 10 (g)`
pm10 <- as.numeric(as.character(pm10))
pm2.5 <-as.numeric(as.character(pm2.5))
Nombres1=datarb$`Nombre común`
Numero1=datarb$`Número del Individuo`
Nombres2=area$`Nombre comun`#nombre del area de la especie base
Numero2=area$Numero
areas=(area$`Suma individuos(cm2)`)
MPA2.5=replicate(length(Nombres2), 0)
MPA10=replicate(length(Nombres2), 0)
Nombre10=c()
Nombre2.5=c()
for(j in 1:length(Nombres2)){
  for(k in 1:length(Nombres1)){
    if ((toupper(Nombres2[j])==toupper(Nombres1[k]))&(Numero2[j]==Numero1[k])){
      if(as.character(class(pm2.5[k]))=='numeric'){
        a<-pm2.5[k]/areas[j]
        MPA2.5[j]<-MPA2.5[j]+a
        Nombre2.5[j]<-toupper(Nombres2[j])
      }
      if(as.character(class(pm10[k]))=='numeric'){
        b<-pm10[k]/areas[j]
        MPA10[j]<-MPA10[j]+b
        Nombre10[j]<-toupper(Nombres2[j])
      }
    }
  }
}
## ahora paso a jugar con los datos para generar varios datos para cada especie
tratamiento1=replicate(length(Nombres2), 0)
b=1
for(k in 1:(length(Nombre10)-1)){
  if ((Nombre10[k]==Nombre10[k+1])){
    tratamiento1[length(tratamiento1)+1]<-b
  }
  if (Nombre10[k]!=Nombre10[k+1]){
    tratamiento1[length(tratamiento1)+1]<-b
    b<-b+1
  }
}
tratamiento2=replicate(length(Nombres2), 0)
c=1
for(k in 1:(length(Nombre2.5)-1)){
  if ((Nombre2.5[k]==Nombre2.5[k+1])){
    tratamiento2[length(tratamiento2)+1]<-c
  }
  if (Nombre2.5[k]!=Nombre2.5[k+1]){
    tratamiento2[length(tratamiento2)+1]<-c
    c<-c+1
  }
}
tratamiento1[length(tratamiento1)+1]<-tratamiento1[length(tratamiento1)]
tratamiento2[length(tratamiento2)+1]<-tratamiento2[length(tratamiento2)]
library(zoo)
library(lmtest)
library(agricolae)
mod1=lm(MPA10~factor(Nombre10))
shapiro.test(mod1$residuals)
dwtest(mod1)
bartlett.test(residuals(mod1))
plot(mod1)
anova(mod1)
mod2=lm(MPA2.5~factor(Nombre2.5))
shapiro.test(mod2$residuals)
bartlett.test(residuals(mod2))
dwtest(mod2)
plot(mod2)
anova(mod2)
##librerias con dwtest agricolae lmtest zoo
oneway.test(MPA10~tratamiento1,data=MPA10, var.equal=FALSE)
plot(MPA10, col="red", xlab="individuo", ylab="gr/cm2")
Nom10<-complete.cases(Nombre10)#acá quito los valores vacios
Nom2.5<-complete.cases(Nombre2.5)
Nombres10<-Nombre10[Nom10]
Nombres2.5<-Nombre2.5[Nom2.5]
MPAT2.5<-MPA2.5[Nom2.5]
MPAT10<-MPA10[Nom10]
mod2.5=lm(MPAT2.5~factor(Nombres2.5))
plot(mod2.5)
MP2.5<- which(MPAT2.5 < 0.00000001)#valores donde el material particulado por area es 0
MP10<-which(MPAT10 < 0.00000001)#valores donde el material particulado por area es 0
MPAT10[MP10]<-0.00000000001
MPAT2.5[MP2.5]<-0.00000000001
mod2.5=lm(MPAT2.5~factor(Nombres2.5))#sin NANS y sin 0
mod10<-lm(MPAT10~factor(Nombres10))#sin NANS y sin 0
boxplot(MPAT10~factor(Nombres10), xlab = "Especie",col = c("green"),
        ylab = "PM10(g/cm2)", main = "Material particulado 10 capturado por area")
boxplot(MPAT2.5~factor(Nombres2.5), xlab = "Especie",col = c("green"),
        ylab = "PM2.5(g/cm2)", main = "Material particulado 2.5 capturado por area")
        