#codigo r biomat ##557 obs
library(data.table)
datarb = fread('datarb.csv')
area= fread('dathoj.csv')
pm2.5=datarb$`Material particulado total PM 2.5`
pm10=datarb$`Peso PM 10 (g)`
pm10 <- as.numeric(as.character(pm10))
pm2.5 <-as.numeric(as.character(pm2.5))
Nombres1=datarb$`Nombre común`
Numero1=datarb$`Número del Individuo`
Nombres2=area$`Nombre comun`
Numero2=area$Numero
areas=(area$`Suma individuos(cm2)`)/10000
MPA2.5=c()
MPA10=c()
Nombre10=c()
Nombre2.5=c()
  for(j in 1:length(Nombres2)){
    for(k in 1:length(Nombres1)){
      if ((toupper(Nombres2[j])==toupper(Nombres1[k]))&(Numero2[j]==Numero1[k])){
       if(as.character(class(pm2.5[k]))=='numeric'){
         a<-pm2.5[k]/areas[j]
        MPA2.5[length(MPA2.5)+1]<-a
        Nombre2.5[length(Nombre2.5)+1]<-Nombres2[j]
       }
        if(as.character(class(pm10[k]))=='numeric'){
          b<-pm10[k]/areas[j]
        MPA10[length(MPA10)+1]<-b
        Nombre10[length(Nombre10)+1]<-Nombres2[j]
       }
      }
    }
  }
## ahora paso a jugar con los datos para geneerar varios datos para cada especie
tratamiento1=c()
b=1
for(k in 1:length(Nombre10)){
  if ((Nombre10[k]==Nombre10[k+1])){
    tratamiento1[length(tratamiento1)+1]<-b
  }
  if (Nombre10[k]!=Nombre10[k+1]){
    tratamiento1[length(tratamiento1)+1]<-b
    b<-b+1
  }
}
tratamiento2=c()
c=1
for(k in 1:length(Nombre2.5)){
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
mod1=lm(MPA10~factor(tratamiento1))
shapiro.test(mod1$residuals)
dwtest(mod1)
bartlett.test(residuals(mod1))
plot(mod1)
anova(mod1)
mod2=lm(MPA2.5~factor(tratamiento2))
shapiro.test(mod2$residuals)
bartlett.test(residuals(mod2))
dwtest(mod2)
plot(mod2)
anova(mod2)
##librerias con dwtest agricolae lmtest zoo
oneway.test(MPA10~tratamiento1,data=MPA10, var.equal=FALSE)