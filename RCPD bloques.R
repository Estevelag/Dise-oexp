#diseño de bloques aleatorizados
trt<-c("A","B","C")
b<-6# Numero de bloques
disexp<-design.rcbd(trt,b,random=FALSE)
book=edit(disexp$book) #Hasta aqui correr, ingreso los datos y luego lo de abajo
mod=lm(var4~trt+block,data=book)
anova(mod)
OUT=LSD.test(mod,"trt",group=TRUE)
bar.err(OUT$means)
