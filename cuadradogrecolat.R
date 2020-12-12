#cuadradogrecolatino
lat<-c("A","B","C","D","E")
grg<-c("a","b","c","d")
disexp<-design.graeco(lat,grg)
book=edit(disexp$book) #Hasta aqui correr, ingreso los datos y luego lo de abajo
mod=lm(Y~trt+grg+lat,data=book)
anova(mod)
OUT=LSD.test(mod,"trt",group=TRUE)
bar.err(OUT$means)

#disñeincompleto=design.bib(trt,k) k es los tratamientos por bloque
#4 tratainetos, 4 bloques y 3 tratamientos por bloque
#Para hacer replicas se puede: duplicar el sketch a mano
#o row=rep(book$row,2) y esto es para evaluar otra variable mas
#hacer rowrep pero con col y trt también
#por ultimo hacer el mod=lm(hsggdgshsi)