#diseñocuadradolatino
trt<-c("A","B","C","D")
disexp<-design.lsd(trt,random=TRUE)
book=edit(disexp$book) #Hasta aqui correr, ingreso los datos y luego lo de abajo
row=rep(book$row,2)
col=rep(book$col,2)
trt=rep(book$trt,2)
Y=c(6.12,5.5,5.03,4.9,4.6,5.15,5.13,5.81,4.99,5.17,4.9,4.21,5.53,4.75,5.8,4.41,4.82,5.73)
mod=lm(Y~row+col+trt)
anova(mod)
F=qf(0.9,2,10)#DF trataiento,error
OUT=LSD.test(mod,"trt",group=TRUE)
bar.err(OUT$means)
