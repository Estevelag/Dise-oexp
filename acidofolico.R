acidofolico = c(243, 251, 275, 291, 347, 206, 210, 226, 249, 255, 241, 258, 270, 293, 328) 
tratamiento= c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3)
mod1=lm(acidofolico~factor(tratamiento))
plot(mod1)
anova(mod1)
shapiro.test(mod1$residuals)
# Si el p-value dado por el shapiro test es mayor al alfa rechazo la hipotesis nula y se cumple la priemra suposición de normalidad
dwtest(mod1)
