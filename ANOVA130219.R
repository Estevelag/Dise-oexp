AF=c(243,251,275,291,347,206,210,226,249,255,241,258,270,293,328)
T=c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3)
T=factor(T)
mod2=lm(AF~T)
mod2
anova(mod2)
#Aquí se carga el archivo de excel "Calcio"
FactorCalcio=factor(Calcio$Calcio)
Y=Calcio$Y
mod3=lm(Y~FactorCalcio)
mod3
anova(mod3)
plot(mod3)
shapiro.test(mod3$residuals)
plot(mod3$residuals)
#Aquí se carga el paquete lmtest
library("lmtest", lib.loc="~/R/win-library/3.3")
dwtest(mod3$residuals)
dwtest(mod3)
bartlett.test(mod3$residuals,FactorCalcio)
Yt=1/Y
mod4=lm(Yt~FactorCalcio)
plot(mod4)
bartlett.test(mod4$residuals,FactorCalcio)
anova(mod4)
