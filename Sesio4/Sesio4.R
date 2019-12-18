
# Dataset A
plot(anscombe$XA,anscombe$YA,main="Data A",pch=19)
text(anscombe$XA,anscombe$YA,label=rownames(anscombe))

(m1<- lm(YA~XA,data=anscombe))
summary(m1)

m1$fitted.values
fitted(m1)
resid(m1)
cbind(anscombe$XA,fitted(m1),anscombe$YA,resid(m1))
plot(anscombe$XA,anscombe$YA,main="Data A",pch=19)
text(anscombe$XA,anscombe$YA,label=rownames(anscombe),adj=-0.5) # adj per moure el text.

lines(anscombe$XA,m1$fitted.values, lty=2,lwd=2)
#la X, la dada predita, valor real, i residu

#residual analysis

par(mfrow=c(2,2))
plot(m1, id.n = 0)
par(mfrow=c(1,1))




# Dataset B
plot(anscombe$XB,anscombe$YB,main="Data B",pch=19,col="red")
text(anscombe$XB,anscombe$YB,label=rownames(anscombe))

(m2<- lm(YB~XB,data=anscombe))
summary(m2)

m2$fitted.values
fitted(m2)
resid(m2)
cbind(anscombe$XB,fitted(m2),anscombe$YB,resid(m2))
plot(anscombe$XB,anscombe$YB,main="Data B",pch=19,col="red")
text(anscombe$XB,anscombe$YB,label=rownames(anscombe),adj=-0.5,col="red") # adj per moure el text.

lines(anscombe$XB,m2$fitted.values, lty=2,lwd=2,col="red")
#la X, la dada predita, valor real, i residu

#residual analysis




par(mfrow=c(2,2))
plot(m2, id.n = 0)
par(mfrow=c(1,1))
#Solution
m2a<- lm(YB~XB+ I(XB^2),data = anscombe)
#I(jd) Variable transitoria per aquesta comanda
summary(m2a)






# Dataset c
(m3<- lm(YC~XC,data=anscombe))
summary(m3)

m3$fitted.values
cbind(anscombe$XC,fitted(m3),anscombe$YC,resid(m3))
plot(anscombe$XC,anscombe$YC,main="Data C",pch=19,col="darkgreen")
text(anscombe$XC,anscombe$YC,label=rownames(anscombe),adj=-0.5,col="darkgreen") # adj per moure el text.

lines(anscombe$XB,m3$fitted.values, lty=2,lwd=2,col="darkgreen")
#la X, la dada predita, valor real, i residu

#residual analysis

par(mfrow=c(2,2))
plot(m3, id.n = 0)
par(mfrow=c(1,1))
library(car)
Boxplot(resid(m3))

hatvalues(m3) #laverage of an observation ( factor d'anglatge)
# quan un hatvalue es 2 o 3 cops la mitjana -> lluny del centre de gravetat de les xs.
hatMean<-mean(hatvalues(m3))
cooks.distance(m3)
#agafem el valor mes extrem. el 3 => esta molt elevat en relacio a la resta, per tant 
# podem decidir que es un outlier.

#Solucio
# si no tenim mes variables explicatives l'eliminem

m3a<-lm(YC~XC, data= anscombe[-3,])
summary(m3a)

#individus influents no poden haveri en un model robust.


plot(anscombe$XC,anscombe$YC,main="Data B",pch=19,col="darkgreen")
text(anscombe$XC,anscombe$YC,label=rownames(anscombe),adj=-0.5,col="darkgreen") # adj per moure el text.

lines(anscombe$XB,m3$fitted.values, lty=2,lwd=2,col="darkgreen")
lines(anscombe$XB[-3],m3a$fitted.values, lty=2,lwd=2,col="magenta")




# Dataset D
(m4<- lm(YD~XD,data=anscombe))
summary(m4)

m4$fitted.values
cbind(anscombe$XD,fitted(m4),anscombe$YD,resid(m4))
plot(anscombe$XD,anscombe$YD,main="Data D",pch=19,col="blue")
text(anscombe$XD,anscombe$YD,label=rownames(anscombe),adj=-0.5,col="blue") # adj per moure el text.

lines(anscombe$XD,m4$fitted.values, lty=2,lwd=2,col="blue")
#la X, la dada predita, valor real, i residu

hatvalues(m4) #observació 8 es salta les cotes.

cooks.distance(m4)
#la obs 8 es infinta, massa influent.

#Solution eliminem el 8 , i vol dir que la X no te a veure amb la prediccio de la Y.


