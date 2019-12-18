load("mostra2.RData")
install.packages(c("car","FactoMineR","effects","lmtest"))
library(car)
library(FactoMineR)
library(effects)
library(lmtest)

#target hr.per.week
#modelar utilitzant les variables numeriques

#Alternativa A (Bruta)
vars_con <- c("age","education.num","capital.gain","capital.loss","hr.per.week")
vars_con
m1<-lm(hr.per.week~.,data=df[,vars_con])
summary(m1)
vif(m1)
# sembla que totes les variables són rellevants.

#Hem de fer una diagnòsis

marginalModelPlots(m1)


m2<-lm(hr.per.week~ age+poly(education.num,2)+capital.gain+capital.loss,data=df[,vars_con])
summary(m2)
#Pvalor de education num al 2 es molt petit. per tant rebutjem hipotesis nula.
anova(m1,m2)
# seleccionem el model 1 ja que no es molt gran el benefici obtingut 

#hauriem de fer un factor que fos el capital gain per aquest exemple
#capital.gain :  covariate or factor?
m1a<-lm(hr.per.week~ age+education.num
        +f.benefici,data=df)
summary(m1a)
#hem de decidir amb algun argument que es millor factor o numerica.

#Criteri d'informació Bayesia BIC (millor per sets grans de dades)/AIC

BIC(m1,m1a)
#Són preferibles quells que tenen minim BIC
#Segons aquest criteri es millor la variable numerica que el factor.


#Residual analysis: just take a look
par(mfrow=c(2,2))
plot(m1,id.n =0 )
#no segueix una dist normal, veiem en els Residuals VS Fitted que possiblement hi ha alguna variable
#influent
par(mfrow=c(1,1))

#residual vs each explanatory variable
residualPlots(m1)
#Potser hariem de introduir algun terme quadratic al age.
#Amb capital gain i capital.loss tenim discrepancies...
# Sols => O bé factoritzar, o bé substituir el 0 per un valor proper a 0 (0.5) i fem una regresio logaritmica

m3<-lm(hr.per.week~poly(age,2)+education.num+capital.gain+capital.loss,data=df)
summary(m3)
anova(m1,m3)
#m3 es millor que m1
plot(allEffects(m3))

influencePlot(m3)
#Dist de Cook indica si es influent o no una variable.
#Discrepancia elevada(StudRes) => i una obs inusual (Hat) elevades.


