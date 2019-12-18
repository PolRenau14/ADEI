
install.packages(c("car", "rgl"))
install.packages("cran")
library(car)
library(rgl)
library(effects)

load("mostra2.RData")
summary(df)
install.packages(c("ROCR"))
library(ROCR)
install.packages(("FactoMineR"))
library(FactoMineR)
names(df)
set.seed(14121997)
ll<-sample(1:nrow(df),nrow(df)*0.75)
ll<-order(ll)
dfwork<-df[ll,]
dftest<-df[-ll,]
#Modelling
## Using only covariates as explanatory vars
names(df)
vars_exp<-c("age","education.num","capital.gain","capital.loss","hr.per.week")
#In general
catdes(dfwork[,c("y.bin",vars_exp)],1)
#Veiem quines son rellevants, en aquest cas no es necesari ja que no tenim moltes variables
m1<-glm(y.bin~.,family =binomial,data=dfwork[,c("y.bin",vars_exp)] )
summary(m1)
#Els pvalor de contrast són molt inferiors al 5% ens les quedem.

#good News
Anova(m1,test="LR") # afegim el LR (CHIsq) ja que es un model generalitzat
#totes es variables tenen uns efectes nets significatius
m2<-step(m1, k = log(nrow(dfwork))) # PEr el bBIC utilitzem la K, usant el log
#Veiem que totes són rellavants
#comprobar si hi ha desajustos i coses aixi.
marginalModelPlots(m1)
#Age: El que fem amb el model te una certa discrepància
#pero veiem que la majoria s'adapten be. l'unic el age.
m2 <- glm(y.bin~poly(age,2)+education.num+capital.gain+capital.loss+hr.per.week,family=binomial,data=dfwork)
summary(m2)
#veiem que es significatiu
#Deviance test
anova(m1,m2,test="Chisq")
#hipotesis nula els dos models són equivalents.
#com el pvalor es menor a 5%, rebutjo hipotesis nula.
marginalModelPlots(m2)
#veiem que aixi tenim millor ajust.
#Plt amb respostia binaria
residualPlots(m2)
#hi ha un cert desajust ja que el smoother no es pla el de Linear Predictor
#No es preocupant
#En cas de que hi hagi error,  tot ens queda adalt o abaix, es un outlier.
#Haurem de controlar quina dada es i tractar-la l'eliminem.

#Adding factors
#En cas de que capital gain i capital loss fascin la gitza, cremm en ufactor i el substituim les variables numeriques per els respectius factors

#for this Script : assume my best Model ut to ths point is m2
#m2a <- glm(y.bin~poly(age,2)+education.num+cgain+capital.loss+hr.per.week,family=binomial,data=dfwork)
#summary(m2)
#m2b <- glm(y.bin~poly(age,2)+education.num+capital.gain+closs+hr.per.week,family=binomial,data=dfwork)
#m2c <- glm(y.bin~poly(age,2)+education.num+cgain+closs+hr.per.week,family=binomial,data=dfwork)
#BIC(m2,m2a,m2b,m2c)
#ens quedem amb la que tingui minim BIC

#Adding New factors
names(dfwork)
vars_edisc<-names(dfwork)[c(7:10,16:20,22)]
#no repetim age peque ja esta a nnumeriques. podem probar peró hauriem deliminar el factor age.
#El mateix amb f.educationNum

#en general use catdes to select the factors to be initally added to your previous bestmodel
catdes(dfwork[,c("y.bin",vars_edisc)],1)
#Els més rellevants són aquells que tenen un pvalor més petit
vars_edisc
m3<- glm(y.bin~(poly(age,2)+education.num+capital.gain+capital.loss+hr.per.week)+occupation+relationship+race+sex+f.type+f.marital+f.education+f.continent+f.benefici+f.hpw,family=binomial,data=dfwork)
summary(m3)
anova(m2,m3,test="Chisq")
#El m3 es mes rellevant
Anova(m3,test="LR")
#Els que tenen un pvalor superior a 0.05 no són rellavants
#Intentem Reduir el model
m4<- step(m3,k=log(nrow(dfwork)))
#Si no coincideixen eliminem la mes alta, i tornem a fer el test d'efectes nets
vif(m4)
#exemple
#Relation ship 5 graus de llibertat (6 nivells)
# valor maxim ultima columna com a maxim 3
plot(allEffects(m4))

#Detectar quins son els otuliers.
influencePlot(m4,id=list(method="noteworthy",n=5))
#Never plot(m4) no vol dir res.

#Qualitat predictiva
#cofusion table
#use your best model, once validated
mfinal<-m4

#taula de confusió
predictModelFinal<- factor(ifelse(predict(mfinal,type="response")<0.5,0,1),labels=c("Pre-<50","pre->50"))
predictModelFinal[1:10]
conf<-table(predictModelFinal,dfwork$y.bin)
#Capacitat predictiva 
perpc<-100*(sum(diag(conf))/sum(nrow(dfwork)))
print(perpc)
#Capacitat Predictiva del 85 per cent
m0<-glm(y.bin~1,family=binomial,data=dfwork) #model nul
m0<- factor(ifelse(predict(m0,type="response")<0.5,0,1),labels=c("Pre-<50"))
conf0<-table(m0,dfwork$y.bin);conf0
#Capacitat predictiva 
perpc0<-100*(conf0[1,1]/sum(nrow(dfwork)))
print(perpc0)
#Per seguretat fem predict amb el de treball i amb el test.
#Si són molt dispars el model esta sobreajustat.

## Repetir per la mostra test

#taula de confusió
predictModelFinalTest<- factor(ifelse(predict(m4,newdata=dftest,type="response")<0.5,0,1),labels=c("Pre-<50","pre->50"))
predictModelFinalTest[1:10]
confTest<-table(predictModelFinalTest,dftest$y.bin)
#Capacitat predictiva 
perpcTest<-100*(sum(diag(confTest))/sum(nrow(dftest)))
print(perpcTest)
#Capacitat Predictiva del 85 per cent

m0<-glm(y.bin~1,family=binomial,data=dfwork) #model nul
predict(m0,newdata = dftest,type="response")[1:10]
m0Test<- factor(ifelse(predict(m0,newdata=dftest,type="response")<0.5,0,1),labels=c("Pre-<50"))
conf0Test<-table(m0Test,dftest$y.bin);conf0Test
#Capacitat predictiva 
perpc0Test<-100*(conf0Test[1,1]/sum(nrow(dftest)))
print(perpc0Test)
#Per seguretat fem predict amb el de treball i amb el test.
#Si són molt dispars el model esta sobreajustat.

#Veiem que les prediccions són similars.

library(ROCR)
dadesroc<-prediction(predict(mfinal,type="response",dfwork$y.bin))
par(mfrow=c(1,2))
plot(performance(dadesroc,"err"))
plot(performance(dadesroc,"tpr","fpr"))
abline(0,1,lty=2)
# Explora entre el millor cutoff
#Relació dels falsos positius i dels falsos negatius
#si la corvba fos perfecte seria fins adalt(area igual a 1)
#Sense capacitat descriptiva en la linea (0.5)
#Menor a 0.5 per sota de la discontinua
#Determinam molts falsos positius o falsos negatius.