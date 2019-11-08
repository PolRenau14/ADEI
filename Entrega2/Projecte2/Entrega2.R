load("mostra2.RData")
#Aquesta es la mostra de l'anterior pràctica ( amb les variables imputades, i )
summary(df)
library(FactoMineR)
library(car)
#CA

#hourperweek discretitzation (fact) vs education

names(df)






prop.table(table(df$f.education,df$f.hpw),1) #Row proportions
prop.table(table(df$f.education,df$f.hpw),2) #Column proportions

#H0: Rows and columns are not dependent
chisq.test(table(df$f.education,df$f.hpw))
#com que dona inferior rebutjem hipotesis nula => files i columnes son dependents.


tt <- table(df[,c("f.education","f.hpw")])
res.ca1<-CA(dftt)
lines(res.ca1$row$coord[,1],res.ca1$row$coord[,2],col="blue",lwd=2)

summary(res.ca1,ncp=2)

plot.CA(res.ca1.selectCol="contrib 2",invisible="row")



###MCA

#oblidem els originals factors amb moltes categories
names(df)
#també afegim el sexe. quantitativa el hores per week
res.mca1<-MCA(df[,c(13,22,15:21,10)],quanti.sup=1,quali.sup = 2:3)
#hr.per setmana esta relaconat possitvament amb la primera dimensió
#nombre total de categories dels factors que són actius

#sortida no grafica
summary(res.mca1,nbind=0,nbelements = 30, ncp=2 )
dimdesc(res.mca1,prob=0.01,axes=1:2)

#Graphics:

plot.MCA(res.mca1,invisible=c("ind","var","quati.sup"))
aux<-res.mca1$quali.sup$coord[2:5,1]
aux<-aux +res.mca1$quali.sup$coord[1,1]
lines(aux,col="darkgreen",lwd=2)
res.mca1$quali.sup$coord


mean(res.mca1$eig[,1])

res.mca2<-MCA(df[,c(13,22,15:21,10)],quanti.sup=1,quali.sup = 2:3,ncp=14)
res.hc<-HCPC(res.mca2,nb.clust = 0)
