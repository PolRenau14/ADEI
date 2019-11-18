load("mostra2.RData")

df$f.hpw<- factor(df$f.hpw)
df$f.educationNum <- factor(df$f.educationNum)
#Aquesta es la mostra de l'anterior pràctica ( amb les variables imputades, i )
summary(df)
library(FactoMineR)
library(car)

#hourperweek discretitzation (fact) vs education

names(df)
vars_con<-names(df)[c(3,5,11:13)];vars_con
vars_dis<-names(df)[c(7,9,10,15:23)];vars_dis


#PCA Analysis -----------------------------------------------------------------
#crec que en lloc de fer aquest hem de fer el que hi ha més abaix, que te variables qualitatives sup i quanti sup
#Fem l'analisi incialment sobre totes les variables
res.pca <- PCA(df[,c(1,3,5,11:13)])

#Display dels Eigenvalues i % explicat amb cada dimensio, veiem que segons
#kaiser hauriem d'agafar les primeres tres dimensions, que expliquen un 66.16% de la inertia 
summary(res.pca,nb.dec=2,nbelements = Inf,nbind = 0)

#Plot per veure quines dim son >1 per explicar-les pel criteri de kaiser
barplot(res.pca$eig[,1])

#Ara cercem els individus que mes contribueixen. Seran els que queden mes als extrems del primer plot
#Al segon plot busquem els 10 que mes contribueixen i el seu valor

#choix="ind" for the individual variables
#select = "contrib 10" per seleccionar els 10 individus que tenen una major contribucio en el plot en dos dimensions
plot(res.pca,choix="ind",label="none",col.ind="grey80")
#cex=0.5 per escalar els elements d'aquest plot per a que es vegin mes petits
plot(res.pca,choix="ind",select="contrib 10",cex=0.5)


#Interpretacio dels axis: 3 primers? (per kaiser)

#Dim 1
Boxplot(res.pca$ind$contrib[,1])
rang1<-order(res.pca$ind$contrib[,1],decreasing = T); rang1[1:10]
rownames(df[rang1[1:10],])
ll<-which(res.pca$ind$coord[,2]<(-5));length(ll)
df[ll,c(vars_con,vars_dis)]

#Dim 2
Boxplot(res.pca$ind$contrib[,2])
rang1<-order(res.pca$ind$contrib[,2],decreasing = T); rang1[1:10]
rownames(df[rang1[1:10],])
ll<-which(res.pca$ind$coord[,2]<(-5));length(ll)
df[ll,c(vars_con,vars_dis)]

#Dim 3
Boxplot(res.pca$ind$contrib[,3])
rang1<-order(res.pca$ind$contrib[,3],decreasing = T); rang1[1:10]
rownames(df[rang1[1:10],])
ll<-which(res.pca$ind$coord[,2]<(-5));length(ll)
df[ll,c(vars_con,vars_dis)]

#Dubto en quines variables agafar com a quanti i quali suplementaries
names(df)
res.pca <- PCA(df[,c(1,3,5,11:13,15:24)], quanti.sup=6,quali.sup =  7:16)
res.pca$eig

#resolució gràfica. Hem d'agafar les 3 primeres components.
colors<- c("Blue", "Orange")
barplot(res.pca$eig[,1], col = colors[ifelse(res.pca$eig[,1] > 1, 1,2)])
legend("bottomright", legend=c("Superior o igual 1", "Inferior 1"), title="Eig Val", col = colors,
       pch=19)
abline(h= 1,col="red",lty=2)

# Per criteri de Elbow's sabem que hem d'agafar 4 Clausters. ( aquelles components que expliquen el 80 % de la variancia)

num.clusters<- 4

barplot(res.pca$eig[,3],col= colors[ifelse(res.pca$eig[,3] < 85, 1, 2)])
abline(h=80,col="red",lty=2)


#K-Means Classification-------------------------------------------------
#Kmeans només es pot fer per aquelles variables que són numeriques.
install.packages("factoextra")
library(factoextra)
names(df)
df.numeriques <- df[,c(1,3,5,11:13)]
kmeans.res <- kmeans(df.numeriques,num.clusters)
summary(kmeans.res$centers)

#Hierarchical Clustering------------------------------------------------

res.hcpc<-HCPC(res.pca,nb.clust=num.clusters,graph=T) 

#Això ho hem de fer per a totes les variables??

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
## FINS AQUI


###MCA

#oblidem els originals factors amb moltes categories
names(df)
#també afegim el sexe. quantitativa el hores per week
res.mca1<-MCA(df[,c(13,22,15:21,10)],quanti.sup=1,quali.sup = 2:3)
#hr.per setmana esta relaconat possitvament amb la primera dimensió
#nombre total de categories dels factors que són actius
#També veiem que no esta molt relacionat amb la dim 2, però també esta relacionat de manera
#negativa respecte la dim 2

#sortida no grafica
summary(res.mca1,nbind=0,nbelements = 30, ncp=2 )
dimdesc(res.mca1,prob=0.01,axes=1:2)

#Graphics:

plot.MCA(res.mca1,invisible=c("ind","var","quati.sup"))
aux<-res.mca1$quali.sup$coord[2:5,1]
aux<-aux +res.mca1$quali.sup$coord[1,1]
lines(aux,col="darkgreen",lwd=2)
res.mca1$quali.sup$coord
fviz_mca_var(res.mca1, choice = "mca.cor", 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())


mean(res.mca1$eig[,1])

res.mca2<-MCA(df[,c(13,22,15:21,10)],quanti.sup=1,quali.sup = 2:3,ncp=14)
res.hc<-HCPC(res.mca2,nb.clust = 0)


