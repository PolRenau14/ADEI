load("mostra2.RData")

#instalar paquets necesaris i carregar les lliberies.
install.packages(c("FactoMineR","car","NbClust","factoextra"))
library(FactoMineR)
library(car)
library(NbClust)
library(factoextra)

df$f.hpw<- factor(df$f.hpw)
df$f.educationNum <- factor(df$f.educationNum)
#Aquesta es la mostra de l'anterior pràctica ( amb les variables imputades, i )
summary(df)

names(df)
vars_con<-names(df)[c(3,5,11:13,24)];vars_con
vars_dis<-names(df)[c(7,9,10,15:23)];vars_dis


#PCA Analysis -----------------------------------------------------------------
#crec que en lloc de fer aquest hem de fer el que hi ha més abaix, que te variables qualitatives sup i quanti sup
#Fem l'analisi incialment sobre totes les variables
res.pca <- PCA(df[,vars_con])

#Display dels Eigenvalues i % explicat amb cada dimensio, veiem que segons
#kaiser hauriem d'agafar les primeres tres dimensions, que expliquen un 66.16% de la inertia 
summary(res.pca,nb.dec=2,nbelements = Inf,nbind = 0)

#Plot per veure quines dim son >1 per explicar-les pel criteri de kaiser

# Per criteri de Elbow's sabem que hem d'agafar 4 Dimensions ( aquelles components que expliquen el 80 % de la variancia)
#Però decidim Quedar-nos amb 3 Dim => això li haurem de preguntar a la profe. Bueno realment
# per Elbow no se si ha de ser aquell que directament expliqui 80% primer,
#o bé el anterior que expliqui el 80%, si es així coincideix amb Kaiser.

colors<-c("Blue","orange")

#Kaiser
barplot(res.pca$eig[,1], col = colors[ifelse(res.pca$eig[,1] >= 1 , 1,2)])
abline(h=1, col = "red", lty=2)

#Elbow

barplot(res.pca$eig[,3],col= colors[ifelse(res.pca$eig[,3] < 85, 1, 2)])
abline(h=80,col="red",lty=2)

#Ara cercem els individus que mes contribueixen. Seran els que queden mes als extrems del primer plot
#Al segon plot busquem els 10 que mes contribueixen i el seu valor

#choix="ind" for the individual variables
#select = "contrib 10" per seleccionar els 10 individus que tenen una major contribucio en el plot en dos dimensions
plot(res.pca,choix="ind",label="none",col.ind="grey80") # aixó no fa res crec
#osigui crec que es un primer pas que va fer la profe, pero no ho hem de fer,
#Jo faria directament el d'abaix  el que te contrib10
#cex=0.5 per escalar els elements d'aquest plot per a que es vegin mes petits
plot(res.pca,choix="ind",select="contrib 10",cex=0.5)


#Interpretacio dels axis: 3 primers? (per kaiser)

#Dim 1
Boxplot(res.pca$ind$contrib[,1])
rang1<-order(res.pca$ind$contrib[,1],decreasing = T); rang1[1:10]
rownames(df[rang1[1:10],])
#Aquest pas no l'entenc
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
res.pca$quali.sup$coord[,1:3]
res.pca$ind$coord[,1:3]



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


dimdesc(res.pca,prob=0.01,axes=1:3)

summary(res.pca,nbind=0,nbelements = 30, ncp=3 )

#Definir el nombre de Clusters


fviz_nbclust(res.pca$ind$coord[,1:3], kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")

#Veiem que el numero òptim de clusters es 5, ja que  la mesura de cohesio  comparada  amb la separació
# es màxima
num.clusters<-5


#K-Means Classification-------------------------------------------------
#correspodencies múltiples. per poder passar aquelles variables que són factors
#les coordenades de les projeccions
# substituir cada individu per les projeccions en les dimensions.
#Kmeans només es pot fer per aquelles variables que són numeriques.
names(df)
data.kmeans <- res.pca$ind$coord[,1:3]
#afagem la projecció de cada individu en els plans definits per les 3 dimensions.
kmeans.res <- kmeans(data.kmeans,num.clusters)
kmeans.res$centers
# això ens dona els centres de les Variables hem de buscar la manera d'explicar-ho
#Podem Definir els centres dient:
# El primer cluster te el centre negatiu en totes les dimensions.
#      es a dir que serà contrari majoritariament a les componets que estan,
#      directament relacionades amb cada una de les DIMS.
# Segon: negatiu en Dim 1 i proper a 0 en Dim 2, mentre que es relatiament positiu per la dim 3

# I explicar coses així, però no estic segur de si es algo així.


fviz_cluster(kmeans.res, data = data.kmeans[,1:2])
fviz_cluster(kmeans.res, data = data.kmeans[,1:3])
fviz_cluster(kmeans.res, data = data.kmeans[,2:3])

# Podem fer la que volguem, la de abaix la he fet jo i no te el boundary per això.
colors <- rainbow(num.clusters) 
nums <- c(1:num.clusters)
#Dimensio 1 i dim 2
plot(data.kmeans[,1:2],col=colors[kmeans.res$cluster],
    main = " DIM 1 and DIM 2",
    xlab = "Dim 1", ylab= "Dim 2")
legend("topright", legend=nums, title="Num cluster", col = colors,
       pch=1, cex= 0.5)

#Dimensio 1 i dim 3
plot(data.kmeans[,c(1,3)],col=colors[kmeans.res$cluster],
     main = " DIM 1 and DIM 3",
     xlab = "Dim 1", ylab= "Dim 3")
legend("topright", legend=nums, title="Num cluster", col = colors,
       pch=1, cex= 0.5)

#Dimensio 2 i dim 3
plot(data.kmeans[,2:3],col=colors[kmeans.res$cluster],
     main = " DIM 2 and DIM 3",
     xlab = "Dim 2", ylab= "Dim 3")
legend("topright", legend=nums, title="Num cluster", col = colors,
       pch=1, cex= 0.5)




#Hierarchical Clustering------------------------------------------------

res.hcpc<-HCPC(res.pca,nb.clust=num.clusters,graph=T) 
output<-res.hcpc$desc.var; 
output$quanti # això ens dona la descipció de cada cluster Variables Quantitatives
#S'ha d'interpretar.
#Aqui he fet una interpretació però no estic segur de si es aixó o un altre cosa
# La veritat es que no tinc molta idea... xD
"#
Cluster 1:
 Tenen un capital.gain bastant per sota de la mitjana global.
 Tenen un capital.loss molt petit o practicament inexistent
  \-> Aquest grup la majoria no  tenen  capital.loss i tenen un capital gain positiu majoritariament
      però distant de la mitjana( no tenen inversions de gran Capital)
 Treballen menys hores de la mitjana, però poc distant unes 38 hores
 Aquest grup tenen una edad bastant més jove ( mitjana de 27 anys)
Cluster 2:
  L'edat d'aquest grup es bastant superior a la mitja global ( 51 anys) 
  En aquest cluster majoritariament tindrem a les persones d'edat superior.
  Igual que al primer clusten en quan al capital gain i capital loss
  Aquest grup te menys hores d'estudi que la mitjana.
  
#Cluster 3:
  Aquest cluster són dels que tenen major hores invertides en educació, i estan per sobre de la mirjana
  En quan a les hores treballades també estan en mitjana significativament per sobre de la global, però tampoc molt
  No tenen practicament capital loss, i tenen un capital gain bastant reduït respecte la mitjana
  Les edats són bastant properes a la mitjana global. (Mitjana edat segurament)
  
#Cluster 4:
  Aquest grup te un capital loss bastant significant respecte a la mitjana (1921).
  No tenen capitalgain, es a dir que són un grup que les inversions
  que tenen són perdudes.
  En quan a les hores invertides en educació, són superiors a la mitjana però no 
  molt significatiu.
  Les hores treballades properes a les 42 per setmana fins a 2 hores lluny de la mitjana

Cluster 5:
  Aquest grup tenen inversions de capital gain bastant bones, i no en tenen de negatives
  Estan en quan a anys d'estudi semblants al cluster 4.
  I l'edat es també aproximadament mitjana edat. ( molt proper a la mitjana 44)

  
#"




#Això ho hem de fer per a totes les variables??
# Aquesta part no la puc fer al meu PC

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

##############################################
#                 MCA                        #
##############################################

#oblidem els originals factors amb moltes categories
names(df)
#també afegim el sexe. quantitativa el hores per week
res.mca1<-MCA(df[,c(13,22,15:21,10)],quanti.sup=1,quali.sup = 2:3)
#hr.per setmana esta relaconat possitvament amb la primera dimensió
#nombre total de categories dels factors que són actius
#També veiem que no esta molt relacionat amb la dim 2, però també esta relacionat de manera
#negativa respecte la dim 2

res.mca1$eig

#veiem que amb 12 dimensions tenim prop d'un 80% de l'explicabilitat de les dades (79,46%)


#sortida no grafica
summary(res.mca1,nbind=0,nbelements = 30, ncp=12 )
dimdesc(res.mca1,prob=0.01, axes= 1:2)
#Perque tot i que dona 12 dimensions,  al posar ncp = 12 no va.
# Tampoc entenc perque el dimdesc només ens dona 3 dim com a màxim.



#Graphics:

#Aquesta vaina no ens funciona. haurem de veure com arreglarho
plot.MCA(res.mca1,invisible=c("ind","var","quati.sup"))
aux<-res.mca1$quali.sup$coord[2:5,1]
aux<-aux +res.mca1$quali.sup$coord[1,1]
lines(aux,col="darkgreen",lwd=2)
res.mca1$quali.sup$coord

#fviz_mca_var(res.mca1, choice = "mca.cor", 
 #            repel = TRUE, # Avoid text overlapping (slow)
  #           ggtheme = theme_minimal())


fviz_nbclust(res.mca1$ind$coord, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")
num.clusters <- 5
#obtenim que el nombre de clusters es de 5.

res.hc<-HCPC(res.mca1,nb.clust = num.clusters)
res.kmeans.mca <- kmeans(res.mca1$ind$coord,num.clusters)
fviz_cluster(res.kmeans.mca, data = res.mca1$ind$coord)

############# MCA Performed ##############
res.mca2<-MCA(df[,c(13,22,15:21,10)],quanti.sup=1,quali.sup = 2:3,ncp=12) 


