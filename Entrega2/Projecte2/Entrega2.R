load("mostra2.RData")

df$f.hpw<- factor(df$f.hpw)
df$f.educationNum <- factor(df$f.educationNum)

vars_con<-names(df)[c(3,5,11:13,24)];vars_con
vars_dis<-names(df)[c(7,9,10,15:23)];vars_dis

options(contrasts=c("contr.treatment","contr.treatment"))

requiredPackages <- c("FactoMineR", "car", "factoextra", "NbClust", "knitr")
missingPackages <- requiredPackages[!(requiredPackages %in% installed.packages()[,"Package"])]
if(length(missingPackages)) install.packages(missingPackages)

lapply(requiredPackages, require, character.only = TRUE)


res.pca <- PCA(df[,vars_con])


summary(res.pca,nb.dec=2,nbelements = Inf,nbind = 0)

colors<-c("Blue", "orange")
barplot(res.pca$eig[,1], col = colors[ifelse(res.pca$eig[,1] >= 1 , 1,2)])
abline(h=1, col = "red", lty=2)

barplot(res.pca$eig[,3],col= colors[ifelse(res.pca$eig[,3] < 80, 1, 2)])
abline(h=80,col="red",lty=2)


plot(res.pca,choix="ind",select="contrib 10",cex=0.5)


#Dim 1
Boxplot(res.pca$ind$contrib[,1])
rang1<-order(res.pca$ind$contrib[,1],decreasing = T); rang1[1:10]
rownames(df[rang1[1:10],])


#Dim 2
Boxplot(res.pca$ind$contrib[,2])
rang1<-order(res.pca$ind$contrib[,2],decreasing = T); rang1[1:10]
rownames(df[rang1[1:10],])

#Dim 3
Boxplot(res.pca$ind$contrib[,3])
rang1<-order(res.pca$ind$contrib[,3],decreasing = T); rang1[1:10]
rownames(df[rang1[1:10],])

res.pca <- PCA(df[,c(1,3,5,11:13,15:23)], quanti.sup=6,quali.sup =  7:15)
names(df[,c(1,3,5,11:13,15:24)])
summary(res.pca,nb.dec=2,nbelements = Inf,nbind = 0)

barplot(res.pca$eig[,1], col = colors[ifelse(res.pca$eig[,1] >= 1 , 1,2)])
abline(h=1, col = "red", lty=2)

res.pca <- PCA(df[,c(1,3,5,11:13,15:23)], quanti.sup=6,quali.sup =  7:15)
names(df[,c(1,3,5,11:13,15:24)])
summary(res.pca,nb.dec=2,nbelements = Inf,nbind = 0)

barplot(res.pca$eig[,1], col = colors[ifelse(res.pca$eig[,1] >= 1 , 1,2)])
abline(h=1, col = "red", lty=2)



barplot(res.pca$eig[,3],col= colors[ifelse(res.pca$eig[,3] < 85, 1, 2)])
abline(h=80,col="red",lty=2)


plot(res.pca,choix="ind",select="contrib 10",cex=0.5)



#Dim 1
Boxplot(res.pca$ind$contrib[,1])
rang1<-order(res.pca$ind$contrib[,1],decreasing = T); rang1[1:10]
rownames(df[rang1[1:10],])

#Dim 2
Boxplot(res.pca$ind$contrib[,2])
rang1<-order(res.pca$ind$contrib[,2],decreasing = T); rang1[1:10]
rownames(df[rang1[1:10],])

#Dim 3
Boxplot(res.pca$ind$contrib[,3])
rang1<-order(res.pca$ind$contrib[,3],decreasing = T); rang1[1:10]
rownames(df[rang1[1:10],])




fviz_nbclust(res.pca$ind$coord[,1:3], kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")




num.clusters<-5
data.kmeans <- res.pca$ind$coord[,1:3]
kmeans.res <- kmeans(data.kmeans,num.clusters)
kmeans.res$centers



fviz_cluster(kmeans.res, data = data.kmeans[,1:2])
fviz_cluster(kmeans.res, data = data.kmeans[,1:3])
fviz_cluster(kmeans.res, data = data.kmeans[,2:3])



res.hcpc<-HCPC(res.pca,nb.clust=num.clusters,graph=T) 
output<-res.hcpc$desc.var; 
output$quanti


prop.table(table(df$f.education,df$f.hpw),1)
prop.table(table(df$f.education,df$f.hpw),2)

chisq.test(table(df$f.education,df$f.hpw))

prop.table(table(df$f.marital,df$f.hpw),1)

prop.table(table(df$f.marital,df$f.hpw),2)
chisq.test(table(df$f.marital,df$f.hpw))
res.mca1<-MCA(df[,c(13,22,15:23,10)],quanti.sup=1,quali.sup = 2:3)
res.mca1$eig
colors<-c("Blue","Orange")
barplot(res.mca1$eig[,3], col= colors[ifelse(res.mca1$eig[,3] < 81, 1, 2)])
abline(h=80,col="red",lty=2)
summary(res.mca1,nbind=0,nbelements = 5, ncp=12 )
dimdesc(res.mca1,prob=0.01)



plot(res.mca1,choix="ind",select="contrib 10",cex=0.5)



#Dim 1
Boxplot(res.mca1$ind$contrib[,1])
rang1<-order(res.mca1$ind$contrib[,1],decreasing = T); rang1[1:10]
rownames(df[rang1[1:10],])
df[rang1[1:10],c(vars_con,vars_dis)]



Boxplot(res.mca1$ind$contrib[,2])
rang2<-order(res.mca1$ind$contrib[,2],decreasing = T); rang1[1:10]
rownames(df[rang2[1:10],])
df[rang2[1:10],c(vars_con,vars_dis)]



Boxplot(res.mca1$ind$contrib[,3])
rang3<-order(res.mca1$ind$contrib[,3],decreasing = T); rang1[1:10]
rownames(df[rang3[1:10],])
df[rang3[1:10],c(vars_con,vars_dis)]


fviz_mca_var(res.mca1, choice = "mca.cor", 
             repel = TRUE, #
             ggtheme = theme_minimal())


out<-dimdesc(res.mca1)
out$`Dim 1`$quanti
out$`Dim 1`$quali
out$`Dim 2`$quanti
out$`Dim 2`$quali
out$`Dim 3`$quanti
out$`Dim 3`$quali

num.clusters <- 5
#obtenim que el nombre de clusters es de 5.

res.hc<-HCPC(res.mca1,nb.clust = num.clusters)
res.hc$desc.var$quanti

res.hcpc$desc.var$quanti