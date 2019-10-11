#df = read.table
df<-read.table("adult.data",header=F, sep=",", 
               fill=FALSE, strip.white=TRUE,na.string="?")
dim(df)
names(df)

#aixó es pot fer en el read table, fent col.names = c("age",...,"salari")
names(df)<- c("age","type.employer","fnlwgt","education","education.num","marital",
              "ocupation","relationship","race","sex","capital.gain","capital.loss",
              "hours-per-week", "native-country","salari")

summary(df)
#Les dades que tenim amb NA's vol dir que no hi ha dada d'aquella columna.

###Creem la mostra.

set.seed(14121997)
sam<-sample(1:nrow(df),5000)
#per veure la sintaxis ?nom_comanda a la consola

sam<- sort(sam)

attributes(df)
#o fer-ho amb
str(df)

#seleccionem el sample
df<-df[sam,]

save(list="df",file="mostra.RData")


