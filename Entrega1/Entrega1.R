
df<-read.table("adult.data",header=F, sep=",",fill=FALSE,              strip.white=TRUE,na.string="?")
dim(df)
names(df)

names(df)<-c("age", "type.employer", "fnlwgt", "education", "education.num","marital", "occupation",
             "relationship", "race","sex", "capital.gain", "capital.loss",
             "hr.per.week", "country", "y.bin")
summary(df)


set.seed(14121997)
sam<-sample(1:nrow(df),5000)
#per veure la sintaxis ?nom_comanda a la consola

sam<- sort(sam)

str(df)

#seleccionem el sample
df<-df[sam,]

save(list="df",file="mostra.RData")


# Load Required Packages: to be increased over the course

options(contrasts=c("contr.treatment","contr.treatment"))

requiredPackages <- c("effects","FactoMineR","car", "factoextra","ggplot2","dplyr","ggmap","ggthemes","knitr","missMDA")
missingPackages <- requiredPackages[!(requiredPackages %in% installed.packages()[,"Package"])]
if(length(missingPackages)) install.packages(missingPackages)

lapply(requiredPackages, require, character.only = TRUE)


# Clear objects
rm(list=ls())
# Clear plots
if(!is.null(dev.list())) dev.off()




#Some useful functions
calcQ <- function(x) {
  s.x <- summary(x)
  iqr<-s.x[5]-s.x[2]
  list(souti=s.x[2]-3*iqr, mouti=s.x[2]-1.5*iqr, min=s.x[1], q1=s.x[2], q2=s.x[3],
       q3=s.x[5], max=s.x[6], mouts=s.x[5]+1.5*iqr, souts=s.x[5]+3*iqr ) }

countNA <- function(x) {
  mis_x <- NULL
  for (j in 1:ncol(x)) {mis_x[j] <- sum(is.na(x[,j])) }
  mis_x <- as.data.frame(mis_x)
  rownames(mis_x) <- names(x)
  mis_i <- rep(0,nrow(x))
  for (j in 1:ncol(x)) {mis_i <- mis_i + as.numeric(is.na(x[,j])) }
  list(mis_col=mis_x,mis_ind=mis_i) }







# Command or Windows-like method
load("mostra.RData")
summary(df)


#### Univariate Descriptive Analysis ##
## Agruparem les variables descriptives en factors, no totes, les que creguem convienients
## I aquelles numeriques que considerem le discretitzarem.
##  ExploraciÃ³ de analisis de les dades.

# Data Preparation


names(df)
vars_con<-names(df)[c(1,3,5,11:13)];vars_con
vars_dis<-names(df)[c(2,4,6:10,14:15)];vars_dis




summary(df[,vars_con]) # Example of descriptive for numeric variables

summary(df[,vars_dis])




## Reagrupament de variables  descriptives


### type.employer

levels(df$type.employer)


barplot(table(df$type.employer))
table(df$type.employer)

# Conceptual decission : federal, local and state gov are grouped together - Private alone , Self-emp and the rest together



tapply(df$hr.per.week,df$type.employer,mean)
df$f.type<-1
ll<-which(df$type.employer == "Private");length(ll)
df$f.type[ll]<-2
ll<-which(df$type.employer == "Self-emp-inc");length(ll)
df$f.type[ll]<-3
ll<-which(df$type.employer %in% c("Self-emp-not-inc","Never-worked","Without-pay"));length(ll)
df$f.type[ll]<-4

# Define f.type as a factor and use 'nice' level names

df$f.type<-factor(df$f.type,levels=1:4,labels=paste0("f.typ-",c("Civil","Private","SelfEm","Other")))

summary(df$f.type)

barplot(table(df$f.type))




##############

#marital:

levels(df$marital)

barplot(table(df$marital))
table(df$marital)



tapply(df$hr.per.week,df$marital,mean)

df$f.marital<-1
ll<-which(df$marital %in% c ("Divorced","Separated")); length(ll)
df$f.marital[ll]<-2
ll<-which(df$marital == "Never-married"); length(ll)
df$f.marital[ll]<-3
ll<-which(df$marital == "Widowed"); length(ll)
df$f.marital[ll]<-4

df$f.marital<-factor(df$f.marital,levels=1:4,labels=paste0("f.marital-",c("Married","No- Married","Never-married","Widowed")))

summary(df$f.marital)

barplot(table(df$f.marital))



##############


##############

#education:

levels(df$education)

barplot(table(df$education))
table(df$education)


tapply(df$hr.per.week,df$education,mean)

df$f.education<-1
ll<-which(df$education == "Some-college")
df$f.education[ll]<-2
ll<-which(df$education %in% c("Doctorate","Bachelors","HS-grad","Masters"))
df$f.education[ll]<-3
ll<-which(df$education %in% c("Assoc-acdm","Assoc-voc","Prof-school"))
df$f.education[ll]<-4

# Define f.type as a factor and use 'nice' level names

df$f.education<-factor(df$f.education,levels=1:4,labels=paste0("f.education-",c("Non-Graduate","Some-college","University-Or-More","Assoc_AND_Proof-school")))

summary(df$f.education)

barplot(table(df$f.education),col=rainbow(12))
tapply(df$hr.per.week,df$f.education,mean)


#######
#country

summary(df$country)
levels(df$country)

barplot(table(df$country))
table(df$country)


summary(df$country)
df$f.continent<-"f.continent-America"
ll<-which(df$country %in% c("China","Hong","Philippines","Taiwan","Thailand","India","Iran","Japan","Vietnam"));ll
df$f.continent[ll]<-"f.continent-Asia"
ll<-which(df$country %in% c("England","France","Germany","Greece","Ireland","Italy","Portugal","Yugoslavia"));ll
df$f.continent[ll]<-"f.continent-Europa"
ll<-which(is.na(df$country)); ll
df$f.continent[ll]<-NA
summary(df$f.continent)

#########

##### FactoritzaciÃ³ d'aquelles variables numeriques


summary(df[,vars_con])

###### Farem el f.benefici (agrupem capital gain i capital loss)

# Neutre
# Positiu
# Negatiu

df$f.benefici<-1 #Neutre
ll<-which((df$capital.gain - df$capital.loss) > 0)
df$f.benefici[ll]<-2 #positiu
ll<-which((df$capital.gain-df$capital.loss) < 0)
df$f.benefici[ll]<-3


df$f.benefici<-factor(df$f.benefici,levels=1:3,labels=paste0("f.benefici-",c("Neutre","Positiu","Negatiu")))

summary(df$f.benefici)

barplot(table(df$f.benefici))

##############
#AGE
summary(df$age)

# Try 4 categories first
quantile(df$age)

df$f.age<-factor(cut(df$age,quantile(df$age),include.lowest = T))
summary(df$f.age)

# Reasonable according to target?
tapply(df$age,df$f.age,median) # OK

# Alternative breaks defined at 30,40,50
df$f.age<-factor(cut(df$age,c(17,29,39,49,90),include.lowest = T))
summary(df$f.age)
levels(df$f.age)<-paste0("f.age-",levels(df$f.age))

barplot(table(df$age),main="Original",col=rainbow(12))

barplot(table(df$f.age),main="Discret",col=rainbow(12))

#Hores per week

summary(df$hr.per.week)
df$f.hpw<-"f.hpw[50-60]"
ll<-which(df$hr.per.week <20);length(ll)
df$f.hpw[ll]<-"f.hpw[10-20]"
ll<-which(df$hr.per.week >= 20 & df$hr.per.week < 30);length(ll)
df$f.hpw[ll]<-"f.hpw[20-30]"
ll<-which(df$hr.per.week >= 30 & df$hr.per.week < 40);length(ll)
df$f.hpw[ll]<-"f.hpw[30-40]"
ll<-which(df$hr.per.week >= 40 & df$hr.per.week < 50);length(ll)
df$f.hpw[ll]<-"f.hpw[40-50]"

#Education num


tapply(df$hr.per.week,df$education.num,mean)

summary(df$education.num)
df$f.educationNum<-"f.educationnum[13-16]"
ll<-which(df$education.num <=4);length(ll)
df$f.educationNum[ll]<-"f.educationnum[1-4]"
ll<-which(df$education.num > 4 & df$education.num < 9);length(ll)
df$f.educationNum[ll]<-"f.educationnum[5-8]"
ll<-which(df$education.num >= 9 & df$education.num < 13);length(ll)
df$f.educationNum[ll]<-"f.educationnum[9-12]"

## Data Quality

# Per a cada variable, comptem:
# - Nombre de missing values (na)
# - Nombre d'errors
# - Nombre d'outliers [només per numériques]


# rank variables =  missing+errors.


## create variable  missing +  out + errors.

#### Contar els errors,missings i outliers

### aquells outliers severs els considerarem errors. Els que son outliers suaus els classifiquem com a outliers, i deixarem el seu valor, mentre que dels severs farem la seva imputacció.

iout<-rep(0,nrow(df))
jout<-rep(0,length(vars_con))

ierr<-rep(0,nrow(df))
jerr<-rep(0,ncol(df))

imiss<-rep(0,nrow(df))
jmiss<-rep(0,ncol(df))

summary(df[,vars_con])
dfaux<-df

### age

#Calcul missing data
missingData<-which(is.na(dfaux$age)); length(missingData) #no missing data

#Calcul errors (que assignem com NA per a la inputation)
sel<-which(df$age <= 0); length(sel) # errors
if(length(sel)>0){
  dfaux[sel,"age"]<-NA
}
# No tenim missin data ni errors d'edat negativa, comprobem els outliers severs.

#Calcul several outliers (i assignar na)
outers <- calcQ(dfaux$age)

outlier<-which(dfaux$age > outers$souts);length(outlier)
dfaux[outlier ,"age"]<-NA

outlier<-which(dfaux$age < outers$souti);length(outlier)
dfaux[outlier ,"age"]<-NA

# 0 outliers severs, es adir que per la variable age no tenim ni errors ni miss

outlier<-which(dfaux$age > outers$mouts);length(outlier)
# tenim 18 outliers superiors, segons la definició de outlier, no obstant considerem que en la diversitat de les dades es normal que hi hagi poca gent d'una edat avançada. Per tant no els considerarem com a outliers.

outlier<-which(dfaux$age < outers$mouti);length(outlier)
#tenim 0 outliers inferiors.

par(mfrow=c(1,1))
boxplot(df$age)
# A continuació veiem per on tallarien els outliers la mostra d'entrada.
abline(h= outers$mouts,col="red",lty=2)



### workclass

missingData<-which(is.na(dfaux$type.employer)); length(missingData)
imiss[missingData]<- imiss[missingData] +1
jmiss[2] <- jmiss[2]+ length(missingData)

#Tractarem com a error tot allo que no pertanyi al rang de valors que contemplem
sel<-which(df$type.employer != 'Private' & df$type.employer != 'Self-emp-not-inc' &
             df$type.employer != 'Self-emp-inc' & df$type.employer != 'Federal-gov' &
             df$type.employer != 'Local-gov' & df$type.employer != 'State-gov' &
             df$type.employer != 'Without-pay' & df$type.employer != 'Never-worked'); length(sel) # errors

if(length(sel)>0){
  dfaux[sel,"type.employer"]<-NA
}

#Tenim 0 errors

### fnlwgt

missingData<-which(is.na(dfaux$fnlwgt)); length(missingData) #no missing data
#no tenim missing data

sel<-which(dfaux$fnlwgt <= 0); length(sel) # errors
if(length(sel)>0){
  dfaux[sel,"fnlwgt"]<-NA
}
#no tenim errors

#amb aquesta variable no te sentit calcular els outliers perque ens es useless.
# el significat d'aquesta es el pes que te en relació a la mostra, no obstant no treballem amb pesos a la nostre pràctica

par(mfrow=c(1,2))
boxplot(dfaux$fnlwgt)
abline( h= outers$mouts, col="red", lty= 2)
abline( h= outers$souts, col="red", lty= 2)

boxplot(df$fnlwgt)
abline( h= outers$mouts, col="red", lty= 2)



## education

missingData<-which(is.na(dfaux$education)); length(missingData)
#no tenim missing data
sel<-which(df$education != 'Bachelors' & df$education != 'Some-college' &
             df$education != '11th' & df$education != 'HS-grad' &
             df$education != 'Prof-school' & df$education != 'Assoc-acdm' &
             df$education != 'Assoc-voc' & df$education != '9th' &
             df$education != '7th-8th' & df$education != '12th' &
             df$education != 'Masters' & df$education != '1st-4th' &
             df$education != '10th' & df$education != 'Doctorate' &
             df$education != '5th-6th' & df$education != 'Preschool'); length(sel) # errors

if(length(sel)>0){
  dfaux[sel,"education"]<-NA
}
#no tenim errors

### education.num

#com veiem aquesta variable sembla ser que es una discretització de la variable education, o que estan bastant lligades
summary(dfaux$education.num)

misingData<-which(is.na(dfaux$education.num));length(missingData)
# no hi ha errors
sel<- which(dfaux$education.num < 1 | dfaux$education.num > 99);length(sel)
#no hi ha errors

#com hem vist en el summary no hi ha valors que siguin extrems per tant no tenim outliers.



## marital status

missingData<-which(is.na(dfaux$marital)); length(missingData)
#no hi ha missing data
sel<-which(df$marital != 'Married-civ-spouse' & df$marital != 'Divorced' &
             df$marital != 'Never-married' & df$marital != 'Separated' &
             df$marital != 'Widowed' & df$marital != 'Married-spouse-absent' &
             df$marital != 'Married-AF-spouse'); length(sel) # errors

if(length(sel)>0){
  dfaux[sel,"marital"]<-NA
}
#no hi ha errors

# occupation

missingData<-which(is.na(dfaux$occupation)); length(missingData)
imiss[missingData]<- imiss[missingData] +1
jmiss[7]<- jmiss[7] + length(missingData)

sel<-which(df$occupation != 'Tech-support' & df$occupation != 'Craft-repair' &
             df$occupation != 'Other-service' & df$occupation != 'Sales' &
             df$occupation != 'Exec-managerial' & df$occupation != 'Prof-specialty' &
             df$occupation != 'Handlers-cleaners' & df$occupation != 'Machine-op-inspct' &
             df$occupation != 'Adm-clerical' & df$occupation != 'Farming-fishing' &
             df$occupation != 'Transport-moving' & df$occupation != 'Priv-house-serv' &
             df$occupation != 'Protective-serv' & df$occupation != 'Armed-Forces'); length(sel) # errors

if(length(sel)>0){
  dfaux[sel,"occupation"]<-NA
}
#tenim 0 errors

### relationship

missingData<-which(is.na(dfaux$relationship)); length(missingData)
#no tenim missing data

sel<-which(df$relationship != 'Wife' & df$relationship != 'Own-child' &
             df$relationship != 'Husband' & df$relationship != 'Not-in-family' &
             df$relationship != 'Other-relative' & df$relationship != 'Unmarried'); length(sel) # errors

if(length(sel)>0){
  dfaux[sel,"relationship"]<-NA
}
#no tenim errors

### race

missingData<-which(is.na(dfaux$race)); length(missingData)
# no tenim missing
sel<-which(df$race != 'White' & df$race != 'Asian-Pac-Islander' &
             df$race != 'Amer-Indian-Eskimo' & df$race != 'Other' &
             df$race != 'Black'); length(sel) # errors

if(length(sel)>0){
  dfaux[sel,"race"]<-NA
}

#tampoc hi ha error
### sex

missingData<-which(is.na(dfaux$sex)); length(missingData)
#no tenim missing

sel<-which(df$sex != 'Female' & df$sex != 'Male'); length(sel) # errors

if(length(sel)>0){
  dfaux[sel,"race"]<-NA
}
#no tenim errors

### capital.gain

#
summary(dfaux$capital.gain)

#Calcul missing data
missingData<-which(is.na(dfaux$capital.gain)); length(missingData) #no missing data
#no tenim NA

#Calcul errors (que assignem com NA per a la inputation)
sel<-which(dfaux$capital.gain < 0 | dfaux$capital.gain == 99999); length(sel) # errors
ierr[sel]<-ierr[sel] +1
jerr[11]<- jerr[11]+length(sel)

if(length(sel)>0){
  dfaux[sel,"capital.gain"]<-NA
}

aux<- sort(dfaux[dfaux$capital.gain > 0,"capital.gain"],decreasing=TRUE); aux[1:30]

#decidim per el criteri propi establir que tot capital gain superior a 28000 serà considerat outlier.
#no considerem outlier inferior, perque les dades que siguin negatives(si hi ha), hauran estat tractades com a errors. 
#ens basem en que són uns valors que depunten en relació a ala majoria d'aquesta clase.

outlimit <- 28000

outlier<-which(dfaux$capital.gain > outlimit);length(outlier)
ierr[outlier] <- ierr[outlier]+1
jerr[11]<- jerr[11] + length(outlier)
dfaux[outlier ,"capital.gain"]<-NA


par(mfrow=c(1,3))
boxplot(df$capital.gain,main="Original Data")
boxplot(dfaux$capital.gain, main= "Eliminant els outliers i errors")
boxplot(dfaux[dfaux$capital.gain>0,"capital.gain"], main= "Eliminant outliers, errors i 0")
#en el primer boxplot no veiem res al respecte, ja que la majoria de dades són 0, per tant mostrem que si treiem les que són 0 del segon boxplot, on hem posat els outliers a NA, ens queda un boxplot bastant bonic.


### capital.loss


summary(dfaux$capital.loss)

#Calcul missing data
missingData<-which(is.na(dfaux$capital.loss)); length(missingData) #no missing data

#Calcul errors (que assignem com NA per a la inputation)
sel<-which(df$capital.loss < 0 | df$capital.los == 99999); length(sel) # errors
if(length(sel)>0){
  dfaux[sel,"capital.loss"]<-NA
}
#no hi han errors

aux<- sort(dfaux[dfaux$capital.loss > 0,"capital.loss"],decreasing=TRUE); aux

#totes les dades són més o menys similars, no determinem un outlier

par(mfrow=c(1,2))
boxplot(df$capital.loss,main="dades originals")
boxplot(dfaux[dfaux$capital.loss>0,"capital.loss"],main="no 0")






### country

missingData<-which(is.na(dfaux$country)); length(missingData)
## tenim 88 missing data
imiss[missingData]<- imiss[missingData] + 1
jmiss[14]<-jmiss[14]+length(missingData)

sel<-which(df$country != 'United-States' & df$country != 'Cambodia' &
             df$country != 'England' & df$country != 'Puerto-Rico' &
             df$country != 'Canada' & df$country != 'Germany' &
             df$country != 'Outlying-US(Guam-USVI-etc)' & df$country != 'India' &
             df$country != 'Japan' & df$country != 'Greece' &
             df$country != 'South' & df$country != 'China' &
             df$country != 'Cuba' & df$country != 'Iran' &
             df$country != 'Honduras' & df$country != 'Philippines' &
             df$country != 'Italy' & df$country != 'Poland' &
             df$country != 'Jamaica' & df$country != 'Vietnam' &
             df$country != 'Mexico' & df$country != 'Portugal' &
             df$country != 'Ireland' & df$country != 'France' &
             df$country != 'Dominican-Republic' & df$country != 'Laos' &
             df$country != 'Ecuador' & df$country != 'Taiwan' &
             df$country != 'Haiti' & df$country != 'Columbia' &
             df$country != 'Hungary' & df$country != 'Guatemala' &
             df$country != 'Nicaragua' & df$country != 'Scotland' &
             df$country != 'Thailand' & df$country != 'Yugoslavia' &
             df$country != 'El-Salvador' & df$country != 'Trinadad&Tobago' &
             df$country != 'Peru' & df$country != 'Hong' &
             df$country != 'Holand-Netherlands'); length(sel) # errors

if(length(sel)>0){
  dfaux[sel,"country"]<-NA
}
# tenim 0 errors

### y.bin
missingData<-which(is.na(dfaux$y.bin)); length(missingData)
#no tenim missing Data

#####################################
 #podriem fer el mateix per les variables reagrupades, i discretitzades, no osbtant no té sentit ja que hem tractat tots els casos.


####
install.packages("corrplot")
install.packages("missMDA")
install.packages("FactoMineR")

library(corrplot)

#afegim la variable que es la suma dels errors missings i outliers al df
dfaux$i.rank<-  ierr + imiss + iout

#que entenem per calcular la mitjana de out/err/miss, sumar tots per columna i dividir entre 3(miss/err/out)?????
aux<-(countNA(dfaux)$mis_col)/3


t<- df[,vars_con]
df$i.rank <- dfaux$i.rank
t$i.rank <- df[,"i.rank"]
corMatrix<-cor(t); corMatrix

corrplot(corMatrix, type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45)


### hr.per.week
#com es un dels targets, els errors els eliminem.

summary(dfaux$hr.per.week)


ll<-which(is.na(dfaux$hr.per.week));ll
#no tenim na
sel<-which(dfaux$hr.per.week <= 0 | dfaux$hr.per.week ==99); length(sel) # errors
dfaux<-dfaux[-sel,]



#tenint en compte que la jornada labroal màxima es de 40 hores etmanals, establirem el limit a un 150% d'aquesta, es a dir 60 hores
# establim un limit inferior també, ja que considerarem que treballar menyys de 10 hores esra outlier
outlimit<- 60
outlier<-which(dfaux$hr.per.week > outlimit );length(outlier) #outliers superiors critics

dfaux<-dfaux[-outlier,]

outlimit<- 10
outlier<-which(dfaux$hr.per.week < outlimit );length(outlier) #outliers superiors critics

dfaux<-dfaux[-outlier,]

par(mfrow=c(1,2))
boxplot(df$hr.per.week)
abline(h= outers$souts,col="red",lty=2)
abline(h= 10,col="red",lty=2)

boxplot(dfaux$hr.per.week)
#############


## Imputing variables


library(FactoMineR)
library(missMDA)
# numericas
res.num<-imputePCA(dfaux[,vars_con[1:5]]) # no imputem el target
summary(res.num$completeObs)
summary(dfaux[,vars_con[1:5]])


# les variables no s'ha guardat com a factors. fem un factor i ale
dfaux$f.continent<-factor(dfaux$f.continent)


# descriptivas
res.des<-imputeMCA(dfaux[,c("occupation","relationship","race","sex","f.continent","f.marital")])
summary(res.des$completeObs)
summary(dfaux[,c("occupation","relationship","race","sex","f.continent","f.marital")])

#### substituim aquelles variables imputades a les dades.
dfaux[,vars_con[1:5]]<- res.num$completeObs
dfaux[,c("occupation","relationship","race","sex","f.continent","f.marital")]<- res.des$completeObs


##  Profiling

#'sha de fer nomes de les agrupades
#numeric target


names(dfaux)
vars<-names(dfaux)[c(13,3,5,7:12,15:24)];vars

condes(dfaux[,vars],num.var=1,prob=0.01)

#Factor(y.bin)
names(dfaux)
vars<-names(dfaux)[c(15,3,5,7:13,16:24)]

catdes(dfaux[,vars],1,prob=0.01)


#save new set of data.

df<-dfaux
#guardem sense la i.rank
save(list="df",file="mostra2.RData")
