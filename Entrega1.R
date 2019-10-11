
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

requiredPackages <- c("effects","FactoMineR","car", "factoextra","ggplot2","dplyr","ggmap","ggthemes","knitr")
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

paste0("f.typ-",c("Civil","Private","SelfEm","Other"))
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
ll<-which(df$education %in% c("Assoc-acdm","Assoc-voc"))
df$f.education[ll]<-4
ll<-which(df$education == "Prof-school")
df$f.education[ll]<-5

# Define f.type as a factor and use 'nice' level names

df$f.education<-factor(df$f.education,levels=1:5,labels=paste0("f.education-",c("Non-Graduate","Some-college","University-Or-More","Assoc","Proof-school")))

summary(df$f.education)

barplot(table(df$f.education))
tapply(df$hr.per.week,df$f.education,mean)



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
# tenim 18 outliers superiors
iout[outlier]<- iout[outlier] + 1
jout[1]<- jout[1] + length(outlier)
outlier<-which(dfaux$age < outers$mouti);length(outlier)
#tenim 0 outliers inferiors.

par(mfrow=c(1,1))
boxplot(df$age)
# nomes marquem la linea utlier vertical perque es la que te outliers
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

outers <- calcQ(dfaux$fnlwgt)

outlier<-which(dfaux$fnlwgt > outers$souts);length(outlier)
ierr[outlier] <- ierr[outlier] +1
jerr[3]<- jerr[3]+length(outlier)

dfaux[outlier ,"fnlwgt"]<-NA

outlier<-which(dfaux$fnlwgt < outers$souti);length(outlier)
dfaux[outlier ,"fnlwgt"]<-NA
#no tenim outliers inferiors severs

outlier<-which(dfaux$fnlwgt > outers$mouts); length(outlier)
iout[outlier]<- iout[outlier] + 1
jout[3]<-jout[3] + length(outlier)
#tenim outliers suaus superiors 119

outlier<-which(dfaux$fnlwgt < outers$mouti); length(outlier)
#no teni outliers suaus inferiors.

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

df %>% slice (1:20) %>% select(education,education.num)
# Com veiem en aquesta taula reduida, education es una discretització de la variable education.num. Per tant podem eludir la variable education.num


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

#la majoria son 0, possem els 0 com a missing data?

#Calcul missing data
missingData<-which(is.na(dfaux$capital.gain)); length(missingData) #no missing data
#no tenim NA

#Calcul errors (que assignem com NA per a la inputation)
sel<-which(df$capital.gain < 0); length(sel) # errors
if(length(sel)>0){
  dfaux[sel,"capital.gain"]<-NA
}
sel<-which(df$capital.gain == 99999); length(sel)
ierr[sel]<-ierr[sel] +1
jerr[11]<- jerr[11]+length(sel)
if(length(sel)>0){
  dfaux[sel,"capital.gain"]<-NA
}
#Calcul several outliers (i assignar na) ho hem de calcular abans dels errors a NA o despres?????
outers <- calcQ(dfaux$capital.gain)

outlier<-which(dfaux$capital.gain > outers$souts);length(outlier)
ierr[outlier] <- ierr[outlier]+1
jerr[11]<- jerr[11] + length(outlier)
dfaux[outlier ,"capital.gain"]<-NA

outlier<-which(dfaux$age < outers$souti);length(outlier)
#no hi han outliers severs inferiors
dfaux[outlier ,"capital.gain"]<-NA

par(mfrow=c(1,2))
boxplot(df$capital.gain)
abline(h=outers$souts,col="red",lty=2)
abline(h=outers$mouts,col="orange",lty=2)
boxplot(dfaux$capital.gain)

ll<-which(is.na(dfaux$capital.gain)); ll
dfaux<-df[-ll,]

### capital.loss

#Calcul missing data
missingData<-which(is.na(dfaux$capital.loss)); length(missingData) #no missing data

#Calcul errors (que assignem com NA per a la inputation)
sel<-which(df$capital.loss < 0); length(sel) # errors
if(length(sel)>0){
  dfaux[sel,"capital.loss"]<-NA
}

#Calcul several outliers (i assignar na)
outers <- calcQ(dfaux$capital.loss)

outlier<-which(dfaux$capital.loss > outers$souts);length(outlier)
dfaux[outlier ,"capital.loss"]<-NA

outlier<-which(dfaux$age < outers$souti);length(outlier)
dfaux[outlier ,"capital.loss"]<-NA

par(mfrow=c(1,2))
boxplot(dfaux$capital.loss)
boxplot(df$capital.loss)

ll<-which(is.na(dfaux$capital.loss)); ll
dfaux<-df[-ll,]


### hr.per.week

summary(dfaux$hr.per.week)


ll<-which(is.na(dfaux$hr.per.week));ll
#no tenim na
sel<-which(dfaux$hr.per.week <= 0); length(sel) # errors
#no tenim errors
dfaux[sel,"hr.per.week"]<-NA


outers <- calcQ(dfaux$hr.per.week)

outlier<-which(dfaux$hr.per.week > outers$souts);length(outlier) #outliers superiors critics
ierr[outlier]<- ierr[outlier] +1
jerr[13]<-jerr[13]+length(outlier)
dfaux[outlier,"hr.per.week"]<-NA

outlier<- which(dfaux$hr.per.week < outers$souti);length(outlier) # outliers inferiors critics.
ierr[outlier]<- ierr[outlier]+1
jerr[13]<- jerr[13] + length(outlier)
dfaux[outlier,"hr.per.week"]<-NA


outlier<- which(dfaux$hr.per.week > outers$mouts);length(outlier)
iout[outlier]<- iout[outlier]+1
jout[13]<- jout[13]+length(outlier)

outlier<- which(dfaux$hr.per.week < outers$mouti);length(outlier)
iout[outlier]<- iout[outlier]+1
jout[13]<- jout[13]+length(outlier)


boxplot(df$hr.per.week)
abline(h= outers$souti,col="red")
abline(h= outers$souts,col="red")
abline(h= outers$mouti,col="orange",lty=2)
abline(h= outers$mouts,col="orange",lty=2)

boxplot(dfaux$hr.per.week)



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

#Dirira q no cal trobar error de les variables que acabem de factoritzar no tindran err, ni miss.
### f.type

missingData<-which(is.na(dfaux$f.type)); length(missingData)

sel<-which(df$f.type != 'f.typ-Civil' & df$f.type != 'f.typ-Private' &
             df$f.type != 'f.typ-SelfEm' & df$f.type != 'f.typ-Other'); length(sel) # errors

if(length(sel)>0){
  dfaux[sel,"f.type"]<-NA
}

### f.marital

missingData<-which(is.na(dfaux$f.marital)); length(missingData)

sel<-which(df$f.marital != 'f.marital-Married' & df$f.marital != 'f.marital-No- Married' &
             df$f.marital != 'f.marital-Never-married' & df$f.marital != 'f.marital-Widowed'); length(sel) # errors

if(length(sel)>0){
  dfaux[sel,"f.marital"]<-NA
}



### f.education

missingData<-which(is.na(dfaux$f.education)); length(missingData)

sel<-which(df$f.education != 'f.education-Non-Graduate' & df$f.education != 'f.education-Some-college' &
             df$f.education != 'f.education-University-Or-More' & df$f.education != 'f.education-Assoc' &
             df$f.education != 'f.education-Proof-school'); length(sel) # errors

if(length(sel)>0){
  dfaux[sel,"f.education"]<-NA
}


### f.benefici

missingData<-which(is.na(dfaux$f.benefici)); length(missingData)

sel<-which(df$f.benefici != 'f.benefici-Neutre' & df$f.benefici != 'f.benefici-Positiu' &
             df$f.benefici != 'f.benefici-Negatiu'); length(sel) # errors

if(length(sel)>0){
  dfaux[sel,"f.benefici"]<-NA
}



#################

mis1 <- countNA(df)
attributes(mis1)
df$mis_ind <- mis1$mis_ind
mis1$mis_col

summary(df[,vars_con])
outlierVal <- NULL
for (j in 1:6) {
  outlierVal[j]<-length(boxplot.stats(df[,vars_con[j]])$out)
}

##############


##  Profiling

library(missMDA)
dff<-df
summary(dff[,vars_con])  # Problem with capital_gain
ll<-which(dff$capital.gain==99999)
dff$capital.gain[ll]<-NA
res_num<-imputePCA(dff[,vars_con])
summary(res_num$completeObs)


par(mfrow=c(1,2))
boxplot(dff$capital.gain)
boxplot(res_num$completeObs[,"capital.gain"])
par(mfrow=c(1,1))

res_num$completeObs

#numeric target


names(dfaux)
vars<-names(dfaux)[c(13,1,3,7:10,14:19)]


condes(dfaux[,vars],1,prob=0.01)


#Factor(y.bin)
names(dfaux)
vars<-names(dfaux)[c(15,1,3,7:10,13:14,16:19)]

catdes(dfaux[,vars],1,prob=0.01)
