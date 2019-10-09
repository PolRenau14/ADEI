
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
##  Exploració de analisis de les dades.

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

# decició conceptual:-> agrupem els cassats, no cassats(Divorciats, separats),
# altres ( viudes, i may cassats(assumint que normalment es gent jove els que mai son casats, y que viudus gent gran))


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

# decició conceptual:-> agrupem el que no es United-States.

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

##### Factorització d'aquelles variables numeriques


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



##############


## Data Quality

# per variable, contar  missing  num of errors num of outliers
# rank variables =  missing+errors.


## create variable  missing +  out + errors.

summary(df[,vars_con])


dfaux<-df
### AGE


missingData<-which(is.na(dfaux$age)); length(missingData) #no missing data

sel<-which(df$age <= 0); length(sel) # errors
if(length(sel)>0){
  dfaux[sel,"age"]<-NA
}


outers <- calcQ(dfaux$age)

outlier<-which(dfaux$age > outers$souts);length(outlier)
dfaux[outlier ,"age"]<-NA

outlier<-which(dfaux$age < outers$souti);length(outlier)
dfaux[outlier ,"age"]<-NA

par(mfrow=c(1,2))
boxplot(dfaux$age)
boxplot(df$age)


ll<-which(is.na(dfaux$age)); ll
# No hi ha NA, sembla ser que a la gent no li importa possar la seva edat.


### fnlwgt

missingData<-which(is.na(dfaux$fnlwgt)); length(missingData) #no missing data


sel<-which(dfaux$fnlwgt <= 0); length(sel) # errors
if(length(sel)>0){
  dfaux[sel,"fnlwgt"]<-NA
}

outers <- calcQ(dfaux$fnlwgt)

outlier<-which(dfaux$fnlwgt > outers$souts);length(outlier)
dfaux[outlier ,"fnlwgt"]<-NA

outlier<-which(dfaux$fnlwgt < outers$souti);length(outlier)
dfaux[outlier ,"fnlwgt"]<-NA

boxplot(dfaux$fnlwgt)

boxplot(df$fnlwgt)



ll<-which(is.na(dfaux$fnlwgt)); ll
dfaux<-df[-ll,]

### education.num
# Aquesta variable esta reflectida en education, que al hora hem reagrupat a f.education



### hr.per.week


summary(dfaux$hr.per.week)


ll<-which(is.na(dfaux$hr.per.week));ll

sel<-which(dfaux$hr.per.week <= 0); length(sel) # errors
dfaux[sel,"hr.per.week"]<-NA


outers <- calcQ(dfaux$hr.per.week)

outlier<-which(dfaux$hr.per.week > outers$souts);length(outlier) #no outliers superiors critics
dfaux[outlier,"hr.per.week"]<-NA

outlier<- which(dfaux$hr.per.week < outers$souti);length(outlier) # no outliers inferiors critics.
dfaux[outlier,"hr.per.week"]<-NA

boxplot(dfaux$hr.per.week)
boxplot(df$hr.per.week)

ll<-which(is.na(dfaux$hr.per.week)); ll
dfaux<-df[-ll,]

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
