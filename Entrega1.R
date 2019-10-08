

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
##  Exploració de analisis de les dades.

# Data Preparation


names(df)
vars_con<-names(df)[c(1,3,5,11:13)];vars_con
vars_dis<-names(df)[c(2,4,6:10,14:15)];vars_dis

summary(df[,vars_con]) # Example of descriptive for numeric variables

summary(df[,vars_dis])

#veient el summary veiem per exemple que podem ajuntar les races, es a dir white or others.


missingData<- which(is.na(df$race));length(missingData)




levels(df$type.employer)

# Graphic tool for Univ EDA and factors
barplot(table(df$type.employer))
table(df$type.employer)

# Conceptual decission : federal, local and state gov are grouped together - Private alone , Self-emp and the rest together

# Use target means

tapply(df$hr.per.week,df$type.employer,mean)
df$f.type<-1
ll<-which(df$type.employer == "Private");length(ll)
df$f.type[ll]<-2
df[ll,"f.type"]<-2 # f.type already available
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



levels(df$country)

barplot(table(df$country))
table(df$country)

# decició conceptual:-> agrupem el que no es United-States.


tapply(df$hr.per.week,df$country,mean)

df$f.country<-1
ll<-which(df$country == "United-States");length(ll)
df$f.country[ll]<-2
df[ll,"f.country"]<-2 # f.type already available

# Define f.type as a factor and use 'nice' level names

df$f.country<-factor(df$f.country,levels=1:2,labels=paste0("f.country-",c("No-United-States","United-States")))

summary(df$f.country)

barplot(table(df$f.country))

##############

#marital: 


levels(df$marital)

barplot(table(df$marital))
table(df$marital)

# decició conceptual:-> agrupem els cassats, no cassats(Divorciats, separats), 
# altres ( viudes, i may cassats(assumint que normalment es gent jove els que mai son casats, y que viudus gent gran))


tapply(df$hr.per.week,df$marital,mean)

df$f.marital<-1
ll<-which(df$marital %in% c("Married-AF-spouse","Married-civ-spouse","Married-spouse-absent"));length(ll)
df$f.marital[ll]<-2
df[ll,"f.marital"]<-2 # f.type already available
ll<-which(df$marital %in% c ("Divorced","Separated")); length(ll)
df$f.marital[ll]<-3
df[ll,"f.marital"]<-3
ll<-which(df$marital %in% c("Never-married","Widowed"))

# Define f.type as a factor and use 'nice' level names

df$f.marital<-factor(df$f.marital,levels=1:3,labels=paste0("f.marital-",c("Married","No- Married","Others")))

summary(df$f.marital)

barplot(table(df$f.marital))



##############


##############

#occupation: 

summary(df[,vars_dis])

levels(df$occupation)

barplot(table(df$occupation))
table(df$occupation)

# decició conceptual:-> agrupem el que no es United-States.


tapply(df$hr.per.week,df$occupation,mean)

df$f.country<-1
ll<-which(df$country == "United-States");length(ll)
df$f.country[ll]<-2
df[ll,"f.country"]<-2 # f.type already available

# Define f.type as a factor and use 'nice' level names

df$f.country<-factor(df$f.country,levels=1:2,labels=paste0("f.country-",c("No-United-States","United-States")))

summary(df$f.country)

barplot(table(df$f.country))



##############


## Data Quality

# per variable, contar  missing  num of errors num of outliers
# rank variables =  missing+errors.


## create variable  missing +  out + errors.







levels(df$sex)<-paste0("Sex-",levels(df$sex))
summary(df$sex)

# Univariant EDA 
100*table(df$sex)/nrow(df)
prop.table(table(df$sex))

# Graphic tools
barplot(table(df$sex),main="Gender observations",col=rainbow(2))

## Variables numeriques detecció de outliers.

summary(df$age)

# Try 4 categories first
quantile(df$age)

cut(df$age,quantile(df$age))
df$f.age<-factor(cut(df$age,quantile(df$age),include.lowest = T))
summary(df$f.age)

# Reasonable according to target?
tapply(df$age,df$f.age,median) # OK

# Alternative breaks defined at 30,40,50
df$f.age<-factor(cut(df$age,c(17,29,39,49,90),include.lowest = T))
summary(df$f.age)
levels(df$f.age)<-paste0("f.age-",levels(df$f.age))


# Numeric indicators - statistics
summary(df$age)
quantile(df$age,seq(0,1,by=0.1)) # Decils of df$age
# Desviació tipus
sd(df$age)
# Variance 
var(df$age)

# Coefficient of variation
sd(df$age)/mean(df$age)

# Graphical tools
hist(df$age,main="Age histogram",col=rainbow(12))

mm<-mean(df$age);dd<-sd(df$age);mm;dd
hist(df$age,freq=F,30,main="Age histogram",col=rainbow(12))
curve(dnorm(x,mean=mm,sd=dd),col="cyan",lwd=3,add=T)

# Outlier detection
boxplot(df$age,main="Boxplot age")
summary(df$age)
outsev<-48+3*(48-27)
outsua<-48+1.5*(48-27);outsev;outsua
abline(h=outsua,col="red",lwd=2,lty=2)

miss<-countNA(df)
attributes(miss)
miss$mis_col  # Number of missing values for each variable
summary(df)
miss$mis_ind # Number of missing values in variables for each observation
summary(miss$mis_ind)
quantile(miss$mis_ind,seq(0,1,0.1))

iout<-rep(0,nrow(df))
jout<-rep(0,length(vars_con))

ierr<-rep(0,nrow(df))
jerr<-rep(0,ncol(df))

summ<-summary(df$hr.per.week)
Boxplot(df$hr.per.week,main="Boxplot hr.per.week",col="orange")
barplot(table(df$hr.per.week))

iqr<-summ[5]-summ[2];iqr
souts<-summ[5]+3*iqr  # upper threshold
souti<-summ[2]-3*iqr# lower threshold
souti;souts

ll<-which((df$hr.per.week<souti)|(df$hr.per.week>souts));length(ll)

ll<-which((df$hr.per.week<5)|(df$hr.per.week>80));length(ll)

### Special treatment: target - Errors or Severe outliers - Remove

dff<-df[-ll,]

### End Special treatment: target - Errors or Severe outliers - Remove

# Update iout,jout,ierr,jerr
jerr[13]<-jerr[13]+length(ll)
ierr[ll]<-ierr[ll]+1

calcQ(df$hr.per.week)

Boxplot(dff$hr.per.week,main="Boxplot hr.per.week",col="orange")
abline(h=calcQ(dff$hr.per.week)$souti,lwd=2,col="red",lty=2)
abline(h=calcQ(dff$hr.per.week)$souts,lwd=2,col="red",lty=2)


library(missMDA)

summary(dff[,vars_con])  # Problem with capital_gain
ll<-which(dff$capital.gain==99999)
dff$capital.gain[ll]<-NA
res_num<-imputePCA(dff[,vars_con])
summary(res_num$completeObs)

par(mfrow=c(1,2))
boxplot(dff$capital.gain)
boxplot(res_num$completeObs[,"capital.gain"])
par(mfrow=c(1,1))





##############


## Imputation

# Eliminar els NA's.


#############


##  Profiling

#numeric target

#Factor(y)

