
install.packages(c("car", "rgl"))
install.packages("cran")
library(car)
library(rgl)
library(effects)

load("mostra2.RData")
summary(df)
#target hr.per.week
##modelling using numeric variables(covariates + factors)
names(df)
vars_con <- c("age","education.num","capital.gain","capital.loss","hr.per.week")
m1 <- lm(hr.per.week~.,data=df[,vars_con])
summary(m1)
vif(m1)

m10 <- lm(hr.per.week~age+education.num,data=df[,vars_con])
summary(m10)
vif(m10)

scatter3d(hr.per.week~age+education.num,data=df)

m11 <- lm(hr.per.week~f.age+education.num,data=df)

BIC(m10,m11)
#millor el m11

#eina visual limitada
scatterplot(hr.per.week~education.num|f.age,smooth=F,data=df)

plot(allEffects(m11))

summary(m11)


m11a <- lm(hr.per.week~education.num,data=df)
m11b <- lm(hr.per.week~f.age,data=df)
anova(m11a,m11)
anova(m11b,m11)

#metode important fa lo mateix que hem fet adalt, sense calcular models intermitjos.
Anova(m11)

m0 <- lm(hr.per.week~1,data=df)
anova(m0,m11a)
#L'efecte rut de education es necesari => ja que rebutjo hipotesis de que siguin iguals pvalor menor a 0.0003
anova(m0,m11b)
#el mateix per age.

# m11b explica el targes a traves del factor edat Y~f.A
summary(m11b)
table(predict(m11b))
table(df$f.age)
#hem de saber interpretar els models.

#model ancova amb addicions Y~A+X

#model ancova amb interaccions Y~A*X
m12<- lm(hr.per.week~ f.age*education.num,data=df)
summary(m12)

anova(m11,m12)
#son models encaixats 
# no tenim evidències per rebutjar H0.
BIC(m11,m12)
#em quedo amb el model que tene menor BIC m11

step(m12,k=log(nrow(df)))

#two way anova_ Y~A+B or Y~A*B
xtabs(.~f.age+f.marital,data=df)
m13<- lm(hr.per.week~f.age+f.marital,data=df)
summary(m13)    

Anova(m13)

m14<- lm(hr.per.week~f.age*f.marital,data=df)
summary(m14)

anova(m13,m14)


###
m15<- lm(hr.per.week~(f.age+f.marital)*f.education,data=df)
summary(m15)
m16<-step(m15,k=log(nrow(df)))
