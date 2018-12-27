setwd('/Users/abhishek/Downloads/Proactive Attrition Management-Logistic Regression Case Study')
data<-read.csv("Proactive Attrition Management-Logistic Regression Case Study.csv")

str(data)
dim(data)

#71047 rows   
#77 predictors 

class(data$CHURN)
#Integer values having 0 and 1
View(data$CHURN)
head(data)

#Outlier treatment of numerical variables

vars<- c("CHURN","REVENUE","MOU","RECCHRGE","DIRECTAS","OVERAGE","ROAM", "CHANGEM", "CHANGER", "DROPVCE",
         "BLCKVCE","UNANSVCE","CUSTCARE","THREEWAY","MOUREC","OUTCALLS","INCALLS","PEAKVCE","OPEAKVCE",
         "DROPBLK","CALLFWDV","CALLWAIT","MONTHS","UNIQSUBS","ACTVSUBS","PHONES","MODELS",
         "EQPDAYS","AGE1","AGE2","SETPRC","SETPRCM","INCOME","INCMISS","RETCALL","RETACCPT","REFER","CREDITAD")

Data_set_N<- data[vars]
mystat<- function(x){
  nmiss<- sum(is.na(x))
  a<- x[!is.na(x)]
  min<-min(a)
  m<-mean(a)
  s<- sd(a)
  max<- max(a)
  p1<-quantile(a,0.01)
  p5<-quantile(a,0.05)
  p10<-quantile(a,0.1)
  q1<-quantile(a,0.25)
  q2<-quantile(a,0.5)
  q3<-quantile(a,0.75)
  p90<-quantile(a,0.9)
  p95<-quantile(a,0.95)
  p99<-quantile(a,0.99)
  IQR<-q3-q1
  UC1<- m + (3*s)
  LC1<- m - (3*s)
  UC2<- q3 + (1.5*IQR)
  LC2<- q1 - (1.5*IQR)
  return(c(nmiss=nmiss,min=min,mean=m,stdev=s,max=max,p1=p1,p5=p5,p10=p10,q1=q1,q2=q2,q3=q3,p90=p90,p95=p95,p99=p99,UC1=UC1,LC1=LC1,UC2=UC2,LC2=LC2))
}
da1<- t(data.frame(apply(Data_set_N,2,mystat)))
View(da1)
#4788 missing values

otreat<- function(x){
  Q3<- quantile(x,0.95,na.rm=T)
  Q1<- quantile(x,0.05,na.rm=T)
  x[x>Q3]<- Q3
  x[x<Q1]<- Q1
  x
}
data_set1<- data.frame(apply(Data_set_N,2,otreat))
da2<- t(data.frame(apply(data_set1,2,mystat)))
View(da2)
library(Hmisc)
library(missForest)
data_set2<- data.frame(apply(data_set1,2,function(x) impute(x, mean(x, na.rm = T))))
vars2<- c("CHURN","CSA","CHILDREN","CREDITA","CREDITAA","CREDITB","CREDITC","CREDITDE","CREDITGY",
          "CREDITZ","PRIZMRUR","PRIZMUB","PRIZMTWN","REFURB","WEBCAP","TRUCK","RV","OCCPROF",
          "OCCCLER","OCCCRFT","OCCSTUD","OCCHMKR","OCCRET","OCCSELF","OWNRENT","MARRYUN","MARRYYES",
          "MARRYNO","MAILORD","MAILRES","MAILFLAG","TRAVEL","PCOWN","CREDITCD",
          "NEWCELLY","NEWCELLN","MCYCLE","RETCALL")

data_set3<- data[vars2]
mystat2<- function(x){
  nmiss<- sum(is.na(x))
}

da4<- t(data.frame(apply(data_set3,2,mystat2)))
View(da4)
#No missing values

library(sqldf)
Inc_<- sqldf("select INCOME
                    from data_set2
             where INCMISS==0")
data_set2$INCOME[data_set3$INCOME==0]<-5.777698557
data_set2$INCMISS<-NULL
Setprice_<- sqldf("select SETPRC
                    from data_set2
                  where SETPRCM==0")
data_set2$SETPRC[data_set3$SETPRC==0]<-78.85713423
data_set2$SETPRCM<-NULL

#VARIABLE REDUCTION

chisq.test(data_set3$CHURN,data_set3$CSA)#S
chisq.test(data_set3$CHURN,data_set3$CHILDREN,correct=F)#NS
chisq.test(data_set3$CHURN,data_set3$CREDITA,correct = F)#S
chisq.test(data_set3$CHURN,data_set3$CREDITAA,correct = F)#S
chisq.test(data_set3$CHURN,data_set3$CREDITB,correct = F)#S
chisq.test(data_set3$CHURN,data_set3$CREDITC,correct = F)#S
chisq.test(data_set3$CHURN,data_set3$CREDITDE,correct = F)#S
chisq.test(data_set3$CHURN,data_set3$CREDITGY,   correct = F)#NS
chisq.test(data_set3$CHURN,data_set3$CREDITZ,correct = F)#NS
chisq.test(data_set3$CHURN,data_set3$PRIZMRUR,correct = F)#S
chisq.test(data_set3$CHURN,data_set3$PRIZMUB,correct = F)#S
chisq.test(data_set3$CHURN,data_set3$PRIZMTWN,correct = F)#S
chisq.test(data_set3$CHURN,data_set3$REFURB,  correct = F)#S
chisq.test(data_set3$CHURN,data_set3$WEBCAP,correct = F)#S
chisq.test(data_set3$CHURN,data_set3$TRUCK,correct = F)#NS
chisq.test(data_set3$CHURN,data_set3$RV,correct = F)#NS
chisq.test(data_set3$CHURN,data_set3$OCCPROF,correct = F)#NS
chisq.test(data_set3$CHURN,data_set3$OCCCLER,correct = F)#NS
chisq.test(data_set3$CHURN,data_set3$OCCCRFT,correct = F)#NS
chisq.test(data_set3$CHURN,data_set3$OCCSTUD,correct = F)#NS
chisq.test(data_set3$CHURN,data_set3$OCCHMKR,correct = F)#NS
chisq.test(data_set3$CHURN,data_set3$OCCRET,correct = F)#S
chisq.test(data_set3$CHURN,data_set3$OCCSELF,correct = F)#NS
chisq.test(data_set3$CHURN,data_set3$OWNRENT,correct = F)#S
chisq.test(data_set3$CHURN,data_set3$MARRYUN,correct = F)#S
chisq.test(data_set3$CHURN,data_set3$MARRYYES,correct = F)#NS
chisq.test(data_set3$CHURN,data_set3$MARRYNO,correct = F)#S
chisq.test(data_set3$CHURN,data_set3$MAILORD,correct = F)#S
chisq.test(data_set3$CHURN,data_set3$MAILRES,correct = F)#S
chisq.test(data_set3$CHURN,data_set3$MAILFLAG,correct = F)#NS
chisq.test(data_set3$CHURN,data_set3$TRAVEL,correct = F)#NS
chisq.test(data_set3$CHURN,data_set3$PCOWN,correct = F)#NS
chisq.test(data_set3$CHURN,data_set3$CREDITCD,correct = F)#S
chisq.test(data_set3$CHURN,data_set3$NEWCELLY,correct = F)#S
chisq.test(data_set3$CHURN,data_set3$NEWCELLN,correct = F)#NS
chisq.test(data_set3$CHURN,data_set3$MCYCLE,correct = F)#NS

require(dplyr)
data_set3N<- select(data_set3,-c("MCYCLE","NEWCELLN","PCOWN","TRAVEL","MAILFLAG","MARRYYES",
                                 "OCCSELF","OCCHMKR","OCCSTUD","OCCCRFT","OCCCLER","OCCPROF","RV",
                                 "TRUCK","CHILDREN","CREDITGY","CREDITZ","RETCALL"))
data_set_f<-cbind.data.frame(data_set2,data_set3N)
dim(data_set_f)
#Got reduced to 55
names(data_set_f)
library('caTools')
library("Rmisc")
library("lattice")
library("plyr")
library('Metrics')
library('bootstrap')
library('lasso2')
library(leaps)
library(stats)
data_set_f[1]<-NULL
names(data_set_f)
set.seed(200)
train_index = sample(1:nrow(data_set_f), 0.8*nrow(data_set_f))
train = data_set_f[train_index,]
test = data_set_f[-train_index,]
data_set_final<- select(data_set_f,c("CHURN","REVENUE","MOU","RECCHRGE","CHANGEM","DROPVCE","UNANSVCE","CUSTCARE","MOUREC","OUTCALLS","INCALLS",
                                     "PEAKVCE","OPEAKVCE","MONTHS","EQPDAYS","CREDITA","CREDITAA","CREDITB","CREDITC","CREDITDE","PRIZMRUR","PRIZMUB",
                                     "PRIZMTWN","REFURB","WEBCAP","OCCRET","OWNRENT","MARRYUN","MAILORD","MAILRES","CREDITCD","NEWCELLY"))
dim(data_set_final)

