#defining parameters
id<-hunger_games$id
name<-hunger_games$name
has_name<-hunger_games$has_name
female<-hunger_games$female
age<-hunger_games$age
volunteer<-hunger_games$volunteer
surv_day1<-hunger_games$surv_day1

#logistic regression with logit link to regress surv_day1 pn female, age, and volunteer
log_reg<-glm(surv_day1~female+age+volunteer,family="binomial",data=hunger_games)
summary(log_reg)

full<-log_reg
notfull<-glm(surv_day1~female+age,family="binomial",data=hunger_games)
library(lmtest)
lrtest(full,notfull)

b0=-6.3837
b1=0.8233
b2=0.3450
b3=2.6211

#what is katniss probabilty of survival based on model?
#Will use point estimation

femalek<-1
agek<-16
p=exp(1)^(b0+b1*femalek+b2*agek)/(1+exp(1)^(b0+b1*femalek+b2*agek))
p
p2=exp(1)^(b0+b1*femalek+b2*agek+b3*1)/(1+exp(1)^(b0+b1*femalek+b2*agek+b3*1))
p2
test=exp(1)^(b0+b1*0+b2*13)/(1+exp(1)^(b0+b1*0+b2*13))
test
