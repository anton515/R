#defining variables
survived<-surg_risk$survived
trt<-surg_risk$trt
surgrisk<-as.factor(surg_risk$surgrisk)
n<-surg_risk$n

#to get beta1 to calculate deviance residual for part i
surgrisk<-relevel(surgrisk, ref=2)

#setting "low" as the reference category
surgrisk<-relevel(surgrisk, ref=1)

#logistic regression
bin_rega<-glm(survived/n~trt+surgrisk+trt*surgrisk
             ,family=binomial(link=logit),weight=n)
summary(bin_rega)

#logistic regression without interaction as no sign
bin_reg<-glm(survived/n~trt+surgrisk
              ,family=binomial(link=logit),weight=n)
summary(bin_reg)


#deviance residual
rd1<-residuals.glm(bin_reg,"deviance")
rd1

#deviance residual plot
rd1<-residuals.glm(bin_reg,"deviance")
lp1<-bin_reg$linear.predictors
fv1<-bin_reg$fitted.values
plot(fv1,rd1,ylim=c(-3,3),main="Deviance Residual Plot",
     xlab="Fitted Values",ylab="Residuals",pch=1)
abline(h=-1.96)
abline(h=1.96)

death<-n-survived
bin_reg2<-glm(death/n~trt+surgrisk,family=binomial,weight=n)
summary(bin_reg2)
