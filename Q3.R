#defining variables
y<-biomed$y
TNF<-biomed$TNF
IFN<-biomed$IFN
n<-biomed$n

#logistic regression 1
bin_reg<-glm(y/n~TNF+IFN+TNF*IFN,family=binomial,weight=n)
#summary output
summary(bin_reg)
#deviance residual plot
rd1<-residuals.glm(bin_reg,"deviance")
fv1<-bin_reg$fitted.values
plot(fv1,rd1,ylim=c(-3,3),main="Deviance Residual Plot",
     xlab="Fitted Values",ylab="Residuals",pch=1)
abline(h=-1.96)
abline(h=1.96)

par(mfrow=c(2,2))
plot(IFN*TNF,y/n) #looks root fn.
plot(IFN,y/n) #looks linear
plot(TNF,y/n) #look likes root fn.

par(mfrow=c(1,1))
#logistic regression 2
bin_reg2<-glm(y/n~TNF+IFN+sqrt(TNF*IFN),family=binomial,weight=n)
#summary output
summary(bin_reg2)
#deviance residual plot
rd2<-residuals.glm(bin_reg2,"deviance")
fv2<-bin_reg2$fitted.values
plot(fv2,rd2,ylim=c(-6,6),main="Deviance Residual Plot",
     xlab="Fitted Values",ylab="Residuals",pch=1)
abline(h=-1.96)
abline(h=1.96)


#using cloglog link
c_reg<-glm(y/n~sqrt(TNF)+IFN+sqrt(TNF*IFN),family=binomial(link=cloglog),weight=n)
summary(c_reg)
#deviance residual plot
rd3<-residuals.glm(c_reg,"deviance")
fv3<-c_reg$fitted.values
plot(fv3,rd3,ylim=c(-6,6),main="Deviance Residual Plot",
     xlab="Fitted Values",ylab="Residuals",pch=1)
abline(h=-1.96)
abline(h=1.96)

#probit link
p_reg<-glm(y/n~sqrt(TNF)+IFN+sqrt(TNF*IFN),family=binomial(link=probit),weight=n)
summary(p_reg)
#deviance residual plot
rd4<-residuals.glm(p_reg,"deviance")
fv4<-p_reg$fitted.values
plot(fv4,rd4,ylim=c(-6,6),main="Deviance Residual Plot",
     xlab="Fitted Values",ylab="Residuals",pch=1)
abline(h=-1.96)
abline(h=1.96)
