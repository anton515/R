#Defining variables
id<-rhDNase2$id
trt<-rhDNase2$trt
fev<-rhDNase2$fev
count<-rhDNase2$count
time<-rhDNase2$time

#fitting a poisson model to the data for counts
#a
poi<-glm(count~trt+fev+offset(log(time)),family=poisson)
summary(poi)
b1=-0.253609
RR=exp(b1)
con<-c(exp(b1-1.96*0.075140),exp(b1+1.96*0.075140))
#b
dis=poi$deviance/poi$df.residual #overdisperson
adjusted95<-c(exp(b1-1.96*0.075140*sqrt(dis)),exp(b1+1.96*0.075140*sqrt(dis)))
2*(1-pnorm(abs(b1/((sqrt(dis)*0.075140)))))

#e 
#deviance residual plot for poisson model
pred<-poi$fitted.values
res<-residuals.glm(poi,"deviance")
plot(pred,res,ylim=c(-3,3),main="Deviance Residual  for Poisson Model",
     xlab="Fitted Values",ylab="Residuals",pch=1)
abline(h=1.96)
abline(h=-1.96)

#deviance residual plot for N.Bin model
pred2<-nbin$fitted.values
res2<-residuals.glm(nbin,"deviance")
plot(pred2,res2,ylim=c(-3,3),main="Deviance Residual  for N.Bin Model",
     xlab="Fitted Values",ylab="Residuals",pch=1)
abline(h=1.96)
abline(h=-1.96)

#c
#fitting a neg binomial model
nbin<-glm.nb(count~trt+fev+offset(log(time)),link=log,init.theta=1,trace=T)
summary(nbin)
RR=exp(-0.275605)
ci<-c(-0.275695-1.96*0.141964,-0.275605+1.96*0.141964) #have to exp 
2*(1-pnorm(0.275605/0.141964))
