#defining variable
cholesterol<-factor(as.factor(fram$cholesterol),
                    levels=c(1,2,3,4),labels=c("<200","200-219","220-259","260+"))
bloodpressure<-factor(as.factor(fram$bloodpressure),
                      levels=c(1,2,3,4),labels=c("<127","127-146","147-166","167+"))
count<-fram$count
#checking the contingecy table               
xtabs(count~cholesterol+bloodpressure)
#appears counts are effected more by bloodpressure
plot(count~cholesterol+bloodpressure)

#fitting main effects model
main_eff<-glm(count~cholesterol+bloodpressure,family=poisson)
summary(main_eff)
#testing is model is good fit (ie. no interaction)
1-pchisq(main_eff$deviance,main_eff$df.residual)

#mosaic plot
mosaicplot(xtabs(count~cholesterol+bloodpressure),main="Mosaic Plot of Bloodpressure
           by Cholesterol")

#deviance residuals plot
attach(fram)
fram$fitted<-fitted.values(main_eff)
fram$dres<-residuals.glm(main_eff,type="deviance")
plot(fram$fitted,fram$dres,ylim=c(-3,3),main="Deviance Residual Plot",
     xlab="Fitted Values",ylab="Residuals",pch=1)
abline(h=1.96)
abline(h=-1.96)


cholesterol2<-relevel(cholesterol,ref=2)
bloodpressure2<-relevel(bloodpressure,ref=2)
main_newref<-glm(count~cholesterol2+bloodpressure2,family=poisson)
summary(main_newref)
fram

#fitting interation model
int_mod<-glm(count~cholesterol+bloodpressure+cholesterol*bloodpressure,
             family=poisson)
summary(int_mod)



