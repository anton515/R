count<-epi.datn$COUNT #count of epileptic seizures per 2-week (during a 8 weeks period)
age<-epi.datn$AGE
trt<-as.factor(epi.datn$TMT) #1=has treatment, 0=control
period<-as.factor(epi.datn$PERIOD) #two-week seizure counts
ID<-epi.datn$ID
library(gee)
#(a)
GEEind<-gee(count~trt,family=poisson,id=ID,corstr="independence")
summary(GEEind)

#(b)
GEEexc<-gee(count~trt,family=poisson,id=ID,corstr="exchangeable")
summary(GEEexc)

#(c) without baseline patient Y0=151
period2<-as.factor(epi.datn2$PERIOD)
ID2<-epi.datn2$ID
trt2<-epi.datn2$TMT
count2<-epi.datn2$COUNT
GEEind2<-gee(count2~trt2,family=poisson,id=ID2,corstr="exchangeable")
summary(GEEind2)


