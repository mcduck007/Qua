#playing with a little chaos
#
#install.packages('tsDyn','tseriesChaos')

library(quantmod)
library(tsDyn)
library(tseriesChaos)


#Norm Func's
nrmw.stk=function(x,y) {
tmp1=to.weekly(x)
names(tmp1)=c("op", "hi", "lo", "cl", "vo", "ad")
tmp2=last(tmp1,y)
tmp1=tmp2$cl/max(tmp2$hi)
return(tmp1)
}
nrmw.com=function(x,y){
tmp1=to.weekly(x)
names(tmp1)=c("op", "hi", "lo", "cl")
tmp2=last(tmp1,y)
tmp1=tmp2$cl/max(tmp2$hi)
return(tmp1)
}

#Get Some Data
getSymbols("XAU/USD", src="oanda")
gldnw=nrmw.com(XAUUSD,0)
getSymbols('GOLDPMGBD228NLBM',src='FRED')
getSymbols('DJIA',src='FRED')

#plots
autopairs(gldnw,lag = 1,type = 'regression')
acf(gldnw)
mutual(gldnw)
recurr(series = gldnw,m = 3,d = 1,levels=c(0,0.2,1))
recurr(series = gldnw,m = 3,d = 1,levels=c(0,0.4,1))
lag.plot(gldnw,lags=3,layout = c(1,3))

#Delta Test
delta.test(gldnw)

#Models and forecasting

#linear 2 parameter
modl.2ar = linear(gldnw,m=2)
#linear 3 parameter
modl.3ar = linear(gldnw,m=3)

#Self Threshold, autoregressive (setar)
modl.setar=setar(gldnw,m = 2,thDelay = 1)

#Logistic Smooth Transition AutoRegressive model
modl.lstar=lstar(gldnw,m = 2,thDelay = 1)

#Neural Network nonlinear autoregressive model
modl.nnetts=nnetTs(gldnw,m = 2,size = 3)

#Additive nonlinear autoregressive model
modl.aar=aar(gldnw,m = 2)

#create list of models for comparison
mod=list()

#Index 1
mod[["Linear 2args"]] = modl.2ar
#Index 2
mod[["Linear 3args"]] = modl.3ar
#Index 3
mod[["Self Thr Auto"]] = modl.setar
#Index 4
mod[["Lgst Smth Auto"]] = modl.lstar
#Index 5
mod[["Neural Net"]] = modl.nnetts
#Index 6
mod[["Addtv NL Auto"]] = modl.aar
mod.lst=c("Linear 2args", "Linear 3args","Self Thr Auto", "Lgst Smth Auto", "Neural Net", "Addtv NL Auto")

# Compare using AIC and MAPE
aic=sapply(mod,AIC)
bic=sapply(mod,BIC)
mape=sapply(mod,MAPE)

#Table of Indicators
mod.tabl = c(mod.lst,aic,bic,mape)

# find the best model based on the indicators
#AIC indicator
max(sapply(mod,AIC))
#index((sapply(mod,AIC)))
#mod[index((sapply(mod,AIC)))]

#BIC indicator
max(sapply(mod,BIC))
#index((sapply(mod,BIC)))
#mod[index((sapply(mod,BIC)))]

#MAPE
min(sapply(mod,MAPE))
#index(min(sapply(mod,MAPE)))
#mod[index((sapply(mod,MAPE)))]




