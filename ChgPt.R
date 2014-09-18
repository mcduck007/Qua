####Changepoint Analysis
# install.packages('changepoint')
library(changepoint)
# install quantmod 
# install.packages('quantmod')
library(quantmod)

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

#Change point for gld using 
#need to fix the date formatting somehow
#MeanVar and PELT
chg.gld=cpt.meanvar(head
                    (as.ts(gldnw,
                           start=head(index(gldnw),1),
                           end=tail(index(gldnw),1)),
                     length(gldnw)),
                    method='PELT')
plot(chg.gld)




#get changepoints index values
cpts(chg.gld)
#extract dates from the index values
na.omit(index(gldnw)[cpts(chg.gld)])

#test for FRED's data
getSymbols('GOLDPMGBD228NLBM',src='FRED')
getSymbols('DJIA',src='FRED')


# this isnt working (err: missing values, most likely due to uneven time periods)
chg.gld2=cpt.meanvar(head(
  na.exclude(as.ts(
    to.yearly(GOLDPMGBD228NLBM),
    start=head(index(to.yearly(GOLDPMGBD228NLBM)),1),
    end=tail(index(to.yearly(GOLDPMGBD228NLBM)),1))),
  length(na.exclude(to.yearly(GOLDPMGBD228NLBM)))),
  method='PELT')

plot(chg.gld2)
