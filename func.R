#Norm funcs
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

#Download Data
# install quantmod 
#install.packages('quantmod')

library(quantmod)
getSymbols("XAU/USD", src="oanda")
getSymbols("XAG/USD", src="oanda")
getSymbols("XPT/USD", src="oanda")
getSymbols("XCU/USD", src="oanda")

getSymbols("EUR/USD",src="oanda")
getSymbols("INR/USD",src="oanda")
getSymbols("CNY/USD",src="oanda")
getSymbols("JPY/USD",src="oanda")
getSymbols("BRL/USD",src="oanda")
getSymbols("RUB/USD", src="oanda")

getSymbols("^GSPC", src = "yahoo")


gldnw=nrmw.com(XAUUSD,0)
slvnw=nrmw.com(XAGUSD,0)
pltnw=nrmw.com(XPTUSD,0)
eurnw=nrmw.com(EURUSD,0)
inrnw=nrmw.com(INRUSD,0)
cnynw=nrmw.com(CNYUSD,0)
brlnw=nrmw.com(BRLUSD,0)
rubnw=nrmw.com(RUBUSD,0)
spxnw=nrmw.stk(GSPC,length(gldnw))

leg=c("au","ag","pt","eur","inr","cny","brl","rub","sp500")



limy=c(0.45,1.1)

plot(gldnw,ylim=limy)
lines(slvnw,col=2)
lines(pltnw,col=3)
lines(eurnw,col=4)
lines(inrnw,col=5)
lines(cnynw,col=6)
lines(brlnw,col=7)
lines(rubnw,col=8)
lines(spxnw,col=9,lty=5,lwd=2)

legend("topleft",leg,text.col=c(1:9),horiz=TRUE)

####Changepoint Analysis
#install.packages('changepoint')
library(changepoint)

#Change point for gld using MeanVar and PELT
#need to fix the date formatting somehow
chg.gld=cpt.meanvar(as.ts(gldnw,start=head(index(gldnw),1),end=tail(index(gldnw),1)),method='PELT')
plot(chg.gld)

#get changepoints index values
cpts(chg.gld)
#extract dates from the index values
na.omit(index(gldnw)[cpts(chg.gld)])

