#Trading Strategy 1
#Shamelessly Copied from:
#http://blog.fosstrading.com/search/label/Examples
library(PerformanceAnalytics)
library(quantmod)

#Get Some Data
getSymbols("^GSPC", src = "yahoo")
getSymbols('DJIA',src='FRED')
getSymbols('SP500',src='FRED')

#Buy low sell high


#Create a retuns vector based on opaning cna closing prices
ret.spy=Delt(Op(x = GSPC),Cl(x = GSPC))

# Signal Vector based on the prce changes increase => 1 else 0 and lag it by one period
sgnl.spy=lag(ifelse(Cl(GSPC)>Op(GSPC),1,0),1)

#performance matrix by multiplying returns to signal
strat.perf=ret.spy*sgnl.spy

#check the cumulative return??

#try another signal
sgnl.spy2=ifelse(((Cl(GSPC)>Op(GSPC))),1,0)*
  ifelse(lag(Cl(GSPC),1)>lag(Op(GSPC),1),1,0)*
  ifelse(lag(Cl(GSPC),2)>lag(Op(GSPC),2),1,0)

#number of trades
sum(sgnl.spy2)

#plot performance summary
charts.PerformanceSummary(ret.spy*sgnl.spy2)

#Using SMA
sma.spy.30d.hi=SMA(Hi(GSPC),30)
sma.spy.30d.lo=SMA(Lo(GSPC),30)
sgnl.spy3=lag(ifelse(Cl(GSPC)>sma.spy.30d.hi,1,0)+ifelse(Cl(GSPC)<sma.spy.30d.lo,-1,0),1)
#remove NAs
sgnl.spy3[is.na(sgnl.spy3)]=0
