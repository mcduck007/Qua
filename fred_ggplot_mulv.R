#Normalizing funcs
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

#Libraries

library(quantmod)
library(ggplot2)

#get Data


# Stock Market
getSymbols("^GSPC", src = "yahoo")
getSymbols('DJIA', src = 'FRED')
getSymbols('SP500', src = 'FRED')

#Commodities

#CBOE Volatility Index: VIX Daily
getSymbols('VIXCLS',src='FRED')
# Crude Oil Prices: West Texas Intermediate (WTI) - Cushing, Oklahoma
getSymbols('DCOILWTICO',src='FRED')
#Crude Oil Prices: Brent - Europe
getSymbols('DCOILBRENTEU',src='FRED')
#CBOE Crude Oil ETF Volatility Index
getSymbols('OVXCLS',src='FRED')
#Export (Harmonized System): Corn (maize)
getSymbols('ID1005',src='FRED')
#Gold Fixing Price 3:00 P.M. (London time) in London Bullion Market, based in U.S. Dollars
getSymbols('GOLDPMGBD228NLBM',src='FRED')



# Economic Data
#Real Disposable Income
getSymbols('DSPIC96',src='FRED')
#30-Year Fixed Rate Mortgage Average in the United States©
getSymbols('MORTGAGE30US',src='FRED')
#Moving 12-Month Total Vehicle Miles Traveled
getSymbols('M12MTVUSM227NFWA',src='FRED')
#
getSymbols('OVXCLS',src='FRED')
#
getSymbols('OVXCLS',src='FRED')
#
getSymbols('OVXCLS',src='FRED')

