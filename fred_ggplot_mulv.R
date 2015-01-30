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

nrmd.com=function(x,y){
  tmp1=(x)
  names(tmp1)=c("op", "hi", "lo", "cl")
  tmp2=last(tmp1,y)
  tmp1=tmp2$cl/max(tmp2$hi)
  return(tmp1)
}


#Libraries

library(quantmod)
library(ggplot2)
library(dplyr)

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

#Gas Prices 
getSymbols('GASREGW',src='FRED')


#Export (Harmonized System): Corn (maize)
getSymbols('ID1005',src='FRED')

#Gold Fixing Price 3:00 P.M. (London time) in London Bullion Market, based in U.S. Dollars
getSymbols('GOLDPMGBD228NLBM',src='FRED')


#Currencies

#US Euro
getSymbols('DEXUSEU',src='FRED')
tmp=1/DEXUSEU
rm(DEXUSEU)
names(tmp)='Eur'

#US Japan
getSymbols('DEXJPUS',src='FRED')
names(DEXJPUS)='Jpy'

#US China
getSymbols('DEXCHUS',src='FRED')
names(DEXCHUS)='Chn'

#US UK
getSymbols('DEXUSUK',src='FRED')
tmp=1/DEXUSUK
rm(DEXUSUK)
names(tmp)='Uk'

#US India
getSymbols('DEXINUS',src='FRED')
names(DEXINUS)='Inr'

#US Taiwan
getSymbols('DEXTAUS',src='FRED')
names(DEXTAUS)='Tai'

#US S Korea
getSymbols('DEXKOUS',src='FRED')
names(DEXKOUS)='Skr'

#US Singapore
getSymbols('DEXSIUS',src='FRED')
names(DEXSIUS)='Sgn'

#US Australia
getSymbols('DEXUSAL',src='FRED')
tmp=1/DEXUSAL
rm(DEXUSAL)
names(tmp)='Aus'


# Economic Data

#Real Disposable Income
getSymbols('DSPIC96',src='FRED')

#30-Year Fixed Rate Mortgage Average in the United States©
getSymbols('MORTGAGE30US',src='FRED')

#Moving 12-Month Total Vehicle Miles Traveled
getSymbols('M12MTVUSM227NFWA',src='FRED')

