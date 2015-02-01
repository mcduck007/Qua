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

### 

##Number of Observations per Column

#Weekly data for 5 years, 52weeks*5=260 + 5 "a few extra", 
#Days for 7 day weeks for 5years and 5 more weeks = 1855
#Days for 5 day weeks for 5years and 5 more weeks = 1355


weeks=265
#Daily=1855



###
###


###Libraries

library(quantmod)
library(ggplot2)
library(dplyr)
library(tidyr)

####get Data


#### Stock Market

getSymbols("^GSPC", src = "yahoo")
getSymbols('DJIA', src = 'FRED')
getSymbols('SP500', src = 'FRED')

###Commodities

#CBOE Volatility Index: VIX Daily
getSymbols('VIXCLS',src='FRED')

# Crude Oil Prices: West Texas Intermediate (WTI) - Cushing, Oklahoma
getSymbols('DCOILWTICO',src='FRED')
WTI = DCOILWTICO
rm(DCOILWTICO)



#Crude Oil Prices: Brent - Europe
getSymbols('DCOILBRENTEU',src='FRED')
BRENT=DCOILBRENTEU
rm(DCOILBRENTEU)

#CBOE Crude Oil ETF Volatility Index
getSymbols('OVXCLS',src='FRED')
CrudeVix=OVXCLS
rm(OVXCLS)

#Gas Prices 
getSymbols('GASREGW',src='FRED')
Gas=GASREGW
rm(GASREGW)

#Export (Harmonized System): Corn (maize)
getSymbols('ID1005',src='FRED')
Maize=ID1005
rm(ID1005)

#Gold Fixing Price 3:00 P.M. (London time) in London Bullion Market, based in U.S. Dollars
getSymbols('GOLDPMGBD228NLBM',src='FRED')
Gold=GOLDPMGBD228NLBM
rm(GOLDPMGBD228NLBM)

#Currencies

#US Euro
getSymbols('DEXUSEU',src='FRED')
Eur=1/DEXUSEU
rm(DEXUSEU)

#US Japan
getSymbols('DEXJPUS',src='FRED')
Jpy=(DEXJPUS)
rm(DEXJPUS)

#US China
getSymbols('DEXCHUS',src='FRED')
Chn=(DEXCHUS)
rm(DEXCHUS)

#US UK
getSymbols('DEXUSUK',src='FRED')
Uk=1/DEXUSUK
rm(DEXUSUK)

#US India
getSymbols('DEXINUS',src='FRED')
Inr=(DEXINUS)
rm(DEXINUS)

#US Taiwan
getSymbols('DEXTAUS',src='FRED')
Tai=(DEXTAUS)
rm(DEXTAUS)

#US S Korea
getSymbols('DEXKOUS',src='FRED')
Skr=(DEXKOUS)
rm(DEXKOUS)

#US Singapore
getSymbols('DEXSIUS',src='FRED')
Sgp(DEXSIUS)
rm(DEXSIUS)

#US Australia
getSymbols('DEXUSAL',src='FRED')
Aud=1/DEXUSAL
rm(DEXUSAL)


# Economic Data

#Real Disposable Income
getSymbols('DSPIC96',src='FRED')
IncDisp=DSPIC96
rm(DSPIC96)

#30-Year Fixed Rate Mortgage Average in the United States©
getSymbols('MORTGAGE30US',src='FRED')
Mort30y=MORTGAGE30US
rm(MORTGAGE30US)

#Moving 12-Month Total Vehicle Miles Traveled
getSymbols('M12MTVUSM227NFWA',src='FRED')
Miles12mth=M12MTVUSM227NFWA
rm(M12MTVUSM227NFWA)

