########
#FUNC'S# 
########


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

###########
#LIBRARIES#
###########

library(quantmod)
library(ggplot2)
library(dplyr)
library(changepoint)
library(quadprog)

###############
#DATA DOWNLOAD#
###############

ticks=c('^GSPC','AMD','C','SCI','SCTY','AAPL')
tic.names=c('sp500','amd','sci','scty','apple')
getSymbols(ticks, from='2010-01-01', src = "yahoo")

############
#Weights####
############
wt=rep(x = 0,times = length(ticks))






