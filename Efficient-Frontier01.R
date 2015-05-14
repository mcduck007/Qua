#############
#COPIED FROM 
#http://blog.streeteye.com/blog/2012/01/portfolio-optimization-and-efficient-frontiers-in-r/
#############


############################
#Let’s get some data and plot a quick chart.
############################


install.packages("quantmod")
# load library
require(quantmod)
# some ETFs we might choose for a portfolio:
symbols = c('SPY','IWM','EFA','EEM','AGG','IYR','GLD')
# these are, respectiviely
symbol.names = c('S&P 500','Russell 2000','Europe, Australasia, Far East developed', 'Emerging Markets','Agg bond','REIT','Gold')
# download data using quantmod
getSymbols(symbols, from = '2003-01-01', auto.assign = TRUE)
# run a chart
candleChart(SPY,theme='white', type='candles')
# run a chart with shorter history so we can see better
SPY_6MO<-SPY['2011-06-01::']
candleChart(SPY_6MO,theme='white', type='candles')

