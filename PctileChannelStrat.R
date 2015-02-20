#
#Percentile Channels Strategy
#from https://quantstrattrader.wordpress.com/2015/02/17/an-attempt-at-replicating-david-varadis-percentile-channels-strategy/
#

require(quantmod)
require(caTools)
require(PerformanceAnalytics)
require(TTR)
getSymbols(c("LQD", "DBC", "VTI", "ICF", "SHY"), from="1990-01-01")

prices <- cbind(Ad(LQD), Ad(DBC), Ad(VTI), Ad(ICF), Ad(SHY))
prices <- prices[!is.na(prices[,2]),]
returns <- Return.calculate(prices)
cashPrices <- prices[, 5]
assetPrices <- prices[, -5]

#
#function for computing the percentile channel positions for a given parameter setting. Unfortunately, it is not instantaneous due to R’s rollapply function paying a price in speed for generality. While the package caTools has a runquantile function, as of the time of this writing, I have found differences between its output and runMedian in TTR
#
pctChannelPosition <- function(prices, rebal_on=c("months", "quarters"),
                               dayLookback = 60, 
                               lowerPct = .25, upperPct = .75) {
  
  upperQ <- rollapply(prices, width=dayLookback, quantile, probs=upperPct)
  lowerQ <- rollapply(prices, width=dayLookback, quantile, probs=lowerPct)
  positions <- xts(matrix(nrow=nrow(prices), ncol=ncol(prices), NA), order.by=index(prices))
  positions[prices > upperQ] <- 1
  positions[prices < lowerQ] <- -1
  
  ep <- endpoints(positions, on = rebal_on[1])
  positions <- positions[ep,]
  positions <- na.locf(positions)
  positions[is.na(positions)] <- 0 
  
  colnames(positions) <- colnames(prices)
  return(positions)
}

#
#The way this function works is simple: computes a running quantile using rollapply, and then scores anything with price above its 75th percentile as 1, and anything below the 25th percentile as -1, in accordance with David Varadi’s post.
#
#It then subsets these quantities on months (quarters is also possible–or for that matter, other values, but the spirit of the strategy seems to be months or quarters), and imputes any NAs with the last known observation, or zero, if it is an initial NA before any position is found. Something I have found over the course of writing this and the QTS strategy is that one need not bother implementing a looping mechanism to allocate positions monthly if there isn’t a correlation matrix based on daily data involved every month, and it makes the code more readable.
#
#
#Next, we find our composite position.


#find our positions, add them up
d60 <- pctChannelPosition(assetPrices)
d120 <- pctChannelPosition(assetPrices, dayLookback = 120)
d180 <- pctChannelPosition(assetPrices, dayLookback = 180)
d252 <- pctChannelPosition(assetPrices, dayLookback = 252)
compositePosition <- (d60 + d120 + d180 + d252)/4

#Next, find the running volatility for the assets, and subset them to the same time period (in this case months) as our composite position. In David Varadi’s example, the parameter is a 20-day lookback.
#

#find 20-day rolling standard deviations, subset them on identical indices
#to the percentile channel monthly positions
sd20 <- xts(sapply(returns[,-5], runSD, n=20), order.by=index(assetPrices))
monthlySDs <- sd20[index(compositePosition)]

#
#Next, perform the following steps: find the inverse volatility of these quantities, multiply by the composite position score, take the absolute value, and keep any position for which the composite position is greater than zero (or technically speaking, has positive signage). Due to some initial NA rows due to a lack of data (either not enough days to compute a running volatility, or no positive positions yet), those will simply be imputed to zero. Reinvest the remainder in cash.
#
#

#



#compute inverse volatilities
inverseVols <- 1/monthlySDs

#multiply inverse volatilities by composite positions
invVolPos <- inverseVols*compositePosition

#take absolute values of inverse volatility multiplied by position
absInvVolPos <- abs(invVolPos)

#normalize the above quantities
normalizedAbsInvVols <- absInvVolPos/rowSums(absInvVolPos)

#keep only positions with positive composite positions (remove zeroes/negative)
nonCashPos <- normalizedAbsInvVols * sign(compositePosition > 0)
nonCashPos[is.na(nonCashPos)] <- 0 #no positions before we have enough data

#add cash position which is complement of non-cash position
finalPos <- nonCashPos
finalPos$cashPos <- 1-rowSums(finalPos)


#
#
#
#
#compute returns
stratRets <- Return.portfolio(R = returns, weights = finalPos)
charts.PerformanceSummary(stratRets)
stats <- rbind(table.AnnualizedReturns(stratRets), maxDrawdown(stratRets))
rownames(stats)[4] <- "Worst Drawdown"
stats