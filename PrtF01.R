# http://www.r-bloggers.com/structural-arb-analysis-and-portfolio-management-functionality-in-r/
#
#long TMF 60% (leveraged 3x t-bond bull), long XIV 40%
getSymbols("XIV", from="1990-01-01")
getSymbols("TMF", from="1990-01-01")
tmfRets <- Return.calculate(Cl(TMF))
xivRets <- Return.calculate(Ad(XIV))
both <- merge(xivRets, tmfRets, join='inner')
colnames(both) <- c("xiv", "tmf")
portfRets <- Return.rebalancing(both, weights=c(.4, .6),
                                rebalance_on="weeks", geometric=FALSE)
colnames(portfRets) <- "XIVTMF"
getSymbols("SPY", from="1990-01-01")
SPYrets <- diff(log(Cl(SPY)))
charts.PerformanceSummary(merge(portfRets, SPYrets, join='inner'))

#
SharpeRatio.annualized(merge(portfRets, SPYrets, join='inner'))

Return.annualized(merge(portfRets, SPYrets, join='inner'))

maxDrawdown(merge(portfRets, SPYrets, join='inner'))

