# http://blog.fosstrading.com/2014/03/intro-to-portfolioanalytics.html

# intro to portfolio analytics
# The primary functions in PortfolioAnalytics to specify a portfolio with 
# constraints and objectives are portfolio.spec, add.constraint, and add.objective.

library(PortfolioAnalytics)
data(edhec)
returns <- edhec[, 1:6]
funds <- colnames(returns)

#create a portfolio object with portfolio.spec
init.portfolio <- portfolio.spec(assets = funds)

print.default(init.portfolio)

# adding constraints; The full investment constraint is a special case of 
# the leverage constraint that specifies the weights must sum to 1 and 
# is specified with the alias type="full_investment

init.portfolio <- add.constraint(portfolio = init.portfolio, type = "full_investment")

# add box constraint to specify a long only portfolio. The long only constraint is 
# a special case of a box constraint where the lower bound of the weights of each asset 
# is equal to 0 and the upper bound of the weights of each asset is equal to 1. 
# This is specified with type="long_only" as shown below. The box constraint also allows 
# for per asset weights to be specified.

init.portfolio <- add.constraint(portfolio = init.portfolio, type = "long_only")

# This re-uses the constraints from init.portfolio and adds the objectives specified 
# below to minSD.portfolio and meanES.portfolio while leaving init.portfolio unchanged. 
# This is useful for testing multiple portfolios with different objectives using the 
# same constraints because the constraints only need to be specified once and several 
# new portfolios can be created using an initial portfolio object.

# Add objective for portfolio to minimize portfolio standard deviation
minSD.portfolio <- add.objective(portfolio=init.portfolio, 
                                 type="risk", 
                                 name="StdDev")
# Add objectives for portfolio to maximize mean per unit ES
meanES.portfolio <- add.objective(portfolio=init.portfolio, 
                                  type="return", 
                                  name="mean")

meanES.portfolio <- add.objective(portfolio=meanES.portfolio, 
                                  type="risk", 
                                  name="ES")

#Print portfolios on the screen as needed

print(minSD.portfolio)
print(meanES.portfolio)


# Run the optimization for the minimum standard deviation portfolio
minSD.opt <- optimize.portfolio(R = returns, portfolio = minSD.portfolio, 
                                optimize_method = "ROI", trace = TRUE)

print(minSD.opt)

# Run the optimization for the maximize mean per unit ES
meanES.opt <- optimize.portfolio(R = returns, portfolio = meanES.portfolio, 
                                 optimize_method = "ROI", trace = TRUE)

print(meanES.opt)

plot(minSD.opt, risk.col="StdDev", chart.assets=TRUE, 
     main="Min SD Optimization",
     ylim=c(0, 0.0083), xlim=c(0, 0.06))

plot(meanES.opt, chart.assets=TRUE, 
     main="Mean ES Optimization",
     ylim=c(0, 0.0083), xlim=c(0, 0.16))
