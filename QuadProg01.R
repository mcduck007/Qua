####
#http://blog.ryanwalker.us/2014/01/solving-quadratic-progams-with-rs.html
###


# Get monthly return data from 2012 through 2013
require(quantmod)
myStocks <- c("AAPL","XOM","GOOGL","MSFT","GE","JNJ","WMT","CVX","PG","WF")
getSymbols(myStocks ,src='yahoo')
returnsList <- lapply(myStocks, 
                      function(s) periodReturn(eval(parse(text=s)),
                                               period='monthly', subset='2012::2013'))

# Combine return time series
returns.df  <- do.call(cbind,returnsList)
colnames(returns.df) <- myStocks

# Plot monthly return data
require(ggplot2)
require(reshape2)
returns2 <- as.data.frame(returns.df)
returns2$date <- row.names(returns2)
returns2 <- melt(returns2, id="date")
ggplot(returns2, aes(x=date,y=value, group=variable)) +
  geom_line(aes(color=variable), lwd=1.5) +
  ylab("Monthly Return")+ xlab("Date") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Compute the average returns and covariance matrix of the return series
r <- matrix(colMeans(returns.df), nrow=1)
C <- cov(returns.df)


# Stage and solve the QP
require(quadprog)
A    <- matrix(1,1,10)
A    <- rbind(A, r, diag(10),-diag(10))
f    <- c(1, 0.01, rep(0,10),rep(-1,10))
sol <- solve.QP(Dmat=C, dvec = rep(0,10), Amat=t(A), bvec=f, meq=1)


###

require(ggplot2)
portfolio <- data.frame(name = myStocks, w = round(sol$solution,3))
ggplot(portfolio, aes(x=name, y=w)) + geom_bar(stat="identity", fill="blue")