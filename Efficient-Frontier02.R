###########
#Copied From
#http://zoonek.free.fr/blosxom/R/2012-06-01_Optimization.html
##########

##################
# Real-world data
################
library(quantmod)
symbols <- c("AAPL", "DELL", "GOOG", "MSFT", "AMZN", "BIDU", "EBAY", "YHOO")
d <- list()
for(s in symbols) {
  tmp <- getSymbols(s, auto.assign=FALSE, verbose=TRUE)
  tmp <- Ad(tmp)
  names(tmp) <- "price"
  tmp <- data.frame( date=index(tmp), id=s, price=coredata(tmp) )
  d[[s]] <- tmp
}
d <- do.call(rbind, d)
d <- d[ d$date >= as.Date("2007-01-01"), ]
rownames(d) <- NULL

# Weekly returns
library(plyr)
library(reshape2)
d$next_friday <- d$date - as.numeric(format(d$date, "%u")) + 5
d <- subset(d, date==next_friday)
d <- ddply(d, "id", mutate,
           previous_price = lag(xts(price,date)),
           log_return    = log(price / previous_price),
           simple_return = price / previous_price - 1
)
d <- dcast(d, date ~ id, value.var="simple_return")

# Variance matrix
# There are few assets: the sample variance will suffice
V <- cov(as.matrix(d[,-1]), use="complete")
plot(
  as.dendrogram( hclust( 
    as.dist(sqrt(2-cor(as.matrix(d[,-1]), use="complete"))) 
  ), h=.1 ), 
  horiz=TRUE, xlim=c(1.3,1)
)
library(ellipse)
plotcorr(cov2cor(V))

# Expected returns
mu <- apply(d[,-1], 2, mean, na.rm=TRUE)

V0  <- V
mu0 <- mu
plot_assets <- function(V=V0, mu=mu0) {
  plot( 
    sqrt(diag(V)), mu, 
    xlim = sqrt(c(0,max(diag(V)))), 
    ylim = range(c(0,mu)), 
    pch = 15, las = 1, 
    xlab = "Risk", ylab = "Return" 
  )
  text( sqrt(diag(V)), mu, colnames(V), adj=c(.5,-.5) )
}
plot_assets()