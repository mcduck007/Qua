# http://www.r-bloggers.com/visualization-of-probabilistic-forecasts/

#HDR for time series object
# Assumes that forecasts can be computed and futures simulated from object
forecasthdr <- function(object, h = ifelse(object$m > 1, 2 * object$m, 10), 
                        nsim=2000, plot=TRUE, level=c(50,95), xlim=NULL, ylim=NULL, ...)
{
  require(hdrcde)
  # Compute forecasts
  fc <- forecast(object)
  ft <- time(fc$mean)
  
  # Simulate future sample paths
  sim <- matrix(0,nrow=h,ncol=nsim)
  for(i in 1:nsim)
    sim[,i] <- simulate(object, nsim=h)
  
  # Compute HDRs
  nl <- length(level)
  hd <- array(0, c(h,nl,10))
  mode <- numeric(h)
  for(k in 1:h)
  {
    hdtmp <- hdr(sim[k,], prob=level)
    hd[k,,1:ncol(hdtmp$hdr)] <- hdtmp$hdr
    mode[k] <- hdtmp$mode
  }
  # Remove unnecessary sections of HDRs
  nz <- apply(abs(hd),3,sum) > 0
  hd <- hd[,,nz]
  dimnames(hd)[[1]] <- 1:h
  dimnames(hd)[[2]] <- level
  
  
  # Produce plot if required
  if(plot)
  {
    if(is.null(xlim))
      xlim <- range(time(object$x),ft)
    if(is.null(ylim))
      ylim <- range(object$x, hd)
    plot(object$x,xlim=xlim, ylim=ylim, ...)
    # Show HDRs
    cols <- rev(colorspace::sequential_hcl(52))[level - 49]
    for(k in 1:h)
    {
      for(j in 1:nl)
      {
        hdtmp <- hd[k,j,]
        nint <- length(hdtmp)/2
        for(l in 1:nint)
        {
          polygon(ft[k]+c(-1,-1,1,1)/object$m/2, 
                  c(hdtmp[2*l-1],hdtmp[2*l],hdtmp[2*l],hdtmp[2*l-1]),
                  col=cols[j], border=FALSE)
        }
      }
      points(ft[k], mode[k], pch=19, col="blue",cex=0.8)
    }
    #lines(fc$mean,col='blue',lwd=2)
  }
  
  # Return HDRs
  return(list(hdr=hd,mode=mode,level=level))
}

#___________________

fit <- ets(hsales)
plot(forecast(fit),include=120)
plot(forecast(fit,level=c(50,95)),include=120)
plot(forecast(fit,fan=TRUE),include=120)

#____________________

z <- forecasthdr(fit,xlim=c(1986,1998),nsim=5000,
                 xlab="Year",ylab="US monthly housing sales")

