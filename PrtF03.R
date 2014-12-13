# http://tradingposts.wordpress.com/2013/06/27/portfolio-optimization/

minVarPortfolio= function(tickers,start='2000-01-01',end=Sys.Date(),
                          riskfree=0,short=TRUE,lowestWeight=-1,highestWeight=1){
  
  # Load up the package
  require(tseries)
  
  #Initialize all the variables we will be using. returnMatrix is 
  #initailized as a vector,with length equal to one of the input 
  #ticker vectors (dependent on the start and end dates).
  #Sharpe is set to 0. The weights vector is set equal in 
  #length to the number of tickers. The portfolio is set to 
  #NULL. A 'constraint' variable is created to pass on the 
  #short parameter to the portfolio.optim function. And vectors 
  #are created with the low and high weight restrictions, which
  #are then passed to the portfolio.optim function as well. ##
  
  returnMatrix=vector(length=length(getSymbols(tickers[1],
                                               auto.assign=FALSE,from=start,to=end)))
  sharpe=0
  weights=vector(,length(tickers))
  port=NULL
  constraint=short
  lowVec=rep(lowestWeight,length(tickers))
  hiVec=rep(highestWeight,length(tickers))
  
  #This is a for-loop which cycles through the tickers, calculates 
  #their return, and stores the returns in a matrix, adding 
  #the return vector for each ticker to the matrix
  
  for(i in 1:length(tickers)){
    temp=getSymbols(tickers[i],auto.assign=FALSE,from=start,to=end)
    if(i==1){
      returnMatrix=diff(log(Ad(temp)))
    }
    else{
      returnMatrix=cbind(returnMatrix,diff(log(Ad(temp))))
    }
  }
  
  returnMatrix[is.na(returnMatrix)]=0
  it
  
  #This for-loop cycles through returns to test the portfolio.optim function 
  #for the highest Sharpe ratio.
  for(j in 1:100){
    
    #Stores the log of the return in retcalc
    retcalc=log((1+j/100))
    retcalc=retcalc/252
    print(paste("Ret Calc:",retcalc))
    
    #Tries to see if the specified return from retcalc can result 
    #in an efficient portfolio
    try(port<-portfolio.optim(returnMatrix,pm=retcalc,shorts=constraint,
                              reslow=lowVec,reshigh=hiVec,riskfree=riskfree),silent=T)
    
    #If the portfolio exists, it is compared against previous portfolios 
    #for different returns using the #Sharpe ratio. If it has the highest 
    #Sharpe ratio, it is stored and the old one is discarded.
    if(!is.null(port)){
      print('Not Null')
      sd=port$ps
      tSharpe=((retcalc-riskfree)/sd)
      print(paste("Sharpe",tSharpe))
      
      if(tSharpe>sharpe){
        sharpe=tSharpe
        weights=port$pw
      }}
    
  }
  print(paste('Sharpe:', sharpe))
  print(rbind(tickers,weights))
  return(returnMatrix)
  
}