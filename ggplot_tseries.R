#ggplot for tmie series
#http://www.theresearchkitchen.com/archives/934

#Norm funcs
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

#

library(quantmod)

getSymbols("^GSPC", src = "yahoo")
getSymbols("AMD", src = "yahoo")
getSymbols('DJIA',src='FRED')
getSymbols('SP500',src='FRED')


spxnw=nrmw.stk(GSPC,length(GSPC))

leg=c("SPX")



limy=c(0.45,1.1)



library(ggplot2)
ggplot(spxnw, aes(x=index(spxnw), y=cl)) + 
  geom_line() + 
  ggtitle("S & P 500 Normalized")

#ggplot(spxnw, aes(x=(price))) + 
#  geom_histogram()

#ggplot(spxnw, aes(x=(price))) + 
#  geom_histogram(position="identity", binwidth=1)

ggplot(GSPC, aes(x=index(GSPC), y=GSPC.Close)) + 
  geom_line() + 
  ggtitle("S & P 500 Closing Price")

ggplot(GSPC, aes(x=index(GSPC), y=GSPC.Volume)) + 
  geom_line() + 
  ggtitle("S & P 500 Volume")

