#http://isomorphism.es/post/65454224472/trading-time-impact-size
#"trading time" and price impact of a trade proportional to exp( √size )
#This doesn’t run as is. I think you can fix that by combining the ggplot pieces differently.
#

require(quantmod)
getSymbols("MER")                           #Merrill Lynch

#Gatheral's model
HiLo <- function(symbol) log( Hi(symbol) / Lo(symbol) ) **2
UpDay <- function(symbol) Cl(symbol) > Op(symbol)

#munging
mer <- merge( MER, UpDay(MER), HiLo(MER) )
mer <- data.frame(mer)
names(mer)[7] = "UpDay"
names(mer)[8] = "HiLo"
mer <- subset(mer, Vo(mer) > 0)          #data cleaning

#plot layers
require(ggplot2)
base <- ggplot(data=mer, aes(x=log(MER.Volume), y=HiLo, col=as.factor(UpDay)))
points <- geom_point(alpha=.5)                #no more I(.5) !
line <- geom_smooth(method='lm', col='red', fill='red')
labels <- labs(y="Volatility", x="Volume", colour="Up day?", title="Jim Gatheral's model")

#more munging
winners <- subset(mer, UpDay>0)
losers <- subset(mer, UpDay<0)
col.winner <- '#0077cc'      #yellow
col.loser <- '#ddaa00'       #blue


#more plot layers
rug.winners <- geom_rug(alpha=.5, col=col.winner, data=winners, sides='tr')
rug.losers <- geom_rug(alpha=.5, col=col.loser, data=losers, sides='bl')
colour <- scale_colour_manual(values= c(col.loser, '#333333', col.winner) )       #some days $MER ended up where it started: grey those



###PAYOFF###
#simpler plot with margins and fit
base + points + line + labels + colour + rug.winners + rug.losers

#what was hard about that was combining new columns
#(model columns: Jim Gatheral's transforms) in xts,
#and combining xts with ggplot2
###########



#now let's close with an even better, but larger, ggplot: replacing the rugs with smoothed histograms like http://stackoverflow.com/a/8545849/563329
require(gridExtra)

loser.depth.hist <- ggplot() + geom_density(data=losers, fill=col.loser, col=FALSE, alpha=.5, aes(x=log(Vo(mer)))) + theme_bw() %+replace% theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

#the differences among these long lines are just substitutions: s/volume/volatility/ and s/loser/winner/
winner.depth.hist <- ggplot() + geom_density(data=winners, fill=col.winner, col=FALSE, alpha=.5, aes(x=log(Vo(mer)))) + theme_bw() %+replace% theme(axis.text.y = element_blank(), axis.ticks.y = element_blank() )
loser.range.hist <- ggplot() + geom_density(data=losers, fill=col.loser, col=FALSE, alpha=.5, aes(x=HiLo(mer))) + theme_bw() %+replace% theme( axis.text.y = element_blank(), axis.ticks.y = element_blank() )
winner.range.hist <- ggplot() + geom_density(data=winners, fill=col.winner, col=FALSE, alpha=.5, aes(x=HiLo(mer))) + theme_bw() %+replace% theme( axis.text.y = element_blank(), axis.ticks.y = element_blank() )
#depth and range are better abbrevs than vol and vol


#now to align these on the edges of the graph while still keeping things straight as to what goes on top, left, right: some gratuitous titling

right.hist <- winner.range.hist + coord_flip()

left.hist <- loser.range.hist + coord_flip() + scale_x_reverse()

top.hist <- winner.depth.hist

bottom.hist <- loser.depth.hist + scale_y_reverse()


#http://stackoverflow.com/a/8545849/563329
empty <- ggplot() + geom_point(aes(1,1), colour='white') + theme(axis.ticks=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank()  )



###SECOND PAYOFF###

grid.arrange(
  empty, top.hist, empty,
  left.hist, base + points + line + labels + colour + rug.winners + rug.losers, right.hist,
  empty, bottom.hist, empty,
  widths=c(1,10,1),
  heights=c(1,10,1)
)

# phew!