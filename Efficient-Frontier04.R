###
#http://blog.streeteye.com/blog/2012/01/portfolio-optimization-and-efficient-frontiers-in-r/
###

#################################################################
# Create some data
#################################################################

require(quantmod)

# not used in abbreviated example, but useful for reporting
startYear = 1928
endYear = 2010
YEARS =startYear:endYear

# nominal returns
SP500 = c(0.4381,-0.083,-0.2512,-0.4384,-0.0864,0.4998,-0.0119,0.4674,0.3194,-0.3534,0.2928,
          -0.011,-0.1067,-0.1277,0.1917,0.2506,0.1903,0.3582,-0.0843,0.052,0.057,0.183,0.3081,0.2368,
          0.1815,-0.0121,0.5256,0.326,0.0744,-0.1046,0.4372,0.1206,0.0034,0.2664,-0.0881,0.2261,0.1642,
          0.124,-0.0997,0.238,0.1081,-0.0824,0.0356,0.1422,0.1876,-0.1431,-0.259,0.370,0.2383,-0.0698,
          0.0651,0.1852,0.3174,-0.047,0.2042,0.2234,0.0615,0.3124,0.1849,0.0581,0.1654,0.3148,-0.0306,
          0.3023,0.0749,0.0997,0.0133,0.372,0.2382,0.3186,0.2834,0.2089,-0.0903,-0.1185,-0.2197,0.2836,
          0.1074,0.0483,0.1561,0.0548,-0.3658,0.2592,0.148600)

BILLS = c(0.0308,0.0316,0.0455,0.0231,0.0107,0.0096,0.0032,0.0018,0.0017,0.003,0.0008,0.0004,
          0.0003,0.0008,0.0034,0.0038,0.0038,0.0038,0.0038,0.0057,0.0102,0.011,0.0117,0.0148,0.0167,
          0.0189,0.0096,0.0166,0.0256,0.0323,0.0178,0.0326,0.0305,0.0227,0.0278,0.0311,0.0351,0.039,
          0.0484,0.0433,0.0526,0.0656,0.0669,0.0454,0.0395,0.0673,0.0778,0.0599,0.0497,0.0513,0.0693,
          0.0994,0.1122,0.143,0.1101,0.0845,0.0961,0.0749,0.0604,0.0572,0.0645,0.0811,0.0755,0.0561,
          0.0341,0.0298,0.0399,0.0552,0.0502,0.0505,0.0473,0.0451,0.0576,0.0367,0.0166,0.0103,0.0123,
          0.0301,0.0468,0.0464,0.0159,0.0014,0.001300)

BONDS=c(0.0084,0.042,0.0454,-0.0256,0.0879,0.0186,0.0796,0.0447,0.0502,0.0138,0.0421,0.0441,
        0.054,-0.0202,0.0229,0.0249,0.0258,0.038,0.0313,0.0092,0.0195,0.0466,0.0043,-0.003,0.0227,
        0.0414,0.0329,-0.0134,-0.0226,0.068,-0.021,-0.0265,0.1164,0.0206,0.0569,0.0168,0.0373,0.0072,
        0.0291,-0.0158,0.0327,-0.0501,0.1675,0.0979,0.0282,0.0366,0.0199,0.0361,0.1598,0.0129,-0.0078,
        0.0067,-0.0299,0.082,0.3281,0.032,0.1373,0.2571,0.2428,-0.0496,0.0822,0.1769,0.0624,0.150,
        0.0936,0.1421,-0.0804,0.2348,0.0143,0.0994,0.1492,-0.0825,0.1666,0.0557,0.1512,0.0038,0.0449,
        0.0287,0.0196,0.1021,0.201,-0.1112,0.084600)

CPI=c(-0.0115607,0.005848,-0.0639535,-0.0931677,-0.1027397,0.0076336,0.0151515,0.0298507,
      0.0144928,0.0285714,-0.0277778,0.0000,0.0071429,0.0992908,0.0903226,0.0295858,0.0229885,
      0.0224719,0.1813187,0.0883721,0.0299145,-0.0207469,0.059322,0.0600,0.0075472,0.0074906,
      -0.0074349,0.0037453,0.0298507,0.0289855,0.0176056,0.017301,0.0136054,0.0067114,0.0133333,
      0.0164474,0.0097087,0.0192308,0.0345912,0.0303951,0.0471976,0.0619718,0.0557029,0.0326633,
      0.0340633,0.0870588,0.1233766,0.0693642,0.0486486,0.0670103,0.0901771,0.1329394,0.125163,
      0.0892236,0.0382979,0.0379098,0.0394867,0.0379867,0.010979,0.0443439,0.0441941,0.046473,
      0.0610626,0.0306428,0.0290065,0.0274841,0.026749,0.0253841,0.0332248,0.017024,0.016119,
      0.0268456,0.0338681,0.0155172,0.0237691,0.0187949,0.0325556,0.0341566,0.0254065,0.0408127,
      0.0009141,0.0272133,0.0149572)

GOLD = c(0.000000000,0.000000000,0.000000000,0.000000000,0.000000000,0.447002860,0.079661828,
         0.000000000,0.000000000,0.000000000,0.000000000,0.000000000,-0.014388737,0.028573372,0.000000000,
         0.027779564,-0.006872879,0.027212564,0.026491615,0.117056555,-0.023530497,-0.036367644,
         -0.006191970,-0.006230550,-0.033039854,-0.086306904,-0.007067167,-0.002840911,0.001421464,
         0.001419447,0.000000000,0.000000000,0.034846731,-0.027779564,-0.004234304,-0.002832863,
         0.002832863,0.004234304,-0.002820876,0.002820876,0.203228242,-0.059188871,-0.052577816,
         0.136739608,0.358646094,0.511577221,0.545727802,-0.132280611,-0.253090628,0.168898536,
         0.265477915,0.464157559,0.689884535,-0.285505793,-0.201637346,0.120144312,-0.163629424,
         -0.127202258,0.149181164,0.192236014,-0.020385757,-0.134512586,0.005221944,-0.058998341,
         -0.051002554,0.045462374,0.064538521,0.002600782,0.010336009,-0.155436854,-0.121167134,
         -0.052185753,0.000000000,-0.028987537,0.133990846,0.157360955,0.119003292,0.084161792,
         0.308209839,0.142551544,0.220855221,0.110501915,0.228258652)

# put into a data frame (matrix)
fnominal=data.frame(stocks=SP500, bills=BILLS, bonds=BONDS, gold=GOLD, CPI=CPI)
freal=data.frame(stocks=SP500-CPI, bills=BILLS-CPI, bonds=BONDS-CPI, gold=GOLD-CPI)

# compute real returns
realreturns = apply(freal, 2, mean)
realreturnspct = realreturns*100
# print them
realreturnspct

# compute real volatility (standard deviation of real returns)
realsds = apply(freal, 2, sd)
realsdspct = realsds*100
# print them
realsdspct

#################################################################
# Run efficient frontier and transition map with Systematic Investor code
#################################################################

# put input assumption in suitable format
ia = list()
ia$n = length(freal)
ia$annual.factor = 1
ia$symbols = names(freal)
ia$symbol.names = names(freal)
ia$hist.returns = freal

ia$arithmetic.return = apply(ia$hist.returns, 2, mean, na.rm = T)
ia$arithmetic.return = (1 + ia$arithmetic.return)^ia$annual.factor - 1
ia$geometric.return = apply(ia$hist.returns, 2, function(x) prod(1+x)^(1/length(x))-1 )
ia$geometric.return = (1 + ia$geometric.return)^ia$annual.factor - 1
ia$risk = apply(ia$hist.returns, 2, sd, na.rm = T)
ia$risk = sqrt(ia$annual.factor) * ia$risk
ia$correlation = cor(ia$hist.returns, use = 'complete.obs', method = 'pearson')
ia$cov = cov(ia$hist.returns, use = 'complete.obs', method = 'pearson')
ia$expected.return = ia$arithmetic.return

###############################################################################
# Load Systematic Investor Toolbox (SIT)
# http://systematicinvestor.wordpress.com/systematic-investor-toolbox/
###############################################################################
setInternet2(TRUE)
con = gzcon(url('http://www.systematicportfolio.com/sit.gz', 'rb'))
source(con)
close(con)
# if the above doesn't work, download and unpack the URL above in a local dir and try
# source('C:\\Temp\\sit.r')

# do optimizations
n = ia$n
# -1 <= x.i <= 1
constraints = new.constraints(n, lb = 0, ub = 1)
# SUM x.i = 1
constraints = add.constraints(rep(1, n), 1, type = '=', constraints)
ef.risk = portopt(ia, constraints, 50, 'Historical', equally.spaced.risk = T)

# chart
layout( matrix(1:2, nrow = 2) ) # can skip this in RStudio, use back button
plot.ef(ia, list(ef.risk), portfolio.risk, T, T)
# you should see 2 plots

#################################################################
# use linear programming to find maximum return portfolio (all stocks)
#################################################################

install.packages('lpSolve')
require(lpSolve)

# find maximum return portfolio (rightmost point of efficient frontier
# will be 100% of highest return asset
# maximize
# w1 * stocks +w2 *bills +w3*bonds + w4 * gold
# subject to 0 <= w <= 1  for each W
# will pick highest return asset with w=1
# skipping >0 constraint, no negative return assets, so not binding

opt.objective <- realreturns
opt.constraints <- matrix (c(1, 1, 1, 1,  # constrain sum of weights to 1
                             1, 0, 0, 0,  # constrain w1 < 1
                             0, 1, 0, 0,  # constrain w2 < 1
                             0, 0, 1, 0,  # constrain w3 < 1
                             0, 0, 0, 1)  # constrain w4 < 1
                           , nrow=5, byrow=TRUE)

opt.operator <- c("=", "<=", "<=", "<=", "<=")
opt.rhs <- c(1, 1, 1, 1, 1)
opt.dir="max"

solution.maxret = lp (direction = opt.dir,
                      opt.objective,
                      opt.constraints,
                      opt.operator,
                      opt.rhs)

# portfolio weights for max return portfolio
wts.maxret=solution.maxret$solution
# return for max return portfolio
ret.maxret=solution.maxret$objval
# compute return covariance matrix to determine volatility of this portfolio
covmatrix = cov(freal, use = 'complete.obs', method = 'pearson')
# multiply weights x covariances x weights, gives variance
var.maxret = wts.maxret %*% covmatrix %*% wts.maxret
# square root gives standard deviation (volatility)
vol.maxret = sqrt(var.maxret)

#################################################################
# find minimum volatility portfolio
#################################################################

install.packages('quadprog')
require(quadprog)

# minimize variance:  w %*% covmatrix %*% t(w)
# subject to sum of ws = 1
# subject to each w > 0

# solution.minvol <- solve.QP(covmatrix, zeros, t(opt.constraints), opt.rhs, meq = opt.meq)
# first 2 parameters covmatrix, zeros define function to be minimized
# if zeros is all 0s, the function minimized ends up equal to port variance / 2
# opt.constraints is the left hand side of the constraints, ie the cs in
# c1 w1 + c2 w2 ... + cn wn = K
# opt.rhs is the Ks in the above equation
# meq means the first meq rows are 'equals' constraints, remainder are >= constraints
# if you want to do a <= constraint, multiply by -1 to make it a >= constraint
# does not appear to accept 0 RHS, so we make it a tiny number> 0

# compute covariance matrix
covmatrix = cov(freal, use = 'complete.obs', method = 'pearson')

nObs <- length(freal$stocks)
nAssets <- length(freal)

# 1 x numassets array of 1s
opt.constraints <- matrix (c(1, 1, 1, 1,   # sum of weights =1
                             1, 0, 0, 0,   # w1 >= 0
                             0, 1, 0, 0,   # w1 >= 0
                             0, 0, 1, 0,   # w1 >= 0
                             0, 0, 0, 1)   # w2 >= 0
                           , nrow=5, byrow=TRUE)
opt.rhs <- matrix(c(1, 0.000001, 0.000001, 0.000001, 0.000001))
opt.meq <- 1  # first constraint is '=', rest are '>='
# numassets x 1 array of 0s
zeros <- array(0, dim = c(nAssets,1))

solution.minvol <- solve.QP(covmatrix, zeros, t(opt.constraints), opt.rhs, meq = opt.meq)

wts.minvol = solution.minvol$solution
var.minvol = solution.minvol$value *2
ret.minvol = realreturns %*% wts.minvol
vol.minvol = sqrt(var.minvol)

# generate a sequence of 50 evenly spaced returns between min var return and max return
lowreturn=ret.minvol
highreturn=ret.maxret
minreturns=seq(lowreturn, highreturn, length.out=50)

# add a return constraint: sum of weight * return >= x
retconst= rbind(opt.constraints, realreturns)
retrhs=rbind(opt.rhs, ret.minvol)

# create vectors for the returns, vols, and weights along the frontier,
# starting with the minvol portfolio
out.ret=c(ret.minvol)
out.vol=c(vol.minvol)
out.stocks=c(wts.minvol[1])
out.bills=c(wts.minvol[2])
out.bonds=c(wts.minvol[3])
out.gold=c(wts.minvol[4])

#################################################################
# loop and run a minimum volatility optimization for each return level from 2-49
#################################################################

for(i in 2:(length(minreturns) - 1)) {
  print(i)
  # start with existing constraints, no return constraint
  tmp.constraints = retconst
  tmp.rhs=retrhs
  # set return constraint
  tmp.rhs[6] = minreturns[i]
  
  tmpsol <- solve.QP(covmatrix, zeros, t(tmp.constraints), tmp.rhs, meq = opt.meq)
  
  tmp.wts = tmpsol$solution
  tmp.var = tmpsol$value *2
  out.ret[i] = realreturns %*% tmp.wts
  out.vol[i] = sqrt(tmp.var)
  out.stocks[i]=tmp.wts[1]
  out.bills[i]=tmp.wts[2]
  out.bonds[i]=tmp.wts[3]
  out.gold[i]=tmp.wts[4]
}

# put maxreturn portfolio in return series for max return, index =50
out.ret[50]=c(ret.maxret)
out.vol[50]=c(vol.maxret)
out.stocks[50]=c(wts.maxret[1])
out.bills[50]=c(wts.maxret[2])
out.bonds[50]=c(wts.maxret[3])
out.gold[50]=c(wts.maxret[4])

# organize in a data frame
efrontier=data.frame(out.ret*100)
efrontier$vol=out.vol*100
efrontier$stocks=out.stocks*100
efrontier$bills=out.bills*100
efrontier$bonds=out.bonds*100
efrontier$gold=out.gold*100
names(efrontier) = c("Return", "Risk", "%Stocks", "%Bills", "%Bonds", "%Gold")

############################################################
# charts
############################################################

install.packages('ggplot2')
require(ggplot2)

# define colors
dvblue = "#000099"
dvred = "#e41a1c"
dvgreen = "#4daf4a"
dvpurple = "#984ea3"
dvorange = "#ff7f00"
dvyellow = "#ffff33"
dvgray="#666666"

apoints=data.frame(realsdspct)
apoints$realreturns = realreturnspct
ggplot(data=efrontier, aes(x=Risk, y=Return)) +
  opts(title="Efficient Frontier") +
  theme_bw() +
  geom_line(size=1.4) +
  geom_point(aes(x=apoints$realsdspct, y=apoints$realreturns)) +
  scale_x_continuous(limits=c(1,24)) +
  annotate("text", apoints[1,1], apoints[1,2],label=" stocks", hjust=0) +
  annotate("text", apoints[2,1], apoints[2,2],label=" bills", hjust=0) +
  annotate("text", apoints[3,1], apoints[3,2],label=" bonds", hjust=0) +
  annotate("text", apoints[4,1], apoints[4,2],label=" gold", hjust=0) +
  annotate("text", 19,0.3,label="streeteye.com", hjust=0, alpha=0.5)


keep=c("Risk", "%Stocks","%Bills","%Bonds","%Gold")
efrontier.tmp = efrontier[keep]
efrontier.m = melt(efrontier.tmp, id ='Risk')

ggplot(data=efrontier.m, aes(x=Risk, y=value, colour=variable, fill=variable)) +
  theme_bw() +
  opts(title="Transition Map", legend.position="top", legend.direction="horizontal") +
  ylab('% Portfolio') +
  geom_area() +
  scale_colour_manual("", breaks=c("%Stocks", "%Bills", "%Bonds","%Gold"), values = c(dvblue,dvgreen,dvred,dvyellow), labels=c('%Stocks', '%Bills','%Bonds','%Gold')) +
  scale_fill_manual("", breaks=c("%Stocks", "%Bills", "%Bonds","%Gold"), values = c(dvblue,dvgreen,dvred,dvyellow), labels=c('%Stocks', '%Bills','%Bonds','%Gold')) +
  annotate("text", 16,-2.5,label="streeteye.com", hjust=0, alpha=0.5)

