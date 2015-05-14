require(lpSolve)
Loading required package: lpSolve

   # find maximum return portfolio (rightmost point of efficient frontier)
   # will be 100% of highest return asset
   # maximize
   # w1 * stocks +w2 *bills +w3*bonds + w4 * gold
   # subject to 0 <= w <= 1  for each w
   # will pick highest return asset with w=1
   # skipping 0 constraint, no negative return assets, so not binding
  
   opt.objective <- realreturns
 opt.constraints <- matrix (c(1, 1, 1, 1,  # constrain sum of weights to 1
                               +  						  1, 0, 0, 0,  # constrain w1 <= 1
                               +							  0, 1, 0, 0,  # constrain w2 <= 1
                               +							  0, 0, 1, 0,  # constrain w3 <= 1
                               +							  0, 0, 0, 1)  # constrain w4 <= 1
                             +							, nrow=5, byrow=TRUE)

   opt.operator <- c("=", "<=", "<=", "<=", "<=")
 opt.rhs <- c(1, 1, 1, 1, 1)
 opt.dir="max"

   solution.maxret = lp (direction = opt.dir,
                          +   opt.objective,
                          +   opt.constraints,
                          +   opt.operator,
                          +   opt.rhs)

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

 wts.maxret
 ret.maxret
 vol.maxret