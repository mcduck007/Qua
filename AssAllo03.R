# http://blog.fosstrading.com/2011/07/creating-financial-instrument-metadata.html
#
#
#

# install.packages("FinancialInstrument", repos="http://R-Forge.R-project.org")
require(FinancialInstrument)
Data <- data.frame(primary_id="CL", month_cycle="F,G,H,J,K,M,N,Q,U,V,X,Z")
Data <- rbind(Data, data.frame(primary_id="STXE", month_cycle="H,M,U,Z"))
Data
#   primary_id               month_cycle
# 1         CL   F,G,H,J,K,M,N,Q,U,V,X,Z
# 2       STXE                   H,M,U,Z

build_series_symbols(Data, yearlist=c(0,1,2))
[1] "CLF0"   "CLG0"   "CLH0"   "CLJ0"   "CLK0"   "CLM0"   "CLN0"   "CLQ0"  
[9] "CLU0"   "CLV0"   "CLX0"   "CLZ0"   "STXEH0" "STXEM0" "STXEU0" "STXEZ0"
[17] "CLF1"   "CLG1"   "CLH1"   "CLJ1"   "CLK1"   "CLM1"   "CLN1"   "CLQ1"  
[25] "CLU1"   "CLV1"   "CLX1"   "CLZ1"   "STXEH1" "STXEM1" "STXEU1" "STXEZ1"
[33] "CLF2"   "CLG2"   "CLH2"   "CLJ2"   "CLK2"   "CLM2"   "CLN2"   "CLQ2"  
[41] "CLU2"   "CLV2"   "CLX2"   "CLZ2"   "STXEH2" "STXEM2" "STXEU2" "STXEZ2"

# read in data that would be suitable for load.instruments on root contracts
Data <- read.csv("series_data.csv", stringsAsFactors=FALSE)
# set the type to guaranteed_spread
Data$type <- "guaranteed_spread"
# call build_spread_symbols
output <- build_spread_symbols(Data[6:7,], start_date="2010-01-01")



