# required libraries
library(quantmod)
library(PerformanceAnalytics)
library(tseries)

# symbols
symbols <- c("SPY", "VGK", "EWJ", "EEM", "VNQ", "RWX", "IEF", "TLT", "DBC", "GLD", "VWO", "BND")

# get data
rets <- list()
for (i in 1:length(symbols)) {
  returns <- Return.calculate(Ad(get(getSymbols(symbols[i], from = "2017-01-01", src = "yahoo"))))
  colnames(returns) <- symbols[i]
  rets[[i]] <- returns
}
rets <- na.omit(do.call(cbind, rets))
View(rets)

plot(rets$SPY)