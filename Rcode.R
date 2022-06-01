# required libraries
library(quantmod)
library(PerformanceAnalytics)
library(tseries)


# ETN volatility strategy 

symbols <- c("VIXY", "VXX", "VXZ", "SHY")


prices <- list()
for(i in 1:length(symbols)) {
  price <- (Ad(get(getSymbols(symbols[i], from = "2012-01-01", src = "yahoo", type = "xts"))))
  colnames(price) <- symbols[i]
  prices[[i]] <- price
}
prices <- na.omit(do.call(cbind, prices))
returns <- na.omit(Return.calculate(prices))
View(returns)

#####

# find highest asset, assign column names
topAsset <- function(row, assetNames) {
  out <- row==max(row, na.rm = TRUE)
  names(out) <- assetNames
  out <- data.frame(out)
  return(out)
}

# compute momentum
momentums <- na.omit(xts(apply(prices, 2, ROC, n = 83), order.by=index(prices)))

# find highest asset each day, turn it into an xts
highestMom <- apply(momentums, 1, topAsset, assetNames = colnames(momentums))
highestMom <- xts(t(do.call(cbind, highestMom)), order.by=index(momentums))

# observe today's close, buy tomorrow's close
buyTomorrow <- na.omit(xts(rowSums(returns * lag(highestMom, 2)), order.by=index(highestMom)))

# observe today's close, buy today's close (aka magic thinking)
magicThinking <- na.omit(xts(rowSums(returns * lag(highestMom)), order.by=index(highestMom)))

out <- na.omit(cbind(buyTomorrow, magicThinking))
colnames(out) <- c("buyTomorrow", "magicalThinking")

# results
charts.PerformanceSummary(out['2017-04-11::'], legend.loc = 'top')
rbind(table.AnnualizedReturns(out['2017-04-11::']), maxDrawdown(out['2017-04-11::']))








# # symbols
# symbols <- c("SPY", "VGK", "EWJ", "EEM", "VNQ", "RWX", "IEF", "TLT", "DBC", "GLD", "VWO", "BND")

# # get data
# rets <- list()
# for (i in 1:length(symbols)) {
#   returns <- Return.calculate(Ad(get(getSymbols(symbols[i], from = "2017-01-01", src = "yahoo"))))
#   colnames(returns) <- symbols[i]
#   rets[[i]] <- returns
# }
# rets <- na.omit(do.call(cbind, rets))
# View(rets)

# plot(rets$SPY)

