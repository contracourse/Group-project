library(downloader)
library(quantmod)
library(PerformanceAnalytics)
library(TTR)
library(Quandl)
library(data.table)
 
download("https://cdn.cboe.com/api/global/us_indices/daily_prices/VIX3M_History.csv", destfile="vxvData.csv")

VIX <- fread("https://cdn.cboe.com/api/global/us_indices/daily_prices/VIX_History.csv")
VIXdates <- VIX$DATE
VIX$DATE <- NULL; VIX <- xts(VIX, order.by=as.Date(VIXdates, format = '%m/%d/%Y'))
 
vxv <- xts(read.zoo("vxvData.csv", header=TRUE, sep=",", format="%m/%d/%Y"))

download("https://dl.dropboxusercontent.com/s/jk6der1s5lxtcfy/XIVlong.TXT",
         destfile="longXIV.txt")
 
xiv <- xts(read.zoo("longXIV.txt", format="%Y-%m-%d", sep=",", header=TRUE))

xivRets <-  Return.calculate(Cl(xiv))

download("https://dl.dropboxusercontent.com/s/950x55x7jtm9x2q/VXXlong.TXT", 
         destfile="longVXX.txt")

vxx <- xts(read.zoo("longVXX.txt", format="%Y-%m-%d", sep=",", header=TRUE))
vxxRets <- Return.calculate(Cl(xiv))


ma_vRatio <- SMA(Cl(VIX)/Cl(vxv), 10)
xivSigVratio <- ma_vRatio < 1 
vxxSigVratio <- ma_vRatio > 1 
 
# V-ratio (VXV/VXMT)
vRatio <- lag(xivSigVratio) * xivRets + lag(vxxSigVratio) * vxxRets
# vRatio <- lag(xivSigVratio, 2) * xivRets + lag(vxxSigVratio, 2) * vxxRets
 
 
# Volatility Risk Premium Strategy
spy <- getSymbols("SPY", from = "2012-01-01", src = "yahoo", type = "xts")
spyRets <- Return.calculate(SPY$SPY.Close)
histVol <- runSD(spyRets, n = 10, sample = FALSE) * sqrt(252) * 100
vixDiff <- Cl(VIX) - histVol
maVixDiff <- SMA(vixDiff, 5)
 
vrpXivSig <- maVixDiff > 0 
vrpVxxSig <- maVixDiff < 0
vrpRets <- lag(vrpXivSig, 1) * xivRets + lag(vrpVxxSig, 1) * vxxRets
 
 
obsCloseMomentum <- magicThinking # from previous post
 
compare <- na.omit(cbind(xivRets, vRatio, vrpRets))
colnames(compare) <- c("BH_XIV", "DDN_VRatio", "DDN_VRP")
charts.PerformanceSummary(compare, main = "strategies")


## PCR analysis
##
library(pls)
set.seed (1000)

pcr_model <- pcr(Sepal.Length~., data = iris, scale = TRUE, validation = "CV")
summary(pcr_model)

# Plot the root mean squared error
validationplot(pcr_model)

# Plot the cross validation MSE
validationplot(pcr_model, val.type="MSEP")

# Plot the R2
validationplot(pcr_model, val.type = "R2")
predplot(pcr_model)
coefplot(pcr_model)
# Train-test split
train <- iris[1:120,]
y_test <- iris[120:150, 1]
test <- iris[120:150, 2:5]
    
pcr_model <- pcr(Sepal.Length~., data = train,scale =TRUE, validation = "CV")

pcr_pred <- predict(pcr_model, test, ncomp = 3)
mean((pcr_pred - y_test)^2)




##PCA analysis govi rates
library(quantmod)
library(downloader)
library(ggplot2)
library(reshape2)
library(corrplot)
library(data.table)


terms = c(1, 2, 3, 5, 7, 10, 30)
for (term in terms) {
  getSymbols(paste('DGS', term, sep=''), src='FRED')
}


DGS1 = DGS1[!is.na(DGS1)]
DGS2 = DGS2[!is.na(DGS2)]
DGS3 = DGS3[!is.na(DGS3)]
DGS5 = DGS5[!is.na(DGS5)]
DGS7 = DGS7[!is.na(DGS7)]
DGS10 = DGS10[!is.na(DGS10)]
DGS30 = DGS30[!is.na(DGS30)]

start_date <- as.Date('1981-07-01') 
end_date <- as.Date('1982-11-01')

rates = cbind(DGS1, DGS2, DGS3, DGS5, DGS7, DGS10, DGS30)


train <- rates["1981-07-01/1982-11-01"]
test <- last(rates, 334)

test %>% as.matrix()
train %>% as.matrix()

#
rates = last(rates, 500)

dataframe = data.frame(index(rates), rates)
colnames(dataframe) = c('date', 'y1', 'y2', 'y3', 'y5', 'y7', 'y10', 'y30')
melted = melt(dataframe, id.vars='date')
plot = ggplot(data=melted, aes(x=date, y=value, color=variable)) + geom_line() + xlab('Date') + ylab('IRS Yield Rate')
plot

pcadata = rates
colnames(pcadata) = c('y1', 'y2', 'y3', 'y5', 'y7', 'y10', 'y30')
fit = princomp(na.omit(pcadata, cor=FALSE, scores=TRUE))
summary(fit)

covariance_matrix = cor(pcadata)
corrplot(covariance_matrix, method='shade', type='full', shade.col=NA, tl.col='black')

#useful visual PCA library
library(ggbiplot)
ggbiplot(fit, obs.scale=1, var.scale=1)
ggscreeplot(fit)


scores = fit$scores
scores_dataframe = data.frame(index(rates), scores)
colnames(scores_dataframe) = c('date', 'pc1', 'pc2', 'pc3', 'pc5', 'pc6', 'pc7', 'pc8')
keeps = c('date', 'pc1', 'pc2', 'pc3', 'pc5', 'pc6', 'pc7', 'pc8')
scores_dataframe = scores_dataframe[keeps]
scores_melted = melt(scores_dataframe, id.vars='date')
plot = ggplot(data=scores_melted, aes(x=as.Date(date), y=value, color=variable)) + geom_line() + xlab('Date') + ylab('Principal Component Score')
plot

#interpreting results
loadings = with(fit, unclass(loadings))
View(loadings)

## Since PCA is a useful tool to analyse feature reduction in models
## we use the PCR in order to predict the future volatility in PCAs 

## PCR analysis

library(pls) # for performing PCR
library(ISLR) # for Hitters dataset
set.seed(1)
pcr.fit = pcr( DGS1 ~ ., data = train, scale = TRUE, validation = "CV" )
summary(pcr.fit)

validationplot(pcr.fit, val.type = 'MSEP')

min.pcr = which.min( MSEP( pcr.fit )$val[1,1, ] ) - 1
min.pcr

coef(pcr.fit, ncomp = min.pcr)

testt <- head( predict( pcr.fit, ncomp = min.pcr ) )


coef.mat = matrix(NA, 6, 6)
for(i in 1:6){
  coef.mat[,i] = pcr.fit$coefficients[,,i]
}

plot(coef.mat[1,], type = 'l', ylab = 'Coefficients', 
     xlab = 'Number of components', ylim = c(min(coef.mat), max(coef.mat)))
for(i in 2:6){
  lines(coef.mat[i,], col = i)
}

abline(v = min.pcr, lty = 3)


PVE <- rep(NA,6)
for(i in 1:6){ PVE[i]<- sum(pcr.fit$Xvar[1:i])/pcr.fit$Xtotvar }
barplot( PVE, names.arg = 1:6, main = "scree plot", 
         xlab = "number of PCs", 
         ylab = "proportion of variance explained" )

####

train <- na.omit(train)
test <- na.omit(test)

simple_lm <- lm(DGS1 ~., data = train)
summary(simple_lm)
library(Metrics)
lm_pred <- predict(simple_lm, test)
#Lower RMSE is a better model
rmse(actual = test$DGS1, predicted = as.numeric(lm_pred))


# install.package("pls")
library(pls)
pcr_model <- pcr(DGS1 ~ .,
                 data = train,
                 scale = TRUE,
                 validation = "CV")
summary(pcr_model)
pcr_pred <- predict(pcr_model, test, ncomp = 5)
rmse(actual = test$DGS1, predicted = as.numeric(pcr_pred))
