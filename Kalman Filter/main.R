library(egcm)
library(xts)
library(PerformanceAnalytics)
library(rollRegres)

source('/Users/carmineminichini/Desktop/Kalman Filter/Cointegration Pairs Trading/kalman_filter.R')
source('/Users/carmineminichini/Desktop/Kalman Filter/Cointegration Pairs Trading/least_squares.R')
source('/Users/carmineminichini/Desktop/Kalman Filter/Cointegration Pairs Trading/calc_spread.R')
source('/Users/carmineminichini/Desktop/Kalman Filter/Cointegration Pairs Trading/signals.R')
source('/Users/carmineminichini/Desktop/Kalman Filter/Cointegration Pairs Trading/pairs_trading.R')
source('/Users/carmineminichini/Desktop/Kalman Filter/Cointegration Pairs Trading/ema_zscore.R')

# Leggi
df <- read.csv('/Users/carmineminichini/Desktop/Kalman Filter/data/ftsemib_2015-2020.csv')

# stocks da considerare 
assets <- c('DIA.MI','AMP.MI')

# in xts
xy <- xts(df[,assets],order.by=as.Date(df[,ncol(df)]))

# x 
x <- xy[,assets[2]]

# y 
y <- xy[,assets[1]]

# splits date
split_date <- '2019-01-01'
split_train <- paste0('::',split_date)
split_test <- paste0(split_date,'::')

# train
x_train <- x[split_train]
y_train <- y[split_train]

# test
x_test <- x[split_test]
y_test <- y[split_test]

# yx_test
yx_test <- cbind(y_test,x_test)

#yx_train
yx_train <- cbind(y_train,x_train)

#################
####### ANALISI 
#################

ls <- alpha_beta_LS(x_train,y_train,method='log',x_test[,1])

ls_trading <- pairs_trading(yx_test,ls$alpha_test,ls$beta_test,name='LS',
                            0.2,plot=F)

proof <- ls_trading$strategy


