main <- function(dataset,assets,cut_2020=F,split_date,
                 method='normal',
                 st_var=1e-5,
                 ob_var=1e-3,
                 init_var=1e-3,
                 smooth_kf,
                 lag=T,
                 threshold,
                 threshold_kf,
                 plot_spread=F,
                 plot_signals=F,
                 plot_parameters=F,
                 plot_comparison=F)
{
  library(egcm)
  library(xts)
  library(PerformanceAnalytics)
  library(tseries)
  library(dplyr)
  
  source('/Users/carmineminichini/Desktop/Kalman Filter/Cointegration Pairs Trading/kalman_filter.R')
  source('/Users/carmineminichini/Desktop/Kalman Filter/Cointegration Pairs Trading/least_squares.R')
  source('/Users/carmineminichini/Desktop/Kalman Filter/Cointegration Pairs Trading/signals.R')
  source('/Users/carmineminichini/Desktop/Kalman Filter/Cointegration Pairs Trading/pairs_trading.R')
  source('/Users/carmineminichini/Desktop/Kalman Filter/Cointegration Pairs Trading/rolling_2month.R')
  
  ############ LETTURA DEI DATI
  path <- paste0('/Users/carmineminichini/Desktop/Kalman Filter/data/',dataset)
  
  # Leggi
  df <- read.csv(path)
  
  # stocks da considerare 
  assets <- assets
  # in xts
  xy <- xts(df[,assets],order.by=as.POSIXct(df[,ncol(df)]))
  
  # taglio il 2020
  if(cut_2020){
  xy <-xy['::2020-01-01']
  }
  else{NULL}
  
  # x 
  x <- xy[,assets[2]]
  # y 
  y <- xy[,assets[1]]
  
  ######
  
  # splits date
  split_date <- split_date
  split_train <- paste0('::',split_date)
  split_test <- paste0(split_date,'::')
  #######
  
  # train
  x_train <- x[split_train]
  y_train <- y[split_train]
  
  # test
  x_test <- x[split_test]
  y_test <- y[split_test]
  
  # xy_test
  yx_test <- cbind(y_test,x_test)
  
  
  ###############
  #### ANALYSIS
  ##############
  
  # Alpha & beta from Least Squares
  ls <- alpha_beta_LS(x_train,y_train,method=method,x_test[,1])
  
  # Alpha & beta from Kalman Filter
  kf <- kalman_filter(x_test,y_test,ls$alpha_test,ls$beta_test,
                      method,st_var,ob_var,init_var,smooth_kf)
  
  kf_alpha <- kf$alpha
  kf_beta <- kf$beta
  
  # Alpha & Beta from Rolling Regression
  rl <- rolling_2month(x_train,y_train,x_test,y_test)
  
  
  ##############
  ## PLOT PARAMETERS
  #############
  
  
  ls_parameters <- cbind(ls$alpha_test,ls$beta_test)
  colnames(ls_parameters) <- c('Co-integration Alpha','Co-integration Beta')
  if(plot_parameters){
    par(mfrow=c(2,1))
    print(plot(cbind(ls_parameters[,1],kf_alpha),legend.loc='topleft',main='Alpha Co-integration & Kalman Filter',col=c('black','red')))
    print(plot(rl$alpha,legend.loc='topleft',main='Alpha Rolling Least Squares',col=c('blue')))
    par(mfrow=c(2,1))
    print(plot(cbind(ls_parameters[,2],kf_beta),legend.loc='topleft',main='Beta Co-integration & Kalman Filter',col=c('black','red')))
    print(plot(rl$beta,legend.loc='topleft',main='Beta Rolling Least Squares',col=c('blue')))
  }else{NULL}
  
  
  ##########
  #### CALCULATE SPREAD & MSE
  ########
  
  # Spread dai tre metodi
  ls_spread <- log(y_test) - ls$alpha_test - ls$beta_test*log(x_test)
  kf_spread <- log(y_test) - kf_alpha - kf_beta*log(x_test)
  rl_spread <- log(y_test) - rl$alpha - rl$beta*log(x_test)
  
  ############
  ######## PLOT SPREAD & SET THRESHOLD
  ############
  
  if(missing(threshold)){
    threshold <- round(ls$h_sd,2)
  }else{threshold <- threshold}
  
  if(missing(threshold_kf)){
    threshold_kf <- threshold/2
  }else{threshold_kf<-threshold_kf}
  
  lt_long <- lt_short <- kf_spread$spread
  lt_long[] <- -threshold
  lt_short[] <- threshold
  
  if(plot_spread==T){
    par(mfrow = c(3, 1))
    print(plot(kf_spread, main = "Kalman Filter Spread",col=c('red')))
    print(plot(ls_spread, main = "Co-integration Spread",col=c('black')))
    print(plot(rl_spread, main = "Rolling Regression Spread",col=c('blue')))
  }else{NULL}
  
  ############
  ############ PAIRS TRADING
  ###########
  
  # Pairs trading on Least Squares Spread
  ls_trading <- pairs_trading(yx_test,ls$alpha_test,ls$beta_test,name='Co-integration',
                threshold,plot=F,lag=F)
  
  # Pairs trading on Kalman Filter Spread
  kf_trading <- pairs_trading(yx_test,kf_alpha,kf_beta,
                              name='Kalman Filter',threshold_kf,plot=F,lag=T)
  
  # Pairs trading on Rolling Regression 2 months
  rl_trading <- pairs_trading(yx_test,rl$alpha,rl$beta,name='Rolling Regression',
                              threshold,plot=F,lag=T)
  
  ############
  #### PLOT SIGNALS
  #############
  
  if(plot_signals){
    par(mfrow=c(2,1))
    print(plot(kf_trading$signals,main='KF Signals'))
    print(plot(ls_trading$signals,main='LS Signals'))
  }else{NULL}
  
  
  ############
  ##### PERFORMANCE EVALUATION
  ############
  
  # estrai le strategie per intero
  ls_strategy <- ls_trading$strategy
  
  kf_strategy <- kf_trading$strategy
  
  rl_strategy <- rl_trading$strategy
  
  # Print and Plot Results

  final_metrics <- rbind(ls_trading$metrics,
                         kf_trading$metrics,
                         rl_trading$metrics)
  
  print(final_metrics)
  
  ###########
  # PLOT COMPARISON
  ###########
  
  if(plot_comparison){
  par(mfrow=c(1,1))
  print(plot(cumprod(1 + cbind(ls_trading$portfolio_returns,kf_trading$portfolio_returns,rl_trading$portfolio_returns)),
       legend.loc='topleft',main='Portfolio Comparison',grid.col=NA,col=c('black','red','blue')))
  }else{NULL}
  
  
  #############
  #### OUT
  ############
  out <- list(ls_strategy=ls_strategy,
              kf_strategy=kf_strategy,
              rl_strategy=rl_strategy)
  
  return(out)
}

comparison <- main(dataset='ftsemib_2015-2020.csv',
     assets=c('BMED.MI','AZM.MI'),
     cut_2020 = F,
     split_date = '2019-01-01',
     method='log',
     #####
     # Kalman Filter Parameters
     ####
     st_var=1e-5,
     ob_var =1e-3,
     init_var=1e-3,
     smooth_kf = 1,
     lag=F,
     # Threshold for LS & Rolling Least Square
     threshold = 0.01,
     # Threshold for Kalman Filter
     threshold_kf = 0.01,
     # Plot Choice
     plot_spread = T,
     plot_signals = F,
     plot_parameters = F,
     plot_comparison = T)





# Rolling 2 months strategy
rl <- comparison$rl_strategy
# Kalman Filter Strategy
kf <- comparison$kf_strategy
# Least Square Strategy
ls <- comparison$ls_strategy


plot(log(rl[,c(1,2)]))


##########
#### FAKE SOLUTION
#########
source('/Users/carmineminichini/Desktop/Kalman Filter/Cointegration Pairs Trading/fake_solution.R')

fake_rl <- fake_solution(rl,'2019-10-08','2019-10-07')

fake_ls <- fake_solution(ls,'2019-10-08','2019-10-07')

fake_kf <- fake_solution(kf,'2019-10-08','2019-10-07')

plot(cumprod(1+ cbind(fake_ls$fake_portfret,fake_rl$fake_portfret,fake_kf$fake_portfret)),legend.loc = 'topleft',main='Comparison')     


#########
#### PLOT PRICE
##########


plot(log(cbind(rl$DIA.MI,rl$AMP.MI*4.7)),legend.loc = 'topleft',main='Diasorin-Amplifon',
     col=c('black','blue'))




#########
####### PLOT SPREAD
#########

{ plot(cbind(kf$spread), main = "Kalman Filter Spread")
  lines(kf$threshold_short,lty = 2,lwd=2)
  lines(kf$threshold_long, lty = 2,lwd=2)}

{ plot(ls$spread, main = "Co-integration Spread")
  lines(ls$threshold_short, lty = 2,lwd=2)
  lines(ls$threshold_long, lty = 2,lwd=2)}

{ plot(rl$spread, main = "Rolling regression Spread")
  lines(rl$threshold_short, lty = 2,lwd=2)
  lines(rl$threshold_long, lty = 2,lwd=2)}




par(mfrow = c(3, 1))
{ plot(kf$spread, main = "Kalman Filter Spread")
  lines(kf$threshold_short, lty = 1)
  lines(kf$threshold_long, lty = 1)}
{ plot(ls$spread, main = "Co-integration Spread")
  lines(ls$threshold_short, lty = 1)
  lines(ls$threshold_long, lty = 1)}
{ plot(rl$spread, main = "Rolling regression Spread")
  lines(rl$threshold_short, lty = 1)
  lines(rl$threshold_long, lty = 1)}


###########
###### External Analysis
###########
library(dplyr)

rl_df <- data.frame(rl)

kf_df <- data.frame(kf)

ls_df <- data.frame(ls)

rl_df %>%
  group_by(signal) %>%  count()

ls_df %>% 
  group_by(signal) %>% count()

kf_df %>% 
  group_by(signal) %>% count()
