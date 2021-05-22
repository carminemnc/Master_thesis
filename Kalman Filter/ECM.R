ecm <- function(dataset,assets,cut_2020,method,split_date,run_on){
  #library(tidyverse)
  #library(tseries)
  #library(quantmod)
  #library(readxl)
  #library(urca)
  library(xts)
  library(apt)
  
  setwd("/Users/carmineminichini/Desktop/Kalman Filter/data")
  test_df <- read.csv(dataset)
  
  assets <- assets
  
  # converto in xts
  real <- xts(test_df[,assets],order.by = as.POSIXct(test_df[,1]))
  
  # escludo il 2020
  if(cut_2020){
    real <- real['::2020-01-01']
  }
  else{NULL}
  
  ########################  SE METODO LOG
  if(method=='log')
  {
    for(i in 1:ncol(real))
    {
      real[,i] <- log(real[,i])
    }
    real <- na.omit(real)
    print('Cointegrazione calcolata sui log-prezzi')
    print("___________________________________________")
  }
  else{NULL}
  
  # SOLO TRAIN
  train_real <- real[paste0('::',split_date)]
  
  # Test per il plot
  test_real <- real[paste0(split_date,'::')]
  
  if(run_on=='train'){
    data <- train_real
  }else{data<-test_real}
  
  data <- ts(data,start=1950,freq=1)
  
  fit <- ecmAsyFit(data[,assets[1]], data[,assets[2]], lag = 1, split = TRUE, model = "linear", thresh = 0)
  summary(fit)
  
}


ecm('ftsemib_2015-2021.csv',
    assets=c('UCG.MI','BPE.MI'),
    cut_2020=T,
    method='log',
    split_date='2017-01-01',
    run_on='test')



