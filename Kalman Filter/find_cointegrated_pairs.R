find_cointegrated_pairs <- function(dataset,cut_2020=F,method,split_date,plot=FALSE)
{
  library(tidyverse)
  library(tseries)
  library(quantmod)
  library(readxl)
  library(urca)
  
  setwd("/Users/carmineminichini/Desktop/Kalman Filter/data")
  test_df <- read.csv(dataset)
  cost <- read_excel('ftsemib_cost.xlsx')
  
  # converto in xts
  real <- xts(test_df[,-ncol(test_df)],order.by = as.POSIXct(test_df[,ncol(test_df)]))
  
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
  else{
    NULL
  }
  
  ######## 
  ########
  # SOLO TRAIN
  train_real <- real[paste0('::',split_date)]
  
  # Test per il plot
  test_real <- real[paste0(split_date,'::')]
  
  print(paste('Verifico la  cointegrazione nel periodo:',head(index(train_real),1),
              '-',tail(index(train_real),1)))
  print('___________________________________________')

  # nomi dei tickers
  tickers <- colnames(train_real)
  
  y <-NULL
  x <-NULL
  correlation<-NULL
  beta<-NULL
  pvalue_adftest <-NULL
  pvalue_pptest <- NULL
  adf_st <- NULL
  
  # numero di correlazioni da calcolare
  n_correlations <- (ncol(real)^2-ncol(real))/2
  
  # cosa vuoi usare?
  data <- train_real
  
  for (i in 1:length(tickers)) {
    for (j in 1:length(tickers)) {
      
      if (i>j) {
        # SINISTRA / DESTRA
        y <-c(y, tickers[i])
        x <-c(x, tickers[j])
        
        # Correlazioni sui prezzi originali
        correlation<-c(correlation, cor(data[,tickers[i]], data[,tickers[j]]))
        
        # regressione lineare con intercetta
        m <-lm(data[,tickers[i]] ~ data[,tickers[j]])

        # beta
        beta<-c(beta, as.numeric(coef(m)[2]))
        
        # spread
        spread <- residuals(m)
        
        # adf test
        pvalue_adftest<-c(pvalue_adftest, adf.test(spread)$p.value)
        
        adf_st <- c(adf_st,adf.test(spread)$statistic)
        # pp test
        pvalue_pptest <-c(pvalue_pptest,pp.test(spread)$p.value)
      }
    }
    
  }
  
  # risultato dell'analisi
  df<-data.frame(y, x, correlation, beta, adf_st,pvalue_adftest,pvalue_pptest)
  
  # filtra per il p-value dell'adf test
  out_df <- df %>% filter(pvalue_adftest < 0.05) %>% arrange(-correlation)
  
  # aggiungi settori
  out_df$sector_leftside <- cost$Settore[match(out_df$y,cost$Ticker)]
  out_df$sector_rightside <- cost$Settore[match(out_df$x,cost$Ticker)]
  
  # filtra per settori uguali
  out_df <- out_df[which(out_df$sector_leftside==out_df$sector_rightside),]
  
  if(plot){
    f_ticker <- out_df$y[1]
    s_ticker <- out_df$x[1]
    pl <- plot(train_real[,c(f_ticker,s_ticker)],main=paste(f_ticker,'-',s_ticker,'Train Period'),
               legend.loc='topright')
    pl_2 <- plot(test_real[,c(f_ticker,s_ticker)],main=paste(f_ticker,'-',s_ticker,'Test Period'),col=11:12,
                 legend.loc='topright')
    par(mfrow = c(2, 1))
    plot(pl)
    plot(pl_2)
  }
  
  # stampa l'output
  print(out_df)
  
  return(out_df)
}


out_df <- find_cointegrated_pairs("ftsemib_2015-2020.csv",cut_2020=F,
                                  method='log',
                                  split_date = '2019-01-01',plot = F)
i
