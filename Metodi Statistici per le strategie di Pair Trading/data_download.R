data_download <- function(from,to){
  library(tidyverse)
  library(tseries)
  library(quantmod)
  library(readxl)
  
  setwd('~/Desktop/Kalman Filter/data')
  
  tickers <- read_excel('ftsemib_cost.xlsx')$Ticker
  
  myStocks <-lapply(tickers, function(x) {getSymbols(x, 
                                                     from = from, 
                                                     to = to,
                                                     periodicity = "daily",
                                                     auto.assign=FALSE)} )
  
  # rinomina
  names(myStocks)<- tickers
  # estrai Adjusted prices
  closePrices <- lapply(myStocks, Ad)
  # Merge
  closePrices <- do.call(merge, closePrices)
  # Elmina Adjusted
  names(closePrices)<-sub("\\.Adjusted", "",names(closePrices))
  # tickers con NA
  columns_withNA <- colnames(closePrices)[ apply(closePrices, 2, anyNA) ]
  # elimina i tickers che hanno gli NA
  finale <- closePrices[,!colnames(closePrices) %in% columns_withNA]
  
  finale <- data.frame(finale)
  finale$date <- rownames(finale)
  filename <- paste0("~/Desktop/Kalman Filter/data/ftsemib_",substr(from,1,4),"-",substr(to,1,4),".csv")
  
  write.csv(finale,filename,row.names = F)
}


data_download(from='2015-01-01',to="2020-01-01")



