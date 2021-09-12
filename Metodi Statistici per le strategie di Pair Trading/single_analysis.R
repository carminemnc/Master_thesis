analysis <- function(dataset,assets,split_date,method)
  {
  ############ LETTURA DEI DATI
  path <- paste0('/Users/carmineminichini/Desktop/Kalman Filter/data/',dataset)
  
  # Leggi
  df <- read.csv(path)
  
  # stocks da considerare 
  assets <- assets
  # in xts
  x <- xts(df[,assets],order.by=as.POSIXct(df[,ncol(df)]))
  
  # splits date
  split_date <- split_date
  split_train <- paste0('::',split_date)
  

  
  x_train <- x[split_train]
  
  print(paste('Analisi dei titoli nel',
              head(index(x_train),1),
              '-',tail(index(x_train),1)))
  print('___________________________________________')
  
  if(method=='log')
  {
    for(i in 1:ncol(x_train))
    {
      x_train[,i] <- log(x_train[,i])
    }
    x_train <- na.omit(x_train)
    print('Metodo log')
    print("___________________________________________")
  }
  else{
    NULL
  }  
  
  ticker <- NULL
  adf_st <- NULL
  adf_pvalue <-NULL
  adf_st_diff <- NULL
  adf_pvalue_diff <- NULL
  
  for(i in 1:length(assets)) {
    ticker <- c(ticker,assets[i])
    
    adf_st <- c(adf_st,adf.test(x_train[,assets[i]])$statistic)
    
    adf_pvalue <- c(adf_pvalue , adf.test(x_train[,assets[i]])$p.value  )
    
    adf_st_diff <- c(adf_st_diff, adf.test(na.omit(diff(x_train[,assets[i]])))$statistic )
    
    
    adf_pvalue_diff <- c(adf_pvalue_diff, adf.test(na.omit(diff(x_train[,assets[i]])))$p.value)
    

    #print(plot(na.omit(diff(x_train[,assets[i]]))))
  }
  
  data_an <- data.frame(ticker,round(adf_st,2),round(adf_pvalue,2),round(adf_st_diff,2),round(adf_pvalue_diff,2))
  
  colnames(data_an) <- c('Tickers',
                         'ADF Statistic',
                         'p-value',
                         'ADF Statistic',
                         'p-value')
  

  print(data_an)
  
  write.csv(data_an,file=paste0('/Users/carmineminichini/Desktop/Kalman Filter/output/',
                        'analisi_titoli',
                        method,
                        '.csv'),row.names = F)
  
}


analysis("ftsemib_2015-2020.csv",
         assets=c("DIA.MI",'AMP.MI','UCG.MI','BPE.MI','BMED.MI','BGN.MI','AZM.MI'),
         '2019-01-01',
         method='log')



