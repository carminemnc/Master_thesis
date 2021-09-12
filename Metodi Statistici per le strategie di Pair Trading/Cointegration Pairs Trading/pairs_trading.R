pairs_trading <- function(data, alpha, beta, name = NULL, threshold,plot = FALSE,lag=F) {
  
  source('/Users/carmineminichini/Desktop/Kalman Filter/Cointegration Pairs Trading/performance_metrics.R')
  
  print(paste(name,"strategy evaluated with a threshold of +-",threshold))
  # Y
  y <- data[,1]
  # X
  x <- data[,2]
  
  # Pesi per il portafoglio
  w_spread <- cbind(1, -beta)/cbind(1+beta, 1+beta)
  
  real_wspread <- cbind(1, -beta)/cbind(1+beta, 1+beta)
  colnames(real_wspread) <- paste(colnames(data),'real_weight_spread')
  
  if(lag){
    w_spread <- lag.xts(w_spread)
    colnames(w_spread) <- paste(colnames(data),'weight_spread')
  }
  
  # Spread
  spread <- log(y) - alpha - beta*log(x)
  colnames(spread) <- 'spread'
  
  # Z-score==SPREAD
  Z_score <- spread
  threshold_long <- threshold_short <- Z_score
  threshold_short[] <- threshold
  threshold_long[] <- -threshold
  colnames(threshold_short) <- 'threshold_short'
  colnames(threshold_long) <- 'threshold_long'
  
  # Genera i Segnali di trading
  signal <- generate_signal(Z_score, threshold_long, threshold_short)
  
  # Combina i pesi del portafoglio * segnali
  w_portf <- w_spread * lag.xts(cbind(signal, signal)) 
  colnames(w_portf) <- paste(colnames(data),'weight')
  
  # now compute the PnL from the log-prices and the portfolio
  
  X <- diff(log(data))  #compute log-returns from log-prices
  colnames(X) <- paste(colnames(data),'returns')
  
  # Gains
  gains <- X * w_portf
  colnames(gains) <- paste(colnames(data),'gains')
  
  # Portfolio Returns
  portf_return <- xts(rowSums(X * w_portf), index(X))
  portf_return[is.na(portf_return)] <- 0
  colnames(portf_return) <- name
  
  ################ INUTILIZZATO
  # plots
  if (plot) {
    par(mfrow = c(3, 1))
    
    { plot(Z_score, legend.loc = "topleft",
           main = paste("Z-score and trading on spread based on", name))
      lines(threshold_short, lty = 2)
      print(lines(threshold_long, lty = 2)) }
    print(plot(signal))
    print(plot(cumprod(1+ portf_return), main = paste("Cum P&L for spread based on", name)))
  }
  ################
  
  
  metrics <- performance_metrics(y,x,alpha,beta,portf_return,name,root=T)

  strategia <- cbind(y,x,alpha,beta,
                     Z_score,threshold_long,
                     threshold_short,signal,
                     real_wspread,w_spread,
                     w_portf,X,gains,
                     portf_return)
  
  out <- list(portfolio_returns= portf_return,
              signals = signal,
              strategy = strategia,
              metrics = metrics)
  return(out)
}
