performance_metrics <- function(y,x,alpha,beta,portf_returns,name,root=T)
{
  
  if(root){
    y_true <- y
    y_hat <- alpha + beta*log(x)
    mse <-  sqrt(mean((y_hat - log(y_true))^2))
  } else{
    y_true <- y
    y_hat <- alpha + beta*log(x)
    mse <-  mean((y_hat - log(y_true))^2)
  }

  v <- cumprod(1 + portf_returns)
  
  spread <- log(y) - alpha - beta*log(x)
  #####
  ##### METRICS
  #####
  
  # Sharpe Ratio Annualized
  sharpe_ratio_ann <- (mean(portf_returns)/sd(portf_returns))*sqrt(252)
  
  # Max Drawdown
  max_drawdown <- max(1 - v/cummax(v)) 
  
  # average returns annualized
  avg_returns_ann  <- prod(1 + portf_returns) - 1
  
  
  # Modified sharpe ratio
  m_sharpe_ratio <- (mean(portf_returns)/mse)*sqrt(252)
  
  # VaR
  VaR <- - VaR(portf_returns,p=.99,method='gaussian')*sqrt(252)
  # Expected Shortfall
  ES <- - ES(portf_returns,p=.99,method = 'gaussian')*sqrt(252)
  
  # VaR Sharpe Ratio
  sr_var <- (mean(portf_returns)/ES)*sqrt(252)
  
  # ADF test
  adf_statistic <- adf.test(spread)$statistic
  p_value_adf <- adf.test(spread)$p.value
  
  # PP TEST
  pp_statistic <- pp.test(spread)$statistic
  p_value_pp <- pp.test(spread)$p.value
  
  # Drawdowns Table
  drawdowns_results <- table.Drawdowns(portf_returns)
  drawdowns_results <- drawdowns_results %>% arrange(From) %>% select(c(1,3,4,5))
  
  metrics <- cbind(sharpe_ratio_ann,
                   max_drawdown,
                   avg_returns_ann,
                   VaR,
                   ES,
                   m_sharpe_ratio,
                   sr_var,
                   adf_statistic,
                   p_value_adf,
                   mse,
                   pp_statistic,
                   p_value_pp)
  
  colnames(metrics) <- c('Annualized Sharpe Ratio',
                         'MaxDrawdown',
                         'Average Returns Annualized',
                         'VaR',
                         'ES',
                         'Sharpe Ratio (RMSE)',
                         'Sharpe Ratio(ES)',
                         'ADF Statistic',
                         'p_value_adf',
                         'RSME',
                         'PP Statistic',
                         'p_value_pp')
  rownames(metrics) <- name
  
  tickers_name <- paste0(colnames(y),'-',colnames(x))
  
  write.csv(drawdowns_results,file=paste0('/Users/carmineminichini/Desktop/Kalman Filter/output/',
                                          tickers_name,
                                          '_',
                                          name,
                                          '_drawdown.csv'),row.names = F)
  
  return(metrics)
}


