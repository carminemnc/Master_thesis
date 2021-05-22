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
  
  #####
  ##### METRICS
  #####
  
  # Sharpe Ratio Annualized
  sharpe_ratio_ann <- (mean(portf_returns)/sd(portf_returns))*sqrt(252)
  
  # Max Drawdown
  max_drawdown <- max(1 - v/cummax(v)) 
  
  # average returns annualized
  avg_returns_ann  <- mean(portf_returns)*252
  
  # Modified sharpe ratio
  m_sharpe_ratio <- (mean(portf_returns)/mse)*sqrt(252)
  
  
  metrics <- cbind(sharpe_ratio_ann,max_drawdown,avg_returns_ann,mse,m_sharpe_ratio)
  
  colnames(metrics) <- c('Annualized Sharpe Ratio',
                         'MaxDrawdown',
                         'Average Returns Annualized',
                         'RMSE',
                         'Modified Sharpe Ratio')
  rownames(metrics) <- name
  
  return(metrics)
}


