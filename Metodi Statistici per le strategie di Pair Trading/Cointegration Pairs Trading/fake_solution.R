fake_solution <- function(strategy,after,before)
{
  
  source('/Users/carmineminichini/Desktop/Kalman Filter/Cointegration Pairs Trading/performance_metrics.R')
  after <- paste0(after,'::')
  before <- paste0('::',before)
  
  fake_portf_returns <- strategy[,11][after]*strategy[,15][after]
  tot_fake_portf_returns <- rbind(strategy[,ncol(strategy)][before],fake_portf_returns)
  
  y <- strategy[,1]
  x <- strategy[,2]
  alpha <- strategy[,3]
  beta <- strategy[,4]
  name <- colnames(strategy[,ncol(strategy)])
  metrics <- performance_metrics(y,x,alpha,beta,tot_fake_portf_returns,name,root=T)
  
  print(metrics)
  
  out <- list(fake_portfret= tot_fake_portf_returns)
  
  return(out)
}