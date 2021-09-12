alpha_beta_LS<- function(x_train,y_train,method='log',test)
{
  
  lunghezza_train <- nrow(x_train)
 
  # REGRESSIONE
  if(method=='log')
  {
    #print('Regressione effettuata sui log-prezzi')
    regression <- lm(log(y_train) ~ log(x_train))
  }
  else
  {
    regression <- lm(y_train ~ x_train)
  }
  
  ##########
  ########## Alpha & beta per la lunghezza del train
  alpha_train <- xts(rep(NA,lunghezza_train),order.by=index(x_train))
  beta_train <- xts(rep(NA,lunghezza_train),order.by=index(x_train))
  
  beta_train[] <- as.numeric(regression$coefficients[2])
  
  alpha_train[] <- as.numeric(regression$coefficients[1])
  
  
  
  ##########
  ######### Alpha & beta per la lunghezza del test
  alpha_test <- test
  beta_test <- test
  
  beta_test[] <- as.numeric(regression$coefficients[2])
  

  alpha_test[] <- as.numeric(regression$coefficients[1])
  
  
  # Historical standard deviation
  h_sd <- sd(regression$residuals)
  
  # OUT
  out <- list(alpha_train=alpha_train,
              beta_train=beta_train,
              alpha_test=alpha_test,
              beta_test=beta_test,
              h_sd=h_sd)
  
  return(out)
}
