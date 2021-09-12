rolling_2month <- function(x_train,y_train,x_test,y_test)
  {
  train_reg <- lm(log(y_train)~log(x_train))
  
  alpha <- train_reg$coefficients[1]
  beta <- train_reg$coefficients[2]
  
  steps <- seq(1,nrow(x_test)-40,40)
  
  alpha_ <- data.frame()
  beta_ <- data.frame()
  dates_ <- data.frame()
  for(t in steps)
  {
    # print(paste("Nel periodo",index(x_test[t]),index(x_test[t+39])))
    # Date
    dates <- paste0(index(x_test[t]),"::",index(x_test[t+39]))
    dates_ <- rbind(dates_,dates)
    # Regressione
    reg_ <- lm(log(y_test[t:(t+40)]) ~ log(x_test[t:(t+40)]))
    #print(adf.test(reg_$residuals,k=1))
    # alpha
    alpha_ <- rbind(alpha_,reg_$coefficients[1])
    # beta
    beta_ <- rbind(beta_,reg_$coefficients[2])
  }
  
  ######
  
  
  beta_0 <- alpha_0 <- y_test[dates_[1,1]]
  beta_0[] <- beta
  alpha_0[] <- alpha
  
  beta_1 <- alpha_1 <- y_test[dates_[2,1]]
  beta_1[] <- beta_[1,1]
  alpha_1[] <- alpha_[1,1]
  
  beta_2 <- alpha_2 <- y_test[dates_[3,1]]
  beta_2[] <- beta_[2,1]
  alpha_2[] <- alpha_[2,1]
  
  beta_3 <- alpha_3 <- y_test[dates_[4,1]]
  beta_3[] <- beta_[3,1]
  alpha_3[] <- alpha_[3,1]
  
  beta_4 <- alpha_4 <- y_test[dates_[5,1]]
  beta_4[] <- beta_[4,1]
  alpha_4[] <- alpha_[4,1]
  
  beta_5 <- alpha_5 <- y_test['2019-10-15::']
  beta_5[] <- beta_[5,1]
  alpha_5[] <- alpha_[5,1]
  
  
  beta_rl <- rbind(beta_0,beta_1,beta_2,beta_3,beta_4,beta_5)
  colnames(beta_rl) <- 'Beta Rolling Regression'
  alpha_rl <- rbind(alpha_0,alpha_1,alpha_2,alpha_3,alpha_4,alpha_5)
  colnames(alpha_rl) <- 'Alpha Rolling Regression'
  
  
  out <- list(beta=beta_rl,alpha=alpha_rl)
  return(out)
}
