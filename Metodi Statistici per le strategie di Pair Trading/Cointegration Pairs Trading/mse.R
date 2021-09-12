mse <- function(y_test,x_test,alpha,beta,name=NULL)
{
  y_true <- y_test
  y_hat <- alpha + beta*log(x_test)
  
  
  # if(root){
  #   rmse <-  sqrt(mean((y_hat - log(y_true))^2))
  #   
  #   print(paste("RMSE for",name,":",rmse))
  # }else{
  #   mse <-  mean((y_hat - log(y_true))^2)
  #   
  #   print(paste("MSE for",name,":",mse))
  # }

  # dataframe of actual and preedicted
  pred_act <- cbind(y_hat,log(y_true))
  
  y_hat_NAME <- paste0('y_hat',"_",name)
  y_true_NAME <- paste0('y_true',"_",name)
  colnames(pred_act) <- c(y_hat_NAME,y_true_NAME)
  
  return(pred_act)
}
