compute_spread <- function(data, alpha, beta, name = NULL,method='log') {
  
 if(method=='log'){
   data <- log(data)
 }else{NULL}
  
  w_spread <- cbind(1, - beta)/cbind(1+beta, 1+beta)
  spread <- rowSums(data* w_spread) - alpha/(1+beta)
  colnames(spread) <- name
  
  out <- list(spread = spread,
              w_spread = w_spread)
  return(out)
}
