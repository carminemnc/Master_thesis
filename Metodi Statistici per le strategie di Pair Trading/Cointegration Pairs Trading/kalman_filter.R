kalman_filter <- function(x,y,alpha,beta,method='log',st_var=1e-5,ob_var=1e-3,init_var=1e-3,smooth)
{
  library(KFAS)
  time <- nrow(x)
  
  if(method=='log'){
    x <- log(x)
    y <- log(y)
  }else{NULL}
  
  alpha <- as.numeric(alpha[1])
  beta <- as.numeric(beta[1])
  
  # init empty variables
  beta_Kalman_filtering <- alpha_Kalman_filtering <- xts(rep(NA, time), index(x))
  colnames(alpha_Kalman_filtering) <- "Alpha Kalman Filter"
  colnames(beta_Kalman_filtering) <- "Beta Kalman Filter"
  
  # Kalman parameters
  Tt <- diag(2)
  Rt <- diag(2)
  Qt <- st_var*diag(2)  # state transition variance very small
  Zt <- array(as.vector(t(cbind(1, as.matrix(x)))), dim = c(1, 2, time))  # time-varying
  Ht <- matrix(ob_var)  # observation variance
  
  a1 <- matrix(c(alpha[1], beta[1]), 2, 1) 
  P1 <- init_var*diag(2)  # variance of initial point
  P1inf <- 0*diag(2)
  
  # create Kalman model
  model <- SSModel(as.matrix(y) ~ 0 + SSMcustom(Z=Zt, T=Tt, R=Rt, Q=Qt, a1=a1, P1=P1, P1inf=P1inf), H=Ht)
  
  # run Kalman filtering
  out <- KFS(model)
  alpha_Kalman_filtering[] <- out$a[-1, 1]  # a is Kalman filtering (alphahat is Kalman smoothing) (a(T+1)=alphahat(T))
  beta_Kalman_filtering[] <- out$a[-1, 2]
  
  # smoothing
  L <- smooth
  alpha_Kalman_filtering[] <- stats::filter(alpha_Kalman_filtering, rep(1, L)/L, sides = 1)
  alpha_Kalman_filtering <- na.locf(alpha_Kalman_filtering, fromLast = TRUE)
  beta_Kalman_filtering[] <- stats::filter(beta_Kalman_filtering, rep(1, L)/L, sides = 1)
  beta_Kalman_filtering <- na.locf(beta_Kalman_filtering, fromLast = TRUE)

  out  <- list(alpha=alpha_Kalman_filtering,
             beta = beta_Kalman_filtering,
             model=out)

  return(out)
}
