generate_Z_score_EMA <- function(spread, n =20) {
  library(TTR)
  ## traditional rolling windowed mean and variance
  # first, the mean
  spread.mean <- EMA(spread, n)
  spread.mean <- na.locf(spread.mean, fromLast = TRUE)
  spread.demeaned <- spread - spread.mean
  # second, the variance
  spread.var <- EMA(spread.demeaned^2, n)
  spread.var <- na.locf(spread.var, fromLast = TRUE)
  # finally compute Z-score
  Z.score <- spread.demeaned/sqrt(spread.var)
  return(Z.score)
}
