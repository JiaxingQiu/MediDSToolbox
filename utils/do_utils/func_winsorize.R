winsorize <- function(df){
  # winzorize all the numeric columns in a dataframe object
  x = df
  for (i in which(sapply(x, is.numeric))) {
    quantiles <- quantile( x[,i], c(.001, .999 ), na.rm =TRUE)
    x[,i] = ifelse(x[,i] < quantiles[1] , quantiles[1], x[,i])
    x[,i] = ifelse(x[,i] > quantiles[2] , quantiles[2], x[,i])
  }
  return(x)
}
