# transformation functions that could be used on a randome variable

# estimate the percentile of a population based on a certain numeric value
est_pctl <- function(num_vec){
  
  res_pctl <- NULL
  
  try({
    res_rank <- base::rank(num_vec,na.last = "keep")
    res_pctl <- (res_rank - min(res_rank,na.rm=TRUE))/(max(res_rank,na.rm=TRUE) - min(res_rank,na.rm=TRUE))
  },TRUE)
  
  if (is.null(res_pctl)){
    num_vec <- as.numeric(as.vector(num_vec))
    num_scaled <- (num_vec-min(num_vec,na.rm=TRUE))/(max(num_vec,na.rm=TRUE)-min(num_vec,na.rm=TRUE))
    res_pctl <- rep(NA, length(num_vec))
    cuts <- seq(0,1,0.01)
    scaler <- data.frame(qt=as.vector(quantile(num_scaled,cuts,na.rm=TRUE)),pt=cuts)
    for (i in 2:nrow(scaler)){
      res_pctl[which(num_scaled>=scaler$qt[i-1] & num_scaled<scaler$qt[i])]<-scaler$pt[i-1]
    }
  }
 
  return(res_pctl)
}


# estimate a pseudo normal distribution of a random variable by rankit 
rankit <- function(x){
  xx <- qnorm((rank(x)-0.5)/sum(!is.na(x)),mean=mean(x,na.rm=TRUE),sd=sd(x,na.rm = TRUE))
  return(xx)
}

# robust scaler
robust_scale <- function(x){
  xx <- (x - median(x,na.rm = TRUE)) / (quantile(x,0.75,na.rm=TRUE)-quantile(x,0.25,na.rm=TRUE))
}

# normal (standardize) scaler
normal_scale <- function(x){
  xx <- (x - mean(x,na.rm = TRUE)) / sd(x,na.rm = TRUE)
}
