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
