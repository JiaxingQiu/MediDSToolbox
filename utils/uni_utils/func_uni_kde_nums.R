uni_kde_nums <- function(data, num_cols, pct=TRUE){
  
  df_result_all = data.frame()
  for(num_col in num_cols){
   
    if(pct){
      x <- est_pctl(data[,num_col])
    }else{
      x <- data[,num_col]
      x[which( x<0 | x>1)]<- NA
    }
    den <- density(x[!is.na(x)])
    df_result_all <- bind_rows(df_result_all, data.frame(
      pctl=round((den$x-min(den$x))/(max(den$x)-min(den$x)),2), 
      density_scaled=round((den$y-min(den$y))/(max(den$y)-min(den$y)),2),
      var_name=num_col
    ))
  }
  return(df_result_all)
}

