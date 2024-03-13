NIC <- function(mdl){
  # input of rms model object
  x <- as.matrix(mdl$x)
  y <- mdl$y
  b <- coef(mdl)
  c <- mdl$c
  
  x <- cbind(Intercept = rep(1,nrow(x)), x)
  x <- x[,intersect(colnames(x), names(b))]
  b <- b[intersect(colnames(x), names(b))]
  
  nic_obj <- vcov.robust.xy(x,y,b,c)
  nic <- nic_obj$nic
  aic <- nic_obj$aic
  return(list("nic"=nic,
              "aic"=aic))
}
