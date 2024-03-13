#' Clustered statistics from glm with link function of logit
#' 
#' @param mdl a model object from glm package for logistic regression.
#' @cluster a column vector for cluster id
#' @returns vcov a numeric variance covariance matrix




vcov.robust <- function(mdl, cluster){
  # initiate variance covariance matrix to return
  vcov <- NULL
  
  # get x, y, b from the model object
  if(any(class(mdl)%in%c("glm", "lm") ) ){
    # find b
    b <- mdl$coefficients
    # find x
    mdl$data$`(Intercept)` <- 1
    x <- mdl$data[,names(b)]
    # find y 
    y <- mdl$y
    # prepare c
    c <- cluster
    if(length(c)!=length(y)){
      stop("cluster length and number of rows in modeling data differ!")
    }
  }
  
  vcov <- vcov.robust.xy(x,y,b,c)$cHScov
  
  return(list(vcov = vcov))
}
