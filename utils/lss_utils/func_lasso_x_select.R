library(glmnet)
library(vip)

lasso_x_select <- function(
  data, 
  y_col,
  x_cols_nonlin_rcs5,
  x_cols_nonlin_rcs4,
  x_cols_nonlin_rcs3,
  x_cols_linear=x_cols_linear, 
  x_cols_fct=x_cols_fct,
  x_cols_tag=x_cols_tag,
  family = c("binomial", "multinomial", "gaussian")[1],
  standardize = TRUE,
  dict_data=NULL, # dictionary table is optional
  lambda=c("auto","1se","min")[1],
  lambda_value = NULL # external specified lasso lambda value 
){
  
  # ---- Description ----
  # dictionary oriented (do) predictor variables selection
  
  # ---- Arguments ----
  # data: a dataframe object with essential dictionary information as attributes on each column
  # dict: dictionary for corresponding data frame input
  # x_cols: pre-selected predictor column names (x)
  
  # ---- Value ----
  # data_final: a dataframe object with updated redundency attributes on each column
  # dict_final: updated dictionary for correspondingdata_final
  
  
  # linear > rcs3 > rcs4 > rcs5
  # variable name population space
  if(!is.null(dict_data)) {
    input_cols_choices <- intersect(colnames(data), dict_data$varname[which(dict_data$mlrole=="input"&dict_data$type=="num")])
  } else {
    input_cols_choices <- colnames(data)
  }
  # organize model formula string
  x_cols_linear <- intersect(x_cols_linear, input_cols_choices)
  x_cols_nonlin_rcs3 <- setdiff(x_cols_nonlin_rcs3, x_cols_linear) # remove linear from rcs3
  x_cols_nonlin_rcs3 <- intersect(x_cols_nonlin_rcs3, input_cols_choices)# make sure rcs 3 all come from numeric columns 
  x_cols_nonlin_rcs4 <- setdiff(setdiff(x_cols_nonlin_rcs4, x_cols_linear),x_cols_nonlin_rcs3)
  x_cols_nonlin_rcs4 <- intersect(x_cols_nonlin_rcs4, input_cols_choices)
  x_cols_nonlin_rcs5 <- setdiff(setdiff(setdiff(x_cols_nonlin_rcs5, x_cols_linear),x_cols_nonlin_rcs3),x_cols_nonlin_rcs4)
  x_cols_nonlin_rcs5 <- intersect(x_cols_nonlin_rcs5, input_cols_choices) # now we successfully split the rcs knots groups
  x_cols <- unique(c(x_cols_linear, x_cols_nonlin_rcs3, x_cols_nonlin_rcs4, x_cols_nonlin_rcs5,x_cols_tag))
  
  data_org <- data
  
  if(standardize) {
    data[,x_cols] <- scale(data_org[,x_cols])
    data[,x_cols_tag] <- data_org[,x_cols_tag] # overwrite dummy columns, they should not be scaled
  }
  # additional columns
  for (col in x_cols_nonlin_rcs5){
    data[,col] <- as.numeric(rcs(data[,col],5)[,1])
    data[,paste0(col,"'")] <- as.numeric(rcs(data[,col],5)[,2])
    data[,paste0(col,"''")] <- as.numeric(rcs(data[,col],5)[,3])
    data[,paste0(col,"'''")] <- as.numeric(rcs(data[,col],5)[,4])
    x_cols <- c(x_cols, paste0(col,"'"), paste0(col,"''"),paste0(col,"'''"))
  }
  for (col in x_cols_nonlin_rcs4){
    data[,col] <- as.numeric(rcs(data[,col],4)[,1])
    data[,paste0(col,"'")] <- as.numeric(rcs(data[,col],4)[,2])
    data[,paste0(col,"''")] <- as.numeric(rcs(data[,col],4)[,3])
    x_cols <- c(x_cols, paste0(col,"'"), paste0(col,"''"))
  }
  for (col in x_cols_nonlin_rcs3){
    data[,col] <- as.numeric(rcs(data[,col],3)[,1])
    data[,paste0(col,"'")] <- as.numeric(rcs(data[,col],3)[,2])
    x_cols <- c(x_cols, paste0(col,"'"))
  }
  for (col in x_cols_fct){
    levels <- unique(as.character(data[,col]))
    for(l in levels){
      data[,paste0(col,"___",l)] <- ifelse(data[,col]==l,1,0)
      x_cols <- c(x_cols,paste0(col,"___",l))
    }
  }
  
  x_cols <- intersect(x_cols, colnames(data))
  x <- data.matrix(data[complete.cases(data[,c(x_cols,y_col)]),x_cols])
  y <- data.matrix(data[complete.cases(data[,c(x_cols,y_col)]),y_col])
  
  # ---- cv lasso an ridge regression ---
  lasso_cv <- glmnet::cv.glmnet(x=x, y=y, family=family, nfolds = 10, nlambda=100, alpha = 1, standardize = FALSE)
  ridge_cv <- glmnet::cv.glmnet(x=x, y=y, family=family, nfolds = 10, nlambda=100, alpha = 0, standardize = FALSE)
  # # plot results
  # par(mfrow = c(2, 2))
  # plot(lasso_cv, main = "Lasso penalty\n\n")
  # plot(ridge_cv, main = "Ridge penalty\n\n")
  # 
  # ----- show panalization trace -----
  lasso_trace <- glmnet::glmnet(x=x, y=y, family=family, alpha = 1, standardize = FALSE)
  ridge_trace <- glmnet::glmnet(x=x, y=y, family=family, alpha = 0, standardize = FALSE)
  # # plot optimal models
  # plot(lasso_trace, xvar = "lambda", main = "Lasso penalty\n\n")
  # abline(v = log(lasso_cv$lambda.min), col = "red", lty = "dashed")
  # abline(v = log(lasso_cv$lambda.1se), col = "blue", lty = "dashed")
  # plot(ridge_trace, xvar = "lambda", main = "Ridge penalty\n\n")
  # abline(v = log(ridge_cv$lambda.min), col = "red", lty = "dashed")
  # abline(v = log(ridge_cv$lambda.1se), col = "blue", lty = "dashed")
  # 
  #  ----- train optimal lambda models ----
  opt_lambda_lasso <- lasso_cv$lambda.1se
  opt_lambda_ridge <- ridge_cv$lambda.1se
  if(lambda=="min") {
    opt_lambda_lasso <- lasso_cv$lambda.min
    opt_lambda_ridge <- ridge_cv$lambda.min
  }
  if(!is.null(lambda_value)){
    opt_lambda_lasso <- lambda_value
  }
  lasso_optimal <- glmnet::glmnet(x=x, y=y, family=family, alpha = 1, standardize = FALSE, lambda = opt_lambda_lasso)
  ridge_optimal <- glmnet::glmnet(x=x, y=y, family=family, alpha = 1, standardize = FALSE, lambda = opt_lambda_ridge)
  # plot variable importance (coefficients) on final model obj
  # vip::vip(lasso_optimal, horizontal = TRUE, geom = "point", include_type=TRUE)
  # vip::vip(ridge_optimal, horizontal = TRUE, geom = "point", include_type=TRUE)
  #print(lasso_optimal)
  #summary(lasso_optimal)
  return(list( cv_mdls = list(lasso_cv = lasso_cv, ridge_cv = ridge_cv),
               trace_mdls = list(lasso_trace = lasso_trace, ridge_trace = ridge_trace),
               optimal_mdls = list(lasso_optimal=lasso_optimal, ridge_optimal=ridge_optimal) ))
  
}



