library(gglasso)

lasso_x_select_group <- function(
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
  dict_data=NULL # dictionary table is optional
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
  
  # add transformations to predictor variables and save grouping info in a dataframe object
  x_col_df_all <- data.frame()
  # --- rcs5 ---
  for (col in x_cols_nonlin_rcs5){
    # additional columns
    data[,col] <- as.numeric(rcs(data[,col],5)[,1])
    data[,paste0(col,"'")] <- as.numeric(rcs(data[,col],5)[,2])
    data[,paste0(col,"''")] <- as.numeric(rcs(data[,col],5)[,3])
    data[,paste0(col,"'''")] <- as.numeric(rcs(data[,col],5)[,4])
    
    # append this varname and group info as a data frame to overall column info
    x_col_df <- data.frame(
      x_colname = c(col, paste0(col,"'"), paste0(col,"''"),paste0(col,"'''")),
      x_group = paste0(col,"_rcs5")
    )
    x_col_df_all <- bind_rows(x_col_df_all, x_col_df)
  }
  
  # --- rcs4 ---
  for (col in x_cols_nonlin_rcs4){
    # additional columns
    data[,col] <- as.numeric(rcs(data[,col],4)[,1])
    data[,paste0(col,"'")] <- as.numeric(rcs(data[,col],4)[,2])
    data[,paste0(col,"''")] <- as.numeric(rcs(data[,col],4)[,3])
    
    # append this varname and group info as a data frame to overall column info
    x_col_df <- data.frame(
      x_colname = c(col, paste0(col,"'"), paste0(col,"''")),
      x_group = paste0(col,"_rcs4")
    )
    x_col_df_all <- bind_rows(x_col_df_all, x_col_df)
  }
  
  # --- rcs3 ---
  for (col in x_cols_nonlin_rcs3){
    # additional columns
    data[,col] <- as.numeric(rcs(data[,col],3)[,1])
    data[,paste0(col,"'")] <- as.numeric(rcs(data[,col],3)[,2])
    
    # append this varname and group info as a data frame to overall column info
    x_col_df <- data.frame(
      x_colname = c(col, paste0(col,"'")),
      x_group = paste0(col,"_rcs3")
    )
    x_col_df_all <- bind_rows(x_col_df_all, x_col_df)
  }
  
  # --- linear ---
  for (col in x_cols_linear){
    # append this varname and group info as a data frame to overall column info
    x_col_df <- data.frame(
      x_colname = c(col),
      x_group = paste0(col,"_linear")
    )
    x_col_df_all <- bind_rows(x_col_df_all, x_col_df)
  }
  
  # --- fct ---
  for (col in x_cols_fct){
    # dummy the variable
    levels <- unique(as.character(data[,col]))
    for(l in levels){
      data[,paste0(col,"___",l)] <- ifelse(data[,col]==l,1,0)
      # append this varname and group info as a data frame to overall column info
      x_col_df <- data.frame(
        x_colname = c(paste0(col,"___",l)),
        x_group = paste0(col,"_level")
      )
      x_col_df_all <- bind_rows(x_col_df_all, x_col_df)
    }
  }
  # --- tag ---
  for (col in x_cols_tag){
    # append this varname and group info as a data frame to overall column info
    x_col_df <- data.frame(
      x_colname = c(col),
      x_group = paste0(col,"_tag01")
    )
    x_col_df_all <- bind_rows(x_col_df_all, x_col_df)
  }
  
  x <- data.matrix(scale(data[complete.cases(data[,c(x_col_df_all$x_colname,y_col)]),x_col_df_all$x_colname]))
  y <- matrix(data[complete.cases(data[,c(x_col_df_all$x_colname,y_col)]),y_col])
  y <- ifelse(y==1, 1, -1)
  #y <- as.factor(y)
  v.group <- as.numeric(as.factor(x_col_df_all$x_group))
  
  # ---- cv lasso an ridge regression ---
  lasso_cv <- gglasso::cv.gglasso(x=x, 
                                  y=y, 
                                  group=v.group, 
                                  loss="logit",
                                  pred.loss = "misclass",
                                  nfolds = 10)
  # ----- show panalization trace -----
  par(mfrow = c(1, 1))
  plot(lasso_cv, main = "Lasso penalty\n\n")
  abline(v = log(lasso_cv$lambda.min), col = "red", lty = "dashed")
  abline(v = log(lasso_cv$lambda.1se), col = "blue", lty = "dashed")
  
  # ----- show panalization trace -----
  lasso_trace <- gglasso::gglasso(x=x, 
                                    y=y, 
                                    group=v.group, 
                                    loss="logit")
  plot(lasso_trace)
  
  #  ----- train optimal lambda models ----
  lasso_optimal <- gglasso::gglasso(x=x, 
                                    y=y, 
                                    group=v.group, 
                                    loss="logit",
                                    lambda = lasso_cv$lambda.1se)
  
  # plot variable importance (coefficients) on final model obj
  #data.frame(group = v.group, beta = lasso_optimal$beta[,1])
  
  return(list( lasso_cv = lasso_cv,
               lasso_trace = lasso_trace,
               lasso_optimal = lasso_optimal ))
  
}



