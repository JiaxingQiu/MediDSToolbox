ols_formula <- function(df, y_col, cluster_col, rcs5_x_cols=c(), rcs4_x_cols=c(), rcs3_x_cols=c(), linear_x_cols=c(), fct_x_cols=c()){
  
  
  # ---- Usage  ----
  # for multivariable ols model using rcs function, partially singularity will cause error in rms package, 
  # this function is used to modify the complexity / dof assigned to each predictor, and finalize the model formula
  
  # ---- Arguments ----
  # df: dfframe oject to train the model
  # rcs5_x_cols: a list of predictor column names that will be assigned with 5 knots rcs in the formula
  # rcs4_x_cols: a list of predictor column names that will be assigned with 4 knots rcs in the formula
  # rcs3_x_cols: a list of predictor column names that will be assigned with 3 knots rcs in the formula
  # linear_x_cols: a list of predictor column names that will be designed as linear term in the formula
  # fct_x_cols: a list of predictor column names that will be factor term in the formula
  # y_col: numeric responce column name
  # cluster_col: cluster column name for repeated measure
  
  
  # ---- Values  ----
  # fml_string: finalized string object of formula 
  # rcs5_x_cols: finalized list of predictor column names that will be assigned with 5 knots rcs in the formula
  # rcs4_x_cols: finalized list of predictor column names that will be assigned with 4 knots rcs in the formula
  # rcs3_x_cols: finalized list of predictor column names that will be assigned with 3 knots rcs in the formula
  # linear_x_cols: finalized list of predictor column names that will be designed as linear term in the formula
  # fct_x_cols: finalized list of predictor column names that will be factor term in the formula
  # afford_dof: affordable degree of freedom from df
  # use_dof: how many degree of freedom the finalized formula is using 
  
  # prepare modeling dataframe
  df_mdl <- df[, c(y_col, fct_x_cols ,linear_x_cols, rcs3_x_cols, rcs4_x_cols, rcs5_x_cols, cluster_col)]
  df_mdl[,y_col] <- as.factor(as.character( df_mdl[,y_col]) )
  dd <- datadist(as.data.frame( df_mdl))
  options(datadist=dd, na.action=na.omit)
  
  # prepare fml by input term lists
  fml_y <- paste0(y_col," ~ ")
  fml_rcs5_x <- ifelse(length(rcs5_x_cols)==0, "", paste(paste0("rcs(",rcs5_x_cols,",5)"), collapse=" + "))
  fml_rcs4_x <- ifelse(length(rcs4_x_cols)==0, "", paste(paste0("rcs(",rcs4_x_cols,",4)"), collapse=" + "))
  fml_rcs3_x <- ifelse(length(rcs3_x_cols)==0, "", paste(paste0("rcs(",rcs3_x_cols,",3)"), collapse=" + "))
  fml_linear_x <- ifelse(length(linear_x_cols)==0, "",paste(paste0("I(",linear_x_cols,")"), collapse=" + ") )
  fml_fct_x <- ifelse(length(fct_x_cols)==0, "", paste(paste0("catg(as.factor(",fct_x_cols,"))"), collapse=" + "))
  fml_x_list <- c(fml_rcs5_x,fml_rcs4_x, fml_rcs3_x,fml_linear_x,fml_fct_x)
  fml_x <- paste(fml_x_list[which(fml_x_list!="")],collapse ="+")
  fml_string <- paste0(fml_y,fml_x) # initial formula string object
  
  
  # fix offending variables
  mdl <- rms::ols(formula(fml_string),x=TRUE, y=TRUE, data=df_mdl, penalty = 0) # with no penalty
  
  # return finalized fml_string
  return(list("fml_string" = fml_string,
              "rcs5_x_cols" = rcs5_x_cols, 
              "rcs4_x_cols" = rcs4_x_cols, 
              "rcs3_x_cols" = rcs3_x_cols, 
              "linear_x_cols" = linear_x_cols, 
              "fct_x_cols" = fct_x_cols
              ))
  
}

