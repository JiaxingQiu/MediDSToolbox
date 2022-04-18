library(gglasso)

lasso_x_select_group <- function(
  data, 
  y_col,
  x_cols_nonlin_rcs5=c(),
  x_cols_nonlin_rcs4=c(),
  x_cols_nonlin_rcs3=c(),
  x_cols_linear=c(), 
  x_cols_fct=c(),
  x_cols_tag=c(),
  family = c("binomial", "multinomial", "gaussian")[1],
  dict_data=NULL, # dictionary table is optional
  lambda=c("auto","1se","min")[1],
  lambda_value = NULL, # external specified lasso lambda value
  keep_rowname=TRUE
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
  
  print("--- response distribution ---")
  print(table(data[,y_col]))
  
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
  #data[,x_cols] <- scale(data_org[,x_cols], center = standardize, scale = standardize)
  data[,x_cols_tag] <- data_org[,x_cols_tag] # overwrite dummy columns, they should not be scaled
  
  
  # add transformations to predictor variables and save grouping info in a dataframe object
  x_col_df_all <- data.frame()
  # --- rcs5 ---
  for (col in x_cols_nonlin_rcs5){
    success <- FALSE
    tryCatch({
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
      success <- TRUE
    },error=function(e){
      print(paste0("failed to assign rcs 4 for ", col, ", using linear instead"))
    })
    if(!success){
      x_cols_linear <- c(x_cols_linear, col)
      x_cols_nonlin_rcs5 <- setdiff(x_cols_nonlin_rcs5, col)
    }
  }
  
  # --- rcs4 ---
  for (col in x_cols_nonlin_rcs4){
    success <- FALSE
    tryCatch({
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
      success <- TRUE
    },error=function(e){
      print(paste0("failed to assign rcs 4 for ", col, ", using linear instead"))
    })
    if(!success){
      x_cols_linear <- c(x_cols_linear, col)
      x_cols_nonlin_rcs4 <- setdiff(x_cols_nonlin_rcs4, col)
    }
  }
  
  # --- rcs3 ---
  for (col in x_cols_nonlin_rcs3){
    success <- FALSE
    tryCatch({
      # additional columns
      data[,col] <- as.numeric(rcs(data[,col],3)[,1])
      data[,paste0(col,"'")] <- as.numeric(rcs(data[,col],3)[,2])
      
      # append this varname and group info as a data frame to overall column info
      x_col_df <- data.frame(
        x_colname = c(col, paste0(col,"'")),
        x_group = paste0(col,"_rcs3")
      )
      x_col_df_all <- bind_rows(x_col_df_all, x_col_df)
      success <- TRUE
    },error=function(e){
      print(paste0("failed to assign rcs 3 for ", col, ", using linear instead"))
    })
    if(!success){
      x_cols_linear <- c(x_cols_linear, col)
      x_cols_nonlin_rcs3 <- setdiff(x_cols_nonlin_rcs3, col)
    }
  }
  
  # --- linear ---
  for (col in x_cols_linear){
    success <- FALSE
    tryCatch({
      # append this varname and group info as a data frame to overall column info
      x_col_df <- data.frame(
        x_colname = c(col),
        x_group = paste0(col,"_linear")
      )
      x_col_df_all <- bind_rows(x_col_df_all, x_col_df)
      success <- TRUE
    },error=function(e){
      print(paste0("failed to assign linear for ", col, ", variable removed"))
    })
    if(!success) x_cols_linear <- setdiff(x_cols_linear, col)
  }
  
  # --- fct ---
  for (col in x_cols_fct){
    # dummy the variable
    levels <- unique(as.character(data[,col]))
    for(l in levels){
      l_fix <- gsub("[^[:alnum:]]","_",l)
      data[,paste0(col,"___",l_fix)] <- ifelse(data[,col]==l,1,0)
      # append this varname and group info as a data frame to overall column info
      x_col_df <- data.frame(
        x_colname = c(paste0(col,"___",l_fix)),
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
  
  x <- data.matrix(data[complete.cases(data[,c(x_col_df_all$x_colname,y_col)]),x_col_df_all$x_colname])
  y <- data.matrix(data[complete.cases(data[,c(x_col_df_all$x_colname,y_col)]),y_col])
  colnames(y) <- y_col
  y <- ifelse(y==1, 1, -1)
  #y <- as.factor(y)
  v.group <- as.numeric(as.factor(x_col_df_all$x_group))
  
  # ---- cv lasso an ridge regression ---
  lasso_cv <- gglasso::cv.gglasso(x=x, 
                                  y=y, 
                                  group=v.group, 
                                  loss="logit", # deviance function for regular logistic function
                                  pred.loss = "misclass", #"misclass"
                                  nfolds = 10)
  # ----- show penalization trace -----
  # par(mfrow = c(1, 1))
  # plot(lasso_cv, main = "Lasso penalty\n\n")
  # abline(v = log(lasso_cv$lambda.min), col = "red", lty = "dashed")
  # abline(v = log(lasso_cv$lambda.1se), col = "blue", lty = "dashed")
  # 
  
  # ----- show penalization trace -----
  lasso_trace <- gglasso::gglasso(x=x, 
                                  y=y, 
                                  group=v.group, 
                                  loss="logit")
  # plot(lasso_trace)
  
  #  ----- train optimal lambda models ----
  opt_lambda <- lasso_cv$lambda.1se
  if (lasso_cv$cvm[which(lasso_cv$lambda==lasso_cv$lambda.min)] == min(lasso_cv$cvm) ){
    opt_lambda <- lasso_cv$lambda.min
  }
  if(lambda %in% c("min","1se")){
    if(lambda=="min") {
      opt_lambda <- lasso_cv$lambda.min
    }else{
      opt_lambda <- lasso_cv$lambda.1se
    }
  }
  if(!is.null(lambda_value)){
    opt_lambda <- lambda_value
  }
  lasso_optimal <- gglasso::gglasso(x=x, 
                                    y=y, 
                                    group=v.group, 
                                    loss="logit",
                                    lambda = opt_lambda)
  # if(standardize){
  #   attr(x,"scaled:center") <- attr(scale(data_org[,x_cols]), "scaled:center")
  #   attr(x,"scaled:scale") <- attr(scale(data_org[,x_cols]), "scaled:scale")
  # }else{
  #   attr(x,"scaled:center") <- attr(scale(data_org[,x_cols], center = rep(0, length(x_cols)), scale = rep(1, length(x_cols)) ), "scaled:center")
  #   attr(x,"scaled:scale") <- attr(scale(data_org[,x_cols], center = rep(0, length(x_cols)), scale = rep(1, length(x_cols))), "scaled:scale")
  # }
  
  lasso_optimal$x <- x
  lasso_optimal$y <- ifelse(y<0,0,1)
  lasso_optimal$group_info <- x_col_df_all
  
  # plot variable importance (coefficients) on final model obj
  #data.frame(group = v.group, beta = lasso_optimal$beta[,1])
  
  
  # ------ manually 10 fold cross validation scores ---------
  train_scores_tbl_all <- data.frame()
  valid_scores_tbl_all <- data.frame()
  valid_yhat_df_all <- data.frame()
  if(keep_rowname){
    print("cross validation by external sequence")
    row_index <- row.names(data_org)
  }else{
    row_index <- sample(1:nrow(data),nrow(data))# shuffle data rawname / index, if not using external fold indicator
  }
  rownames(data) <- row_index
  foldsize <- round(nrow(data)/10)
  
  for(i in c(1:10)){
    try({
      validset <- data[which( as.numeric(rownames(data)) %in% c(seq((i-1)*foldsize, i*foldsize, 1)+1) ), ]
      trainset <- data[which(!as.numeric(rownames(data)) %in% c(seq((i-1)*foldsize, i*foldsize, 1)+1) ), ]
      
      train_x <- data.matrix(trainset[complete.cases(trainset[,c(x_col_df_all$x_colname,y_col)]),x_col_df_all$x_colname])
      train_y <- data.matrix(trainset[complete.cases(trainset[,c(x_col_df_all$x_colname,y_col)]),y_col])
      train_y <- ifelse(train_y==1, 1, -1)
      
      valid_x <- data.matrix(validset[complete.cases(validset[,c(x_col_df_all$x_colname,y_col)]),x_col_df_all$x_colname])
      valid_y <- data.matrix(validset[complete.cases(validset[,c(x_col_df_all$x_colname,y_col)]),y_col])
      valid_y <- ifelse(valid_y==1, 1, -1)
      lasso_fold_optimal <- NULL
      lasso_fold_optimal <- gglasso::gglasso(x=train_x, 
                                             y=train_y, 
                                             group=v.group, 
                                             loss="logit",
                                             lambda = lasso_cv$lambda.1se)
      y_prob_train <- exp(predict(lasso_fold_optimal, newx = train_x, type="link")) / (1 + exp(predict(lasso_fold_optimal, newx = train_x, type="link")))
      train_scores <- mdl_test(y_true=as.numeric(  ifelse(train_y<0,0,1) ),
                               y_prob =as.numeric(  y_prob_train ),
                               threshold = mean(data[,y_col],na.rm=TRUE))
      train_scores_tbl <- train_scores$res_df
      train_scores_tbl$data <- i
      train_scores_tbl_all <- bind_rows(train_scores_tbl_all, train_scores_tbl)
        
      y_prob_valid <- exp(predict(lasso_fold_optimal, newx = valid_x, type="link")) / (1 + exp(predict(lasso_fold_optimal, newx = valid_x, type="link")))
      valid_yhat_df <- data.frame( 
        rowid = rownames( validset[complete.cases(validset[,c(x_col_df_all$x_colname,y_col)]),x_col_df_all$x_colname] ),
        y_true = as.numeric( ifelse(valid_y<0,0,1) ),
        y_prob =  as.numeric( y_prob_valid) ,
        fold = i
      )
      valid_yhat_df_all <- bind_rows(valid_yhat_df_all, valid_yhat_df, )
      valid_scores <- mdl_test(y_true = as.numeric( ifelse(valid_y<0,0,1) ),
                               y_prob =  as.numeric( y_prob_valid) ,
                               threshold = mean(data[,y_col],na.rm=TRUE))
      valid_scores_tbl <- valid_scores$res_df
      valid_scores_tbl$data <- i
      valid_scores_tbl_all <- bind_rows(valid_scores_tbl_all, valid_scores_tbl)
    },TRUE)
  }
  # reformat 10 AUCs (tricky one)
  train_score_final <- train_scores_tbl_all[,setdiff(colnames(train_scores_tbl_all),"data")] %>% 
    summarise_all( list(mean = ~mean(., na.rm=TRUE),
                        sd = ~sd(., na.rm=TRUE)) )
  valid_score_final <- valid_scores_tbl_all[,setdiff(colnames(valid_scores_tbl_all),"data")] %>% 
    summarise_all( list(mean = ~mean(., na.rm=TRUE),
                        sd = ~sd(., na.rm=TRUE)) )
  scores_final <- bind_rows(train_score_final, valid_score_final)
  scores_final$dataset <- c("train", "valid")
  scores_final <- scores_final[,union("dataset", colnames(scores_final))]
  
  # cross-validated labeling (yhat)
  cv_final_scores <- mdl_test(y_true = valid_yhat_df_all$y_true,
                              y_prob = valid_yhat_df_all$y_prob,
                              threshold = mean(data[,y_col],na.rm=TRUE))
  score_final_cv <- cv_final_scores$res_df
  score_final_cv$data <- "cv_yhat_df"
  
  return(list( lasso_cv = lasso_cv,
               lasso_trace = lasso_trace,
               lasso_optimal = lasso_optimal,
               scores_final_10fold = scores_final,
               cv_yhat_df = valid_yhat_df_all,
               score_final_cv = score_final_cv))
  
}



