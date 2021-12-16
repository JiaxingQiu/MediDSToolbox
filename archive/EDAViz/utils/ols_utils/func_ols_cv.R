ols_cv <- function(df, external_df=NULL, fml, cluster_col, nfold){
  # ---- Usage ----
  # clustered cross validation of rms::ols
  
  # ---- Arguments ----
  # df: a dataframe object for cross validation
  # external_df: external dfset for validation /  testing
  # fml: a string of model formula 
  # cluster_col: cluster column name in the dataframe for repeated measures
  # nfold: N folds cross validation intager
  # stratified:  sampling subjects stratified by outcome
  
  # ---- Value ----
  # model_data: modeling dataframe with folds and predicted response columns
  # model_info: ML score estimates of current model and model information
  # calibration_curve: calibratioin curve plot 
  
  
  
  mdl_cols <- c(cluster_col)# columns to be used in model
  y_col <- NULL# response column name
  for (col in colnames(df)){
    if(grepl(col, fml)) mdl_cols<-c(mdl_cols,col)# store column names by group based on input formula string
    if(grepl(col, strsplit(fml,"~")[[1]][1])) y_col <- col
  }
  
  df_mdl <- df[, mdl_cols]# initiate modeling dataframe
  
  # prepare folds indicators
  if (!is.null(external_df)) { # if testing / validation dfset is as external dataframe
    external_df <- external_df[, mdl_cols]
    external_df$fold <- 1
    df_mdl$fold <- 0
    df_mdl <- bind_rows(df_mdl, external_df)
  }
  
  
  clusters <- sample(unique(as.character(df_mdl[,cluster_col])))# list of clusters to sample from
  
  
  if(n_distinct(clusters)<nfold){# check enough sample subjects in each outcome group
    message( "too few subjects in one or more classes of the outcome for required cv n folds. using 1 fold 80/20 split instead")
    df_mdl$fold <- 0
    valid_clusters <- sample(clusters, size=as.integer(length(clusters)*0.2) )  
    df_mdl$fold[which(df_mdl[,cluster_col]%in%valid_clusters)] <- 1
  } else {
    fold_idx_df <- data.frame(cluster_col=clusters, fold=cut(seq(1, length(clusters)), min(length(clusters),nfold), labels=c(seq(1, nfold))))
    df_mdl$cluster_col <- df_mdl[,cluster_col]
    df_mdl <- merge(df_mdl,fold_idx_df,all.x=TRUE)
    df_mdl <- df_mdl[,setdiff(colnames(df_mdl), c("cluster_col")) ]
  }
  
  
  dd <- datadist(df_mdl)# train model nfold times
  options(datadist=dd, na.action=na.omit)
  df_mdl$y_pred <- NA
  eval_df<-c()
  for (N in 1:max(as.numeric(as.character( df_mdl$fold)), na.rm=TRUE)){
    tryCatch({# train the model on training set
      
      mdl <- rms::robcov(rms::ols(as.formula(fml),x=TRUE, y=TRUE, data=df_mdl[which(df_mdl$fold!=N),]),cluster=df_mdl[which(df_mdl$fold!=N),cluster_col])
      df_mdl$y_pred[which(df_mdl$fold==N)] <- rms::predictrms(mdl, newdata=df_mdl[which(df_mdl$fold==N),])
      
      # keep the train prediction as a data.frame
      train_eval_df <- data.frame(y_pred = predictrms(mdl, newdata=df_mdl[df_mdl$fold!=N,] ),
                                     y_true = as.numeric( as.character(df_mdl[df_mdl$fold!=N,y_col]) ) )
      train_eval_df <- train_eval_df[complete.cases(train_eval_df),]
      # keep the validation prediction as a data.frame
      valid_eval_df <- data.frame(y_pred = predictrms(mdl, newdata=df_mdl[df_mdl$fold==N,]),
                                     y_true = as.numeric( as.character(df_mdl[df_mdl$fold==N,y_col])))
      valid_eval_df <- valid_eval_df[complete.cases(valid_eval_df),]
      
      eval_df <- bind_rows(eval_df, # calculate evaluation matrices
                           data.frame(fold_index = N,
                                      model_AIC = AIC(mdl), 
                                      model_BIC = BIC(mdl), 
                                      train_mse = MLmetrics::MSE(train_eval_df$y_pred, train_eval_df$y_true ),
                                      valid_mse = MLmetrics::MSE(valid_eval_df$y_pred, valid_eval_df$y_true )
                           ))
    },error=function(e){ print(e) })
  }
  # add numeric 0-1 responce y_true as a column in df_mdl
  df_mdl$y_true <- as.numeric(as.character(df_mdl[,y_col]))
  
  # remove NA
  df_mdl <- df_mdl[complete.cases(df_mdl[,c('y_true','y_pred')]),]
  
  # success folds
  success_nfold <- sum(!is.na(eval_df$fold_index)) 
  
  # get scores for current model
  scores <- eval_df %>% summarise_at(c("model_AIC","model_BIC",
                                       "train_mse", "valid_mse"),
                                     list(mean = ~round(mean(., na.rm = TRUE),6) ,
                                          se = ~sd(., na.rm = TRUE)/sqrt(success_nfold) ))
  
  
  scores$success_nfold <- success_nfold
  
  
  model_info <- data.frame(formula=fml, cluster_col=cluster_col, stringsAsFactors = FALSE)
  
  print(eval_df)
  
  return(list( "cv_eval_trace" = eval_df,
               "model_info" = model_info,
               "model_scores"=scores, 
               "model_data"=df_mdl, 
               "calibration_curve"=NULL
  ))
  
}
