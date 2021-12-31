do_ols_pip <- function(data=data_ml, # data for model training
                       dict_data=dict_ml,
                       x_cols=c("ga_days","ca_days"), # predictor
                       y_col="Desat_80_v2_IH_event_dur_sec", # response
                       cluster_col="subjectnbr", # cluster column for repeated measures
                       r2=0.95, # spearman quadratic correlation coefficient
                       rcs5_low="70%",
                       rcs4_low="50%",
                       cv_nfold=5,
                       test_data=NULL, 
                       na_frac_max=0.3, 
                       linear_cols=NULL,
                       num_col2=NULL,
                       stratified_cv=TRUE,
                       r_abs=0.8, 
                       type=c("pearson","spearman")[1],
                       rank=TRUE){
  
  
  do_obj <- NULL
  do_obj$df_final <- data
  
  # --- redundancy analysis ----
  df <- do_obj$df_final
  dict_df <- get.dict(df)
  print(dict_df)
  redun_obj<-NULL
  try({
    do_obj <- do_x_redun(df=df, dict_df=dict_df, x_cols=x_cols, r_abs=r_abs, type=type, r2=r2, rank=rank)
    redun_obj <- list("rmcor_obj"=do_obj$rmcor_obj,
                  "redun_obj"=do_obj$redun_obj)
  },TRUE)
  
  # --- initiate model formula ---
  df <- do_obj$df_final
  dict_df <- get.dict(df)
  print(dict_df)
  do_obj <- do_init(df, dict_df, y_col, x_cols, cluster_col, rcs5_low=rcs5_low,rcs4_low=rcs4_low,linear_cols=linear_cols)
  dof_obj <- NULL
  dof_obj <- do_obj$spearman2_rho
    
    
  # --- dictionary oriented (do) cross validate modeling ---
  df <- do_obj$df_final
  dict_df <- get.dict(df)
  print(dict_df)
  model_obj <- do_ols_cv(df=df, dict_df=dict_df, cv_nfold=cv_nfold, na_frac_max=na_frac_max, external_df=test_data,stratified_cv=stratified_cv)
  
  # --- inference object / report ---
  infer_obj <- ols_infer(df=df, 
                         df_org=df, 
                         dict_df=dict_df, 
                         fml=model_obj$cv_obj$model_info$formula, 
                         cluster_col=model_obj$cv_obj$model_info$cluster_col, 
                         num_col2=num_col2)
  
  return(list("redun_obj"=redun_obj,
              "model_obj"=model_obj, 
              "infer_obj"=infer_obj,
              "dof_obj"=dof_obj))
  
  
  
}