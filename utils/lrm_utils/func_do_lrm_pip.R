do_lrm_pip <- function(data=subset_df(data_ml, "40w"), # data for model training
                       data_org=subset_df(data_ml, "40w"), # data for inference
                       dict_data=dict_ml,
                       x_cols_linear=x_cols_linear,
                       x_cols_nonlin_rcs5=x_cols_nonlin_rcs5,
                       x_cols_nonlin_rcs4=x_cols_nonlin_rcs4,
                       x_cols_nonlin_rcs3=x_cols_nonlin_rcs3,
                       x_cols_fct=x_cols_fct,
                       x_cols_tag=x_cols_tag,
                       x_cols=c(x_num_cols, x_fct_cols), 
                       y_col="primary_outcome_factor_Unfavorable", 
                       cluster_col="subjectnbr",
                       r2=0.9,
                       rcs5_low="70%", # knots to use talk to matthew (AIC? BIC? )
                       rcs4_low="50%",
                       cv_nfold=5,
                       test_data=NULL, # test data that is engineered same way as training dataset
                       test_data_org = NULL,
                       na_frac_max=0.3, 
                       num_col2=NULL,
                       stratified_cv=TRUE,
                       r_abs=0.8, 
                       type=c("pearson","spearman")[1],
                       rank=TRUE,
                       fix_knots=FALSE){
  
  
  do_obj <- NULL
  do_obj$df_final <- data
  
  # --- redundancy analysis ----
  df <- do_obj$df_final
  dict_df <- get.dict(df)
  print(dict_df)
  print("--- do_x_redun ---")
  redun_obj <- NULL
  try({
    do_obj <- do_x_redun(df=df, dict_df=dict_df, x_cols=x_cols, r_abs=r_abs, type=type, r2=r2, rank=rank)
    redun_obj <- list("rmcor_obj"=do_obj$rmcor_obj,
                      "redun_obj"=do_obj$redun_obj)
  },TRUE)
  
  
  # --- initiate model formula ---
  df <- do_obj$df_final
  dict_df <- get.dict(df)
  print(dict_df)
  print("--- do_init ---")
  do_obj <- do_init(df, dict_df, y_col=y_col, x_cols, cluster_col, rcs5_low=rcs5_low,rcs4_low=rcs4_low,linear_cols=x_cols_linear)
  # if knots are fixed by user, overwrite the dictionary dataframe in do_obj
  if(fix_knots){
    dict_init <- do_obj$dict_final
    dict_init[x_cols_nonlin_rcs3,'mdl_term_init'] <- 'rcs3'
    dict_init[x_cols_nonlin_rcs4,'mdl_term_init'] <- 'rcs4'
    dict_init[x_cols_nonlin_rcs5,'mdl_term_init'] <- 'rcs5'
    dict_init[x_cols_linear,'mdl_term_init'] <- 'linear'
    dict_init[union(x_cols_fct,x_cols_tag),'mdl_term_init'] <- 'factor'
    do_obj$dict_final <- dict_init
    do_obj$df_final <- assign.dict(do_obj$df_final, do_obj$dict_final, overwrite = TRUE)
  }
  dof_obj <- NULL
  dof_obj <- do_obj$spearman2_rho
  
  # --- dictionary oriented (do) cross validate modeling ---
  df <- do_obj$df_final
  dict_df <- get.dict(df)
  print(dict_df)
  print("--- do_lrm_cv ---")
  model_obj <- do_lrm_cv(df=df, 
                         dict_df=dict_df, 
                         cv_nfold=cv_nfold, 
                         na_frac_max=na_frac_max, 
                         stratified_cv=stratified_cv)
  
  # --- inference object / report ---
  infer_obj <- lrm_infer(df=df, 
                         df_org=data_org, 
                         dict_df=dict_df, 
                         fml=model_obj$cv_obj$model_info$formula, 
                         cluster_col=model_obj$cv_obj$model_info$cluster_col, 
                         penalty=model_obj$cv_obj$model_info$penalty,
                         num_col2=num_col2)
  
  # --- external testing results ---
  
  test_obj <- NULL
  tryCatch({
    test_obj <- lrm_test(
      test_data = test_data,
      test_data_org = test_data_org,
      y_col = y_col,
      mdl_obj = infer_obj$mdl_obj)
  },error=function(e){
    print(e)
  })
  
  
  return(list("redun_obj"=redun_obj,
              "model_obj"=model_obj, 
              "infer_obj"=infer_obj,
              "test_obj"=test_obj,
              "dof_obj"=dof_obj))
  
}
