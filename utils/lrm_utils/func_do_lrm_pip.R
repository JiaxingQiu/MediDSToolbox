do_lrm_pip <- function(data, # data for model training engineered
                       dict_data,
                       x_cols_linear,
                       x_cols_nonlin_rcs5,
                       x_cols_nonlin_rcs4,
                       x_cols_nonlin_rcs3,
                       x_cols_fct,
                       x_cols_tag,
                       x_cols=union(c(x_cols_linear, x_cols_nonlin_rcs5, x_cols_nonlin_rcs4,x_cols_nonlin_rcs3,x_cols_fct, x_cols_tag)), 
                       y_col, 
                       cluster_col,
                       r2=0.9,
                       rcs5_low="70%", # knots to use talk to matthew (AIC? BIC? )
                       rcs4_low="50%",
                       cv_nfold=5,
                       data_org=NULL, # raw data no engineering
                       test_data=NULL, # test data that is engineered same way as training dataset
                       test_data_org = NULL, # raw test data no engineering
                       na_frac_max=0.3, 
                       joint_col2=NULL,
                       stratified_cv=TRUE,
                       r_abs=0.8, 
                       type=c("pearson","spearman")[1],
                       rank=TRUE,
                       fix_knots=FALSE,
                       y_map_func=c("fold_risk", "probability", "log_odds")[1],
                       y_map_max=3,
                       tune_by=c("logloss","auroc","aic","bic")[1],
                       lambda_value = NULL,
                       trim_by_col=NULL, # colname for relative_time
                       return_performance = TRUE,
                       return_fitted_effect = TRUE,
                       return_scores_plot = TRUE, # feature permutation scores
                       fold_idx_df_ex=NULL
                       ){
  
  
  # --- redundancy analysis ----
  df <- assign.dict(data,dict_data)
  dict_df <- get.dict(df)
  #print(dict_df)
  redun_obj <- NULL
  print("--- do_x_redun ---")
  if(!fix_knots){
    try({
      redun_obj <- do_x_redun(df=df, 
                           dict_df=dict_df, 
                           x_cols=x_cols, 
                           r_abs=r_abs, 
                           type=type, 
                           r2=r2, 
                           rank=rank)
    },TRUE)
  }
  
  
  # --- initiate model formula ---
  if (!is.null(redun_obj)){
    df <- redun_obj$df_final
    dict_df <- redun_obj$dict_final
  }else{
    df <- assign.dict(data,dict_data)
    dict_df <- get.dict(df)
  }
  #print(dict_df)
  print("--- do_init ---")
  init_obj <- do_init(df, dict_df, y_col=y_col, x_cols, cluster_col, rcs5_low=rcs5_low,rcs4_low=rcs4_low,linear_cols=x_cols_linear)
  # if knots are fixed by user, overwrite the dictionary dataframe in do_obj
  if(fix_knots){
    dict_init <- init_obj$dict_final
    dict_init[x_cols_nonlin_rcs3,'mdl_term_init'] <- 'rcs3'
    dict_init[x_cols_nonlin_rcs4,'mdl_term_init'] <- 'rcs4'
    dict_init[x_cols_nonlin_rcs5,'mdl_term_init'] <- 'rcs5'
    dict_init[x_cols_linear,'mdl_term_init'] <- 'linear'
    dict_init[union(x_cols_fct,x_cols_tag),'mdl_term_init'] <- 'factor'
    init_obj$dict_final <- dict_init
    init_obj$df_final <- assign.dict(init_obj$df_final, init_obj$dict_final, overwrite = TRUE)
  }
  
  # --- dictionary oriented (do) cross validate modeling ---
  df <- init_obj$df_final
  dict_df <- init_obj$dict_final
  #print(dict_df)
  print("--- do_lrm_cv ---")
  model_obj <- do_lrm_cv(df=df, 
                         dict_df=dict_df, 
                         cv_nfold=cv_nfold, 
                         na_frac_max=na_frac_max, 
                         stratified_cv=stratified_cv,
                         tune_by = tune_by,
                         lambda_value = lambda_value,
                         fold_idx_df_ex=fold_idx_df_ex)
  
  # --- lrm_infer modeling ---
  print("--- lrm_infer ---")
  infer_obj <- NULL
  # within model inference
  infer_obj <- lrm_infer(mdl_obj = model_obj$mdl_obj,
                         y_map_func = y_map_func,
                         y_map_max = y_map_max,
                         joint_col2 = joint_col2,
                         df = model_obj$df_final)
  
  # --- lrm_perform (internal performance) ---
  print("--- lrm_perform ---")
  cv_scores_all_final <- NULL
  cv_scores_all_final <- model_obj$cv_obj$score_final_cv_permu
  if(!is.null(cv_scores_all_final)){
    cv_scores_all_final_none <- model_obj$score_trace_df[which( model_obj$score_trace_df$penalty==max(model_obj$score_trace_df$penalty,na.rm = TRUE) ),]
    cv_scores_all_final_none$data <- "none"
    cv_scores_all_final_none <- cv_scores_all_final_none[,colnames(cv_scores_all_final)]
    cv_scores_all_final <- bind_rows(cv_scores_all_final, cv_scores_all_final_none)
    cv_scores_all_final$data <- gsub("_linear","",gsub("permutate ","",cv_scores_all_final$data))
    colnames(cv_scores_all_final)[which(colnames(cv_scores_all_final)=="data")] <- "removed_variable"
  }
  
  perform_obj <- NULL
  perform_obj$internal <- NULL
  perform_obj$internal_org <- NULL
  perform_obj$external <- NULL
  perform_obj$external_org <- NULL
  tryCatch({
    stopifnot(return_performance)
    perform_obj$internal <- lrm_perform(mdl_obj = model_obj$mdl_obj,
                                        df = data,
                                        y_map_func = y_map_func,
                                        y_map_max = y_map_max,
                                        rel_time_col=trim_by_col,
                                        return_fitted_effect=return_fitted_effect,
                                        return_scores_plot = return_scores_plot,
                                        cv_scores_all_final = cv_scores_all_final)
  },error=function(e){print(e)})
  tryCatch({
    stopifnot(return_performance)
    perform_obj$internal_org <- lrm_perform(mdl_obj = model_obj$mdl_obj,
                                            df = data_org,
                                            y_map_func =y_map_func,
                                            y_map_max = y_map_max,
                                            rel_time_col=trim_by_col,
                                            return_fitted_effect=return_fitted_effect,
                                            return_scores_plot = FALSE)
  },error=function(e){print(e)})
  tryCatch({
    stopifnot(return_performance)
    perform_obj$external <- lrm_perform(mdl_obj = model_obj$mdl_obj,
                                        df = test_data,
                                        y_map_func = y_map_func,
                                        y_map_max = y_map_max,
                                        rel_time_col=trim_by_col,
                                        return_fitted_effect=return_fitted_effect,
                                        return_scores_plot = return_scores_plot)
  },error=function(e){print(e)})
  tryCatch({
    stopifnot(return_performance)
    perform_obj$external_org <- lrm_perform(mdl_obj = model_obj$mdl_obj,
                                            df = test_data_org,
                                            y_map_func =y_map_func,
                                            y_map_max = y_map_max,
                                            rel_time_col=trim_by_col,
                                            return_fitted_effect=return_fitted_effect,
                                            return_scores_plot = FALSE)
  },error=function(e){print(e)})
  
  
  return(list("redun_obj"=redun_obj,
              "model_obj"=model_obj, 
              "infer_obj"=infer_obj,
              "perform_obj"=perform_obj,
              "dof_obj"=init_obj$spearman2_rho))
  
}
