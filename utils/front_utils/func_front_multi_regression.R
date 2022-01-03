front_multi_regression <- function(
  data = data_ml,
  dict_data=dict_ml,
  trim_by_label="Post-menstrual Age",
  trim_vec = c(22, 40),
  time_unit=7, # each row in the dataset is in the increment of which number
  x_labels_linear=c("Gestational Age", "pH associated with highest CO2 on blood gas"),
  x_labels_nonlin_rcs5=c("Maternal age"),
  x_labels_nonlin_rcs4=c("Gestational Age"),
  x_labels_nonlin_rcs3=c("Birth weight"),
  x_labels_fct = c("Site (EN)"),
  x_labels_tag = c("Baby Gender (EN)___Female"),
  x_labels=unique(c(x_labels_linear,x_labels_nonlin_rcs5,x_labels_nonlin_rcs4,x_labels_nonlin_rcs3,x_labels_fct,x_labels_tag)), 
  y_label="Primary outcome (EN)___Unfavorable", 
  cluster_label="PreVent study ID",
  r2=0.9,
  rcs5_low="70%",
  rcs4_low="50%",
  cv_nfold=5, 
  na_frac_max=0.3, 
  test_data=NULL, 
  joint_col2_label=c("None","Gestational Age")[1],
  imputation=c("None","Mean", "Median", "Zero")[1],
  impute_per_cluster=TRUE,
  winsorizing=TRUE,
  aggregation=TRUE,
  stratified_cv=TRUE,
  r_abs=0.8, 
  type=c("pearson","spearman")[1],
  rank=TRUE,
  seed_value=333,
  fix_knots = TRUE,
  trim_ctrl=TRUE,
  y_map_func=c("fold_risk", "probability", "log_odds")[1],
  y_map_max=3
){
  
  
  set.seed(seed = seed_value)
  data <- assign.dict(data, dict_data, overwrite = TRUE)
  dict_data <- get.dict(data)
  
  # --- group column names ---
  x_cols <- rownames(dict_data[which(dict_data$label_front%in%x_labels), ])
  x_cols_linear <- rownames(dict_data[which(dict_data$label_front%in%x_labels_linear&dict_data$mlrole=="input"&dict_data$type=="num"), ]) # linear numeric columns
  x_cols_nonlin_rcs5 <- rownames(dict_data[which(dict_data$label_front%in%x_labels_nonlin_rcs5&dict_data$mlrole=="input"&dict_data$type=="num"), ])
  x_cols_nonlin_rcs4 <- rownames(dict_data[which(dict_data$label_front%in%x_labels_nonlin_rcs4&dict_data$mlrole=="input"&dict_data$type=="num"), ])
  x_cols_nonlin_rcs3 <- rownames(dict_data[which(dict_data$label_front%in%x_labels_nonlin_rcs3&dict_data$mlrole=="input"&dict_data$type=="num"), ])
  x_cols_fct <- rownames(dict_data[which(dict_data$label_front%in%x_labels_fct & dict_data$type=="fct" & dict_data$unit!="tag01" & dict_data$mlrole=="input"), ])
  x_cols_tag <- rownames(dict_data[which(dict_data$label_front%in%x_labels_tag & dict_data$type=="fct" & dict_data$unit=="tag01" & dict_data$mlrole=="input"), ])
  y_col_tag <- rownames(dict_data[which(dict_data$label_front==y_label & dict_data$type=="fct" & dict_data$unit=="tag01" ),])
  y_col_num <- rownames(dict_data[which(dict_data$label_front==y_label & dict_data$type=="num" ),])
  y_col <- union(y_col_tag, y_col_num)
  cluster_col <- rownames(dict_data[which(dict_data$label_front==cluster_label),])
  rel_time_col <- rownames(dict_data[which(dict_data$label_front==trim_by_label),])
  trim_by_col <- rownames(dict_data[which(dict_data$label_front==trim_by_label), ])
  joint_col2 <- rownames(dict_data[which(dict_data$label_front==joint_col2_label),])
  if(length(joint_col2)==0) joint_col2 <- NULL
  num_cols <- intersect(union(y_col_num,x_cols), rownames(dict_data[which(dict_data$mlrole=="input"&dict_data$type=="num"), ]))
  fct_cols <- setdiff(union(y_col_tag, x_cols), num_cols)
  
  # --- preprocessing ---
  data_in <- NULL
  data_inorg <- NULL
  data_ex <- NULL
  data_exorg <- NULL
  # ---- prepare original / no-engineering training and validation dataset (internal data)  ----
  if (all(unique(as.character(data[,y_col])) %in% c(1,0,NA))){
    data_org <- engineer(data = data,
                           trim_by_col = trim_by_col,
                           trim_min=-Inf,
                           trim_max=Inf,
                           num_cols = num_cols,
                           fct_cols = fct_cols,
                           cluster_col = cluster_col,
                           imputation = "None",
                           impute_per_cluster = FALSE,
                           winsorizing = FALSE,
                           aggregation = FALSE)
  }
  data_inorg <- assign.dict(data_org, dict_data)
  # ---- prepare engineered training and validation dataset (internal data) ----
  if(trim_ctrl){
    data <- engineer(data = data,
                     trim_by_col = trim_by_col,
                     trim_min=trim_vec[1]*time_unit,
                     trim_max=trim_vec[2]*time_unit,
                     num_cols = num_cols,
                     fct_cols = fct_cols,
                     cluster_col = cluster_col,
                     imputation = imputation,
                     impute_per_cluster = impute_per_cluster,
                     winsorizing = winsorizing,
                     aggregation = aggregation)
  }else{
    data_event <- engineer(data = data[which(data[,y_col]==1),],
                     trim_by_col = trim_by_col,
                     trim_min=trim_vec[1]*time_unit,
                     trim_max=trim_vec[2]*time_unit,
                     num_cols = num_cols,
                     fct_cols = fct_cols,
                     cluster_col = cluster_col,
                     imputation = imputation,
                     impute_per_cluster = impute_per_cluster,
                     winsorizing = winsorizing,
                     aggregation = aggregation)
    data_cntrl <- engineer(data = data[which(data[,y_col]==0),],
                           trim_by_col = trim_by_col,
                           trim_min=-Inf,
                           trim_max=Inf,
                           trim_keepna = TRUE,
                           num_cols = num_cols,
                           fct_cols = fct_cols,
                           cluster_col = cluster_col,
                           imputation = imputation,
                           impute_per_cluster = impute_per_cluster,
                           winsorizing = winsorizing,
                           aggregation = aggregation)
    data <- bind_rows(data_cntrl, data_event)
  }
  data_in <- assign.dict(data, dict_data)
  
  # ---- prepare original / no-engineering test dataset (external data)  ----
  if (!is.null(test_data)){# if test_data given
   data <- test_data
   if (all(unique(as.character(data[,y_col])) %in% c(1,0,NA))){
     data_org <- engineer(data = data,
                          trim_by_col = trim_by_col,
                          trim_min=-Inf,
                          trim_max=Inf,
                          num_cols = num_cols,
                          fct_cols = fct_cols,
                          cluster_col = cluster_col,
                          imputation = "None",
                          impute_per_cluster = FALSE,
                          winsorizing = FALSE,
                          aggregation = FALSE)
   }
   data_exorg <- assign.dict(data_org, dict_data)
  }
  # ---- prepare engineered test dataset (external data)  ----
  if (!is.null(test_data)){
    data <- test_data
    if(trim_ctrl){
      data <- engineer(data = data,
                       trim_by_col = trim_by_col,
                       trim_min=trim_vec[1]*time_unit,
                       trim_max=trim_vec[2]*time_unit,
                       num_cols = num_cols,
                       fct_cols = fct_cols,
                       cluster_col = cluster_col,
                       imputation = imputation,
                       impute_per_cluster = impute_per_cluster,
                       winsorizing = winsorizing,
                       aggregation = aggregation)
    }else{
      data_event <- engineer(data = data[which(data[,y_col]==1),],
                             trim_by_col = trim_by_col,
                             trim_min=trim_vec[1]*time_unit,
                             trim_max=trim_vec[2]*time_unit,
                             num_cols = num_cols,
                             fct_cols = fct_cols,
                             cluster_col = cluster_col,
                             imputation = imputation,
                             impute_per_cluster = impute_per_cluster,
                             winsorizing = winsorizing,
                             aggregation = aggregation)
      data_cntrl <- engineer(data = data[which(data[,y_col]==0),],
                             trim_by_col = trim_by_col,
                             trim_min=-Inf,
                             trim_max=Inf,
                             trim_keepna = TRUE,
                             num_cols = num_cols,
                             fct_cols = fct_cols,
                             cluster_col = cluster_col,
                             imputation = imputation,
                             impute_per_cluster = impute_per_cluster,
                             winsorizing = winsorizing,
                             aggregation = aggregation)
      data <- bind_rows(data_cntrl, data_event)
    }
    data_ex <- assign.dict(data, dict_data)
  }
  
  
  
  #  ---- modeling ---- 
  if(dict_data[y_col, "type"]=="fct"){
    results <- do_lrm_pip(data=data_in, 
                          dict_data=dict_data,
                          x_cols_linear=x_cols_linear,
                          x_cols_nonlin_rcs5=x_cols_nonlin_rcs5,
                          x_cols_nonlin_rcs4=x_cols_nonlin_rcs4,
                          x_cols_nonlin_rcs3=x_cols_nonlin_rcs3,
                          x_cols_fct=x_cols_fct,
                          x_cols_tag=x_cols_tag,
                          x_cols=x_cols, 
                          y_col=y_col, 
                          cluster_col=cluster_col,
                          r2=r2,
                          rcs5_low=rcs5_low,
                          rcs4_low=rcs4_low,
                          cv_nfold=cv_nfold,
                          data_org = data_org,
                          test_data=data_ex, 
                          test_data_org=data_exorg,
                          na_frac_max=na_frac_max, 
                          joint_col2=joint_col2,
                          stratified_cv=stratified_cv,
                          r_abs=r_abs, 
                          type=type,
                          rank=rank,
                          fix_knots=fix_knots,
                          y_map_func=y_map_func,
                          y_map_max=y_map_max,
                          trim_by_col = trim_by_col)
    
  }else if(dict_data[y_col, "type"]=="num"){
    # results <- do_ols_pip(data=data, 
    #                       dict_data=dict_data,
    #                       x_cols=x_cols, 
    #                       y_col=y_col, 
    #                       cluster_col=cluster_col,
    #                       r2=r2,
    #                       rcs5_low=rcs5_low,
    #                       rcs4_low=rcs4_low,
    #                       cv_nfold=cv_nfold,
    #                       test_data=test_data, 
    #                       na_frac_max=na_frac_max, 
    #                       joint_col2=joint_col2,
    #                       stratified_cv=stratified_cv,
    #                       r_abs=r_abs, 
    #                       type=type,
    #                       rank=rank)
    
  }
  
  # ---- return ----
  
  #  model development reports 
  devel_model_info_tbl <- results$model_obj$cv_obj$model_info
  devel_score_summ_tbl <- results$model_obj$cv_obj$model_scores
  devel_score_summ_tbl <- devel_score_summ_tbl[,union(c("success_nfold","train_AUROC_mean","valid_AUROC_mean","train_AUROC_se","valid_AUROC_se"), colnames(devel_score_summ_tbl))]
  devel_cali_plot <- results$model_obj$cv_obj$calibration_curve
  devel_cv_eval_trace_tbl <- results$model_obj$cv_obj$cv_eval_trace
  devel_final_model_obj <- results$model_obj$mdl_obj # for print, anova and save locally
  
  # model inference reports
  infer_effect_plot_1d <- results$infer_obj$eff_plot_1d
  infer_effect_plot_2d <- results$infer_obj$eff_plot_2d
  
  # model performance reports on new dataset 
  perform_in_df_hat <- results$perform_obj$internal$df_hat
  perform_inorg_df_hat <- results$perform_obj$internal_org$df_hat
  perform_ex_df_hat <- results$perform_obj$external$df_hat
  perform_exorg_df_hat <- results$perform_obj$external_org$df_hat
  
  perform_in_fitted_eff_plot <- results$perform_obj$internal$fitted_eff_plot
  perform_inorg_fitted_eff_plot <- results$perform_obj$internal_org$fitted_eff_plot
  perform_ex_fitted_eff_plot <- results$perform_obj$external$fitted_eff_plot
  perform_exorg_fitted_eff_plot <- results$perform_obj$external_org$fitted_eff_plot
  
  perform_in_scores_plot <- results$perform_obj$internal$scores_plot
  perform_inorg_scores_plot <- results$perform_obj$internal_org$scores_plot
  perform_ex_scores_plot <- results$perform_obj$external$scores_plot
  perform_exorg_scores_plot<- results$perform_obj$external_org$scores_plot
  
  perform_in_scores_tbl <- results$perform_obj$internal$scores_all_final
  perform_inorg_scores_tbl <- results$perform_obj$internal_org$scores_all_final
  perform_ex_scores_tbl <- results$perform_obj$external$scores_all_final
  perform_exorg_scores_tbl <- results$perform_obj$external_org$scores_all_final
  
  
  return(list( devel_model_info_tbl = devel_model_info_tbl,
               devel_score_summ_tbl = devel_score_summ_tbl,
               devel_cali_plot = devel_cali_plot,
               devel_cv_eval_trace_tbl = devel_cv_eval_trace_tbl,
               devel_final_model_obj = devel_final_model_obj,
               infer_effect_plot_1d = infer_effect_plot_1d,
               infer_effect_plot_2d = infer_effect_plot_2d,
               perform_in_df_hat = perform_in_df_hat,
               perform_inorg_df_hat = perform_inorg_df_hat,
               perform_ex_df_hat = perform_ex_df_hat,
               perform_exorg_df_hat = perform_exorg_df_hat, 
               perform_in_fitted_eff_plot = perform_in_fitted_eff_plot,
               perform_inorg_fitted_eff_plot = perform_inorg_fitted_eff_plot,
               perform_ex_fitted_eff_plot =perform_ex_fitted_eff_plot,
               perform_exorg_fitted_eff_plot = perform_exorg_fitted_eff_plot,
               perform_in_scores_plot = perform_in_scores_plot,
               perform_inorg_scores_plot = perform_inorg_scores_plot,
               perform_ex_scores_plot = perform_ex_scores_plot,
               perform_exorg_scores_plot = perform_exorg_scores_plot,
               perform_in_scores_tbl = perform_in_scores_tbl,
               perform_inorg_scores_tbl = perform_inorg_scores_tbl,
               perform_ex_scores_tbl = perform_ex_scores_tbl,
               perform_exorg_scores_tbl = perform_exorg_scores_tbl))
  
  
}
