front_multi_regression <- function(
  data,
  dict_data,
  y_label, 
  cluster_label,
  x_labels_linear = c(),
  x_labels_nonlin_rcs5 = c(), 
  x_labels_nonlin_rcs4 = c(),
  x_labels_nonlin_rcs3 = c(),
  x_labels_fct = c(), 
  x_labels_tag = c(), 
  x_labels = unique(c(x_labels_linear,x_labels_nonlin_rcs5,x_labels_nonlin_rcs4,x_labels_nonlin_rcs3,x_labels_fct,x_labels_tag)), 
  # --- engineer ---
  trim_by_label=NULL, # reltive time info
  trim_vec = c(-Inf, Inf), # trim relative time [from, to)
  time_unit = 1, # the increment scale of relative time
  pctcut_num_labels = c(),
  pctcut_num_vec = c(0.1, 99.9),
  pctcut_num_coerce=TRUE,
  filter_tag_labels=c(),
  winsorizing=TRUE,
  aggregate_per=c("row", "cluster_trim_by_unit", "cluster")[1],
  imputation=c("None","Mean", "Median", "Zero")[1],
  imputeby_median = c(),
  imputeby_zero = c(),
  imputeby_mean = c(), 
  impute_per_cluster=FALSE,
  standardize_df = NULL, # data.frame(varname=c(), center=c(), scale=c())
  # --- local ---
  trim_ctrl = TRUE,
  r2=0.9,
  rcs5_low="70%",
  rcs4_low="50%",
  cv_nfold=5, 
  na_frac_max=0.3, 
  test_data=NULL, 
  joint_col2_label=c("None")[1],
  stratified_cv=TRUE,
  r_abs=0.8, 
  type=c("pearson","spearman")[1],
  rank=TRUE,
  seed_value=333,
  fix_knots = TRUE,
  y_map_func=c("fold_risk", "probability", "log_odds")[1],
  y_map_max=3,
  tune_by=c("logloss","auroc","aic","bic")[1],
  lambda_value = NULL,
  return_performance = TRUE,
  return_fitted_effect=FALSE,
  return_scores_plot = TRUE, # feature permutation importance scores
  fold_idx_df_ex=NULL  # data.frame(cluster_col = c(), fold = c())
){
  
  
  set.seed(seed = seed_value)
  tryCatch({
    data <- assign.dict(data, dict_data, overwrite = TRUE)
    dict_data <- get.dict(data)
  },error=function(e){
    print("--- Skip refine dictionary from data ---")
    print(e)
  })
   
  # ---- translate front end labels to column names ----
  x_cols <- dict_data$varname[which(dict_data$label%in%x_labels)]
  x_cols_linear <- dict_data$varname[which(dict_data$label%in%x_labels_linear&dict_data$mlrole=="input"&dict_data$type=="num")] # linear numeric columns
  x_cols_nonlin_rcs5 <- dict_data$varname[which(dict_data$label%in%x_labels_nonlin_rcs5&dict_data$mlrole=="input"&dict_data$type=="num")]
  x_cols_nonlin_rcs4 <- dict_data$varname[which(dict_data$label%in%x_labels_nonlin_rcs4&dict_data$mlrole=="input"&dict_data$type=="num")]
  x_cols_nonlin_rcs3 <- dict_data$varname[which(dict_data$label%in%x_labels_nonlin_rcs3&dict_data$mlrole=="input"&dict_data$type=="num")]
  x_cols_fct <- dict_data$varname[which(dict_data$label%in%x_labels_fct & dict_data$type=="fct" & dict_data$unit!="tag01" & dict_data$mlrole=="input")]
  x_cols_tag <- dict_data$varname[which(dict_data$label%in%x_labels_tag & dict_data$type=="fct" & dict_data$unit=="tag01" & dict_data$mlrole=="input")]
  y_col_tag <- dict_data$varname[which(dict_data$label==y_label & dict_data$type=="fct" & dict_data$unit=="tag01" )]
  y_col_num <- dict_data$varname[which(dict_data$label==y_label & dict_data$type=="num" )]
  y_col <- union(y_col_tag, y_col_num)
  cluster_col <- dict_data$varname[which(dict_data$label==cluster_label)]
  rel_time_col <- dict_data$varname[which(dict_data$label==trim_by_label)]
  trim_by_col <- dict_data$varname[which(dict_data$label==trim_by_label)]
  joint_col2 <- dict_data$varname[which(dict_data$label==joint_col2_label)]
  if(length(joint_col2)==0) joint_col2 <- NULL
  num_cols <- intersect(union(y_col_num,x_cols), dict_data$varname[which(dict_data$mlrole=="input"&dict_data$type=="num")])
  fct_cols <- setdiff(union(y_col_tag, x_cols), num_cols)
  pctcut_num_cols <- dict_data$varname[which(dict_data$label%in%pctcut_num_labels)]
  filter_tag_cols <- dict_data$varname[which(dict_data$label%in%filter_tag_labels)]
  
  # ---- engineering ----
  data_in <- NULL
  data_inorg <- NULL
  data_ex <- NULL
  data_exorg <- NULL
  # ---- prepare original / no-engineering train and validation dataset (internal)  ----
  print("---- prepare original / no-engineering train and validation dataset (internal)  ----")
  tryCatch({
    data_inorg <- engineer(data = data,
                           num_cols = num_cols,
                           fct_cols = fct_cols,
                           cluster_col = cluster_col,
                           trim_by_col = trim_by_col,
                           standardize_df = standardize_df) # minimum data engineering = using default settings
    data_inorg <- assign.dict(data_inorg, dict_data)
  },error=function(e){
    print("Error!")
    print(e)
  })
  
  # ---- prepare engineered training and validation dataset (internal) ----
  print("---- prepare engineered train and validation dataset (internal) ----")
  tryCatch({
    if(trim_ctrl){
      data_in <- engineer(data = data,
                          num_cols = num_cols,
                          fct_cols = fct_cols,
                          cluster_col = cluster_col,
                          trim_by_col = trim_by_col,
                          trim_min = trim_vec[1],
                          trim_max = trim_vec[2],
                          trim_step_size = time_unit,
                          pctcut_num_cols = pctcut_num_cols,
                          pctcut_num_vec = pctcut_num_vec,
                          pctcut_num_coerce = pctcut_num_coerce,
                          filter_tag_cols = filter_tag_cols,
                          imputation = imputation,
                          imputeby_median = imputeby_median,
                          imputeby_zero = imputeby_zero,
                          imputeby_mean = imputeby_mean, 
                          impute_per_cluster = impute_per_cluster,
                          winsorizing = winsorizing,
                          aggregate_per = aggregate_per,
                          standardize_df = standardize_df)
    }else{
      if (all(unique(as.character(data[,y_col])) %in% c(1,0,NA))){
        data_event <- engineer(data = data[which(data[,y_col]==1),],
                               num_cols = num_cols,
                               fct_cols = fct_cols,
                               cluster_col = cluster_col,
                               trim_by_col = trim_by_col,
                               trim_min=trim_vec[1],
                               trim_max=trim_vec[2],
                               trim_step_size = time_unit,
                               pctcut_num_cols = pctcut_num_cols,
                               pctcut_num_vec = pctcut_num_vec,
                               pctcut_num_coerce = pctcut_num_coerce,
                               filter_tag_cols = filter_tag_cols,
                               imputation = imputation,
                               imputeby_median = imputeby_median,
                               imputeby_zero = imputeby_zero,
                               imputeby_mean = imputeby_mean, 
                               impute_per_cluster = impute_per_cluster,
                               winsorizing = winsorizing,
                               aggregate_per = aggregate_per,
                               standardize_df = standardize_df)
        data_cntrl <- engineer(data = data[which(data[,y_col]==0),],
                               num_cols = num_cols,
                               fct_cols = fct_cols,
                               cluster_col = cluster_col,
                               trim_by_col = trim_by_col,
                               trim_min=-Inf,
                               trim_max=Inf,
                               trim_step_size = time_unit,
                               trim_keepna = TRUE,
                               pctcut_num_cols = pctcut_num_cols,
                               pctcut_num_vec = pctcut_num_vec,
                               pctcut_num_coerce = pctcut_num_coerce,
                               filter_tag_cols = filter_tag_cols,
                               imputation = imputation,
                               imputeby_median = imputeby_median,
                               imputeby_zero = imputeby_zero,
                               imputeby_mean = imputeby_mean, 
                               impute_per_cluster = impute_per_cluster,
                               winsorizing = winsorizing,
                               aggregate_per = aggregate_per,
                               standardize_df = standardize_df)
        data_in <- bind_rows(data_cntrl, data_event)
      }
    }
    data_in <- assign.dict(data_in, dict_data)
  },error=function(e){
    print("Error!")
    print(e)
  })
  
  # ---- prepare original / no-engineering test dataset (external)  ----
  print("---- prepare original / no-engineering test dataset (external)  ----")
  tryCatch({
    if (!is.null(test_data)){# if test_data given
      data_exorg <- engineer(data = test_data,
                             num_cols = num_cols,
                             fct_cols = fct_cols,
                             cluster_col = cluster_col,
                             trim_by_col = trim_by_col,
                             standardize_df = standardize_df)
      data_exorg <- assign.dict(data_exorg, dict_data)
    }
  },error=function(e){
    print("Error!")
    print(e)
  })
  
  # ---- prepare engineered test dataset (external)  ----
  print("---- prepare engineered test dataset (external)  ----")
  tryCatch({
    if (!is.null(test_data)){
      if(trim_ctrl){
        data_ex <- engineer(data = test_data,
                            num_cols = num_cols,
                            fct_cols = fct_cols,
                            cluster_col = cluster_col,
                            trim_by_col = trim_by_col,
                            trim_min=trim_vec[1],
                            trim_max=trim_vec[2],
                            trim_step_size = time_unit,
                            pctcut_num_cols = pctcut_num_cols,
                            pctcut_num_vec = pctcut_num_vec,
                            pctcut_num_coerce = pctcut_num_coerce,
                            filter_tag_cols = filter_tag_cols,
                            imputation = imputation,
                            imputeby_median = imputeby_median,
                            imputeby_zero = imputeby_zero,
                            imputeby_mean = imputeby_mean, 
                            impute_per_cluster = impute_per_cluster,
                            winsorizing = winsorizing,
                            aggregate_per = aggregate_per,
                            standardize_df = standardize_df)
      }else{
        if (all(unique(as.character(test_data[,y_col])) %in% c(1,0,NA))){
          data_event <- engineer(data = test_data[which(test_data[,y_col]==1),],
                                 num_cols = num_cols,
                                 fct_cols = fct_cols,
                                 cluster_col = cluster_col,
                                 trim_by_col = trim_by_col,
                                 trim_min=trim_vec[1],
                                 trim_max=trim_vec[2],
                                 trim_step_size = time_unit,
                                 pctcut_num_cols = pctcut_num_cols,
                                 pctcut_num_vec = pctcut_num_vec,
                                 pctcut_num_coerce = pctcut_num_coerce,
                                 filter_tag_cols = filter_tag_cols,
                                 imputation = imputation,
                                 imputeby_median = imputeby_median,
                                 imputeby_zero = imputeby_zero,
                                 imputeby_mean = imputeby_mean, 
                                 impute_per_cluster = impute_per_cluster,
                                 winsorizing = winsorizing,
                                 aggregate_per = aggregate_per,
                                 standardize_df = standardize_df)
          data_cntrl <- engineer(data = test_data[which(test_data[,y_col]==0),],
                                 num_cols = num_cols,
                                 fct_cols = fct_cols,
                                 cluster_col = cluster_col,
                                 trim_by_col = trim_by_col,
                                 trim_min=-Inf,
                                 trim_max=Inf,
                                 trim_step_size = time_unit,
                                 trim_keepna = TRUE,
                                 pctcut_num_cols = pctcut_num_cols,
                                 pctcut_num_vec = pctcut_num_vec,
                                 pctcut_num_coerce = pctcut_num_coerce,
                                 filter_tag_cols = filter_tag_cols,
                                 imputation = imputation,
                                 imputeby_median = imputeby_median,
                                 imputeby_zero = imputeby_zero,
                                 imputeby_mean = imputeby_mean, 
                                 impute_per_cluster = impute_per_cluster,
                                 winsorizing = winsorizing,
                                 aggregate_per = aggregate_per,
                                 standardize_df = standardize_df)
          data_ex <- bind_rows(data_cntrl, data_event)
        }
      }
      data_ex <- assign.dict(data_ex, dict_data)
    }
  },error=function(e){
    print("Error!")
    print(e)
  })
  
  
  # ---- modeling ---- 
  print("---- modeling ----")
  if(dict_data[y_col, "type"]=="fct"){
    print("--- do_lrm_pip ---")
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
                          data_org=data_inorg,
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
                          tune_by = tune_by,
                          lambda_value = lambda_value,
                          trim_by_col = trim_by_col,
                          return_performance = return_performance,
                          return_fitted_effect = return_fitted_effect,
                          return_scores_plot = return_scores_plot,
                          fold_idx_df_ex = fold_idx_df_ex)
    
  }else if(dict_data[y_col, "type"]=="num"){
    print("--- do_ols_pip ---")
    results <- do_ols_pip(data=data,
                          dict_data=dict_data,
                          x_cols=x_cols,
                          y_col=y_col,
                          cluster_col=cluster_col,
                          r2=r2,
                          rcs5_low=rcs5_low,
                          rcs4_low=rcs4_low,
                          cv_nfold=cv_nfold,
                          test_data=test_data,
                          na_frac_max=na_frac_max,
                          joint_col2=joint_col2,
                          stratified_cv=stratified_cv,
                          r_abs=r_abs,
                          type=type,
                          rank=rank)
    
  }
  
  # reorganize dof term and x predictor name information in a table
  devel_model_info_tbl <- results$model_obj$cv_obj$model_info
  formula_string <- devel_model_info_tbl$formula
  y_varname <- stringr::str_split_fixed(formula_string,"\\~", 2)[,1]
  y_varname <- gsub(" ","",y_varname)
  x_varname_string <- stringr::str_split_fixed(formula_string,"\\~", 2)[,2]
  x_var_list <- unlist(stringr::str_split(x_varname_string, "\\+"))
  x_var_list <- gsub(" ","",x_var_list)
  devel_vars_selected <- data.frame()
  x_var_linear <- gsub("\\)","", gsub("I\\(","", x_var_list[which(startsWith( x_var_list,"I(") )]))
  x_var_linear <- data.frame(raw_vars_selected=x_var_linear)
  if(nrow(x_var_linear)>0){
    x_var_linear$term <- "linear"
    devel_vars_selected <- bind_rows(devel_vars_selected, x_var_linear)
  }
  x_var_rcs3 <- gsub("\\,3\\)","", gsub("rcs\\(","", x_var_list[which(startsWith( x_var_list,"rcs(") & endsWith( x_var_list,",3)") )]))
  x_var_rcs3 <- data.frame(raw_vars_selected=x_var_rcs3)
  if(nrow(x_var_rcs3)>0){
    x_var_rcs3$term <- "rcs3"
    devel_vars_selected <- bind_rows(devel_vars_selected, x_var_rcs3)
  }
  x_var_rcs4 <- gsub("\\,4\\)","", gsub("rcs\\(","", x_var_list[which(startsWith( x_var_list,"rcs(") & endsWith( x_var_list,",4)") )]))
  x_var_rcs4 <- data.frame(raw_vars_selected=x_var_rcs4)
  if(nrow(x_var_rcs4)>0){
    x_var_rcs4$term <- "rcs4"
    devel_vars_selected <- bind_rows(devel_vars_selected, x_var_rcs4)
  }
  x_var_rcs5 <- gsub("\\,5\\)","", gsub("rcs\\(","", x_var_list[which(startsWith( x_var_list,"rcs(") & endsWith( x_var_list,",5)") )]))
  x_var_rcs5 <- data.frame(raw_vars_selected=x_var_rcs5)
  if(nrow(x_var_rcs5)>0){
    x_var_rcs5$term <- "rcs5"
    devel_vars_selected <- bind_rows(devel_vars_selected, x_var_rcs5)
  }
  x_var_fct <- gsub("\\)","", gsub("catg\\(as.factor\\(","", x_var_list[which(startsWith( x_var_list,"catg(as.factor(") )]))
  x_var_fct <- data.frame(raw_vars_selected=x_var_fct)
  if(nrow(x_var_fct)>0){
    x_var_fct$term <- "fct"
    devel_vars_selected <- bind_rows(devel_vars_selected, x_var_fct)
  }
  
  
  # ---- return ----
  #  model development reports 
  devel_vars_selected_tbl <- devel_vars_selected
  devel_score_summ_tbl <- results$model_obj$cv_obj$model_scores
  devel_score_summ_tbl <- devel_score_summ_tbl[,intersect(colnames(devel_score_summ_tbl), union(c("success_nfold","train_AUROC_mean","valid_AUROC_mean","train_AUROC_se","valid_AUROC_se"), colnames(devel_score_summ_tbl)) )]
  devel_cali_plot <- results$model_obj$cv_obj$calibration_curve
  devel_cv_eval_trace_tbl <- results$model_obj$cv_obj$cv_eval_trace # trace of score per fold while cross validation
  devel_final_model_obj <- results$model_obj$mdl_obj # final model object trained on all data, for print, anova and save locally
  devel_penal_trace_tbl <- results$model_obj$score_trace_df # trace of score while tuning penalty
  
  # model inference reports
  infer_effect_plot_1d <- results$infer_obj$eff_plot_1d
  infer_effect_plot_2d <- results$infer_obj$eff_plot_2d
  infer_effect_plot_list <- results$infer_obj$eff_plot
  infer_effect_plot_diy_list <- results$infer_obj$eff_plot_diy
  
  # model performance reports on new dataset 
  perform_in_df_hat <- results$perform_obj$internal$df_hat
  perform_inorg_df_hat <- results$perform_obj$internal_org$df_hat
  perform_ex_df_hat <- results$perform_obj$external$df_hat
  perform_exorg_df_hat <- results$perform_obj$external_org$df_hat
  
  perform_in_fitted_eff_plot <- results$perform_obj$internal$fitted_eff_plot
  perform_inorg_fitted_eff_plot <- results$perform_obj$internal_org$fitted_eff_plot
  perform_ex_fitted_eff_plot <- results$perform_obj$external$fitted_eff_plot
  perform_exorg_fitted_eff_plot <- results$perform_obj$external_org$fitted_eff_plot
  
  perform_in_cali_plot <- results$perform_obj$internal$cali_plot
  perform_inorg_cali_plot <- results$perform_obj$internal_org$cali_plot
  perform_ex_cali_plot <- results$perform_obj$external$cali_plot
  perform_exorg_cali_plot <- results$perform_obj$external_org$cali_plot
  
  perform_in_tte_plot <- results$perform_obj$internal$tte_plot
  perform_inorg_tte_plot <- results$perform_obj$internal_org$tte_plot
  perform_ex_tte_plot <- results$perform_obj$external$tte_plot
  perform_exorg_tte_plot <- results$perform_obj$external_org$tte_plot
  
  perform_in_scores_plot <- results$perform_obj$internal$scores_plot
  perform_inorg_scores_plot <- results$perform_obj$internal_org$scores_plot
  perform_ex_scores_plot <- results$perform_obj$external$scores_plot
  perform_exorg_scores_plot<- results$perform_obj$external_org$scores_plot
  
  perform_in_scores_tbl <- results$perform_obj$internal$scores_all_final
  perform_inorg_scores_tbl <- results$perform_obj$internal_org$scores_all_final
  perform_ex_scores_tbl <- results$perform_obj$external$scores_all_final
  perform_exorg_scores_tbl <- results$perform_obj$external_org$scores_all_final
  
  perform_in_tradeoff_plot <- results$perform_obj$internal$tradeoff_plot
  perform_inorg_tradeoff_plot <- results$perform_obj$internal_org$tradeoff_plot
  perform_ex_tradeoff_plot <- results$perform_obj$external$tradeoff_plot
  perform_exorg_tradeoff_plot <- results$perform_obj$external_org$tradeoff_plot
  
  return(list( model_obj = results$model_obj,
               infer_obj = results$infer_obj,
               perform_obj = results$perform_obj,
               devel_model_info_tbl = devel_model_info_tbl,
               devel_vars_selected_tbl = devel_vars_selected_tbl,
               devel_score_summ_tbl = devel_score_summ_tbl,
               devel_cali_plot = devel_cali_plot,
               devel_cv_eval_trace_tbl = devel_cv_eval_trace_tbl,
               devel_penal_trace_tbl = devel_penal_trace_tbl,
               devel_final_model_obj = devel_final_model_obj,
               infer_effect_plot_1d = infer_effect_plot_1d,
               infer_effect_plot_2d = infer_effect_plot_2d,
               infer_effect_plot_list = infer_effect_plot_list,
               infer_effect_plot_diy_list = infer_effect_plot_diy_list,
               perform_in_df_hat = perform_in_df_hat,
               perform_inorg_df_hat = perform_inorg_df_hat,
               perform_ex_df_hat = perform_ex_df_hat,
               perform_exorg_df_hat = perform_exorg_df_hat, 
               perform_in_fitted_eff_plot = perform_in_fitted_eff_plot,
               perform_inorg_fitted_eff_plot = perform_inorg_fitted_eff_plot,
               perform_ex_fitted_eff_plot =perform_ex_fitted_eff_plot,
               perform_exorg_fitted_eff_plot = perform_exorg_fitted_eff_plot,
               perform_in_tte_plot = perform_in_tte_plot,
               perform_inorg_tte_plot = perform_inorg_tte_plot,
               perform_ex_tte_plot = perform_ex_tte_plot,
               perform_exorg_tte_plot = perform_exorg_tte_plot,
               perform_in_cali_plot = perform_in_cali_plot,
               perform_inorg_cali_plot = perform_inorg_cali_plot,
               perform_ex_cali_plot =perform_ex_cali_plot,
               perform_exorg_cali_plot = perform_exorg_cali_plot,
               perform_in_scores_plot = perform_in_scores_plot,
               perform_inorg_scores_plot = perform_inorg_scores_plot,
               perform_ex_scores_plot = perform_ex_scores_plot,
               perform_exorg_scores_plot = perform_exorg_scores_plot,
               perform_in_scores_tbl = perform_in_scores_tbl,
               perform_inorg_scores_tbl = perform_inorg_scores_tbl,
               perform_ex_scores_tbl = perform_ex_scores_tbl,
               perform_exorg_scores_tbl = perform_exorg_scores_tbl,
               perform_in_tradeoff_plot = perform_in_tradeoff_plot,
               perform_inorg_tradeoff_plot = perform_inorg_tradeoff_plot,
               perform_ex_tradeoff_plot = perform_ex_tradeoff_plot,
               perform_exorg_tradeoff_plot = perform_exorg_tradeoff_plot))

}



# ############################################# not run #############################################
# data = data_ml
# dict_data = dict_ml
# y_label = "Primary outcome (EN) == Unfavorable"
# cluster_label = "PreVent study ID"
# x_labels_linear = c("Gestational Age", "pH associated with highest CO2 on blood gas")
# x_labels_nonlin_rcs5 = c("Maternal age")
# x_labels_nonlin_rcs4 = c("Gestational Age")
# x_labels_nonlin_rcs3 = c("Birth weight")
# x_labels_fct = c("Site (EN)")
# x_labels_tag = c("Baby Gender (EN) == Female")
# x_labels = unique(c(x_labels_linear,x_labels_nonlin_rcs5,x_labels_nonlin_rcs4,x_labels_nonlin_rcs3,x_labels_fct,x_labels_tag))
# # --- engineer ---
# trim_by_label = "Post-menstrual Age"  # reltive time info
# trim_vec = c(-Inf, Inf) # trim relative time [from, to)
# time_unit = 7 # the increment scale of relative time
# trim_ctrl = TRUE
# pctcut_num_labels = c("PeriodicBreathing_v3 duration per day", "ABD_v3 number of events per day")
# pctcut_num_vec = c(0.1, 99.9)
# pctcut_num_coerce=TRUE
# filter_tag_labels=c("On respiratory support with endotracheal tube (EN) == Yes", "Any  Doses of any medication today")
# imputation=c("None","Mean", "Median", "Zero")[1]
# impute_per_cluster=FALSE
# winsorizing=TRUE
# aggregate_per=c("row", "cluster_trim_by_unit", "cluster")[2]
# # --- local ---
# r2=0.9
# rcs5_low="70%"
# rcs4_low="50%"
# cv_nfold=5
# na_frac_max=0.3
# test_data=NULL
# joint_col2_label="Gestational Age"
# stratified_cv=TRUE
# r_abs=0.8
# type=c("pearson","spearman")[1]
# rank=TRUE
# seed_value=333
# fix_knots = TRUE
# y_map_func=c("fold_risk", "probability", "log_odds")[1]
# y_map_max=3
# tune_by ="auroc"


