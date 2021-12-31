front_multi_regression <- function(
  data = data_ml,
  dict_data=dict_ml,
  trim_by_label="Post-menstrual Age",
  trim_vec = c(22, 40),
  time_unit=7,
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
  joint_col2_label=c("None","Gestational Age")[2],
  imputation="None",
  impute_per_cluster=FALSE,
  winsorizing=FALSE,
  aggregation=FALSE,
  stratified_cv=TRUE,
  r_abs=0.8, 
  type=c("pearson","spearman")[1],
  rank=TRUE,
  seed_value=333,
  fix_knots = TRUE,
  trim_ctrl=TRUE,
  y_map_func=c("fold_risk", "probability", "log_odds")[1],
  y_map_max=10
){
  
  set.seed(seed = seed_value)
  
  # ---- pre-process ----
  x_cols <- rownames(dict_data[which(dict_data$label_front%in%x_labels), ])
  x_cols_linear <- rownames(dict_data[which(dict_data$label_front%in%x_labels_linear&dict_data$mlrole=="input"&dict_data$type=="num"), ]) # linear numeric columns
  x_cols_nonlin_rcs5 <- rownames(dict_data[which(dict_data$label_front%in%x_labels_nonlin_rcs5&dict_data$mlrole=="input"&dict_data$type=="num"), ])
  x_cols_nonlin_rcs4 <- rownames(dict_data[which(dict_data$label_front%in%x_labels_nonlin_rcs4&dict_data$mlrole=="input"&dict_data$type=="num"), ])
  x_cols_nonlin_rcs3 <- rownames(dict_data[which(dict_data$label_front%in%x_labels_nonlin_rcs3&dict_data$mlrole=="input"&dict_data$type=="num"), ])
  x_cols_fct <- rownames(dict_data[which(dict_data$label_front%in%x_labels_fct & dict_data$type=="fct" & dict_data$unit!="tag01" & dict_data$mlrole=="input"), ])
  x_cols_tag <- rownames(dict_data[which(dict_data$label_front%in%x_labels_tag & dict_data$type=="fct" & dict_data$unit=="tag01" & dict_data$mlrole=="input"), ])
  num_cols <- intersect(x_cols, rownames(dict_data[which(dict_data$mlrole=="input"&dict_data$type=="num"), ]))
  fct_cols <- setdiff(x_cols, num_cols)
  y_col <- rownames(dict_data[which(dict_data$label_front==y_label),])
  cluster_col <- rownames(dict_data[which(dict_data$label_front==cluster_label),])
  rel_time_col <- rownames(dict_data[which(dict_data$label_front==trim_by_label),])
  trim_by_col <- rownames(dict_data[which(dict_data$label_front==trim_by_label), ])
  joint_col2 <- rownames(dict_data[which(dict_data$label_front==joint_col2_label),])
  if(length(joint_col2)==0) joint_col2 <- NULL
  
  # keep un-engineered data 
  data_org <- data
  
  # trim time info
  if (length(trim_by_col)>0 ){
    # if there is a valid event / control group split, and user choose not to trim the control group
    if (all(unique(as.character(data[,y_col])) %in% c(1,0,NA)) & !trim_ctrl){
      data_pos <- data %>% filter(data[,y_col]==1 & data[,trim_by_col]>=trim_vec[1]*time_unit & data[,trim_by_col]<trim_vec[2]*time_unit ) %>% as.data.frame()
      data_ctrl <- data %>% filter(data[,y_col]==0) %>% as.data.frame()
      data <- bind_rows(data_pos, data_ctrl)
    }else{
      data <- data %>% filter(data[,trim_by_col]>=trim_vec[1]*time_unit & data[,trim_by_col]<trim_vec[2]*time_unit ) %>% as.data.frame()
    }
  }
  
  # add cluster id
  data$cluster_id <- data[,cluster_col]
  # impute numeric inputs
  if(!impute_per_cluster){
    if(imputation=="Mean"){
      data <- data %>% mutate_at(vars(num_cols), ~tidyr::replace_na(., mean(.,na.rm = TRUE))) %>% as.data.frame()
    }
    if(imputation=="Median"){
      data <- data %>% mutate_at(vars(num_cols), ~tidyr::replace_na(., median(.,na.rm = TRUE))) %>% as.data.frame()
    }
    if(imputation=="Zero"){
      data <- data %>% mutate_at(vars(num_cols), ~tidyr::replace_na(., 0)) %>% as.data.frame()
    }
  }else{
    if(imputation=="Mean"){
      for(sbj in unique(data[,cluster_col]) ){
        for(col in num_cols){
          data[which(data[,cluster_col]==sbj & is.na(data[,col])), col] <- mean(data[which(data[,cluster_col]==sbj), col], na.rm=TRUE)
        }
      }
    }
    if(imputation=="Median"){
      for(sbj in unique(data[,cluster_col]) ){
        for(col in num_cols){
          data[which(data[,cluster_col]==sbj & is.na(data[,col])), col] <- median(data[which(data[,cluster_col]==sbj), col], na.rm=TRUE)
        }
      }
    }
    if(imputation=="Zero"){
      for(sbj in unique(data[,cluster_col]) ){
        for(col in num_cols){
          data[which(data[,cluster_col]==sbj & is.na(data[,col])), col] <- 0
        }
      }
    }
  }
  
  # winsorize numeric columns
  if(winsorizing){
    data[,num_cols] <- winsorize(data[,num_cols])
  }
  # aggregation if required
  if(aggregation){
    df_y <- NULL
    df_fct <- NULL
    df_num <- NULL
    
    # y tag - max 
    if(dict_data[y_col, "type"]=="num"){
      df_y <- data %>% group_by(data[,cluster_col]) %>% summarise_at(vars(y_col), ~mean(.,na.rm = TRUE)) %>% as.data.frame()
      colnames(df_y) <- c(cluster_col, y_col)
    }else if(dict_data[y_col, "type"]=="fct"){
      df_y <- data %>% group_by(data[,cluster_col]) %>% summarise_at(vars(y_col), ~max(.,0,na.rm = TRUE)) %>% as.data.frame()
      colnames(df_y) <- c(cluster_col, y_col) 
    }
    # x num - mean
    if(length(num_cols)>0){
      df_num <- data %>% group_by(data[,cluster_col]) %>% summarise_at(vars(num_cols), ~mean(.,na.rm = TRUE)) %>% as.data.frame()
      colnames(df_num) <- c(cluster_col, num_cols)
    }
    #x fct - max
    if(length(fct_cols)>0){
      df_fct <- data %>% group_by(data[,cluster_col]) %>% 
        summarise_at(vars(fct_cols), ~max(.,na.rm = TRUE)) %>%
        mutate_at(vars(fct_cols), function(x) ifelse(is.infinite(x), 0, x)) %>%
        as.data.frame()
      colnames(df_fct) <- c(cluster_col, fct_cols)
    }
    data <- df_y
    if (!is.null(df_fct)){
      data <- merge(data, df_fct)
    }
    if (!is.null(df_num)){
      data <- merge(data, df_num)
    }
  }
  data <- assign.dict(data, dict_data)
  

  
  # --- preprocessing test data if given and engineer_test_data is TRUE---
  
  if (!is.null(test_data)){
    # save non-engineered test dataset in a original data
    test_data_org <- test_data
    # keep internal data
    data_internal <- data
    # reassign data(tmp) by test data
    data <- test_data
    # trim time info
    if (length(trim_by_col)>0 ){
      # if there is a valid event / control group split, and user choose not to trim the control group
      if (all(unique(as.character(data[,y_col])) %in% c(1,0,NA)) & !trim_ctrl){
        data_pos <- data %>% filter(data[,y_col]==1 & data[,trim_by_col]>=trim_vec[1]*time_unit & data[,trim_by_col]<trim_vec[2]*time_unit ) %>% as.data.frame()
        data_ctrl <- data %>% filter(data[,y_col]==0) %>% as.data.frame()
        data <- bind_rows(data_pos, data_ctrl)
      }else{
        data <- data %>% filter(data[,trim_by_col]>=trim_vec[1]*time_unit & data[,trim_by_col]<trim_vec[2]*time_unit ) %>% as.data.frame()
      }
    }
    data$cluster_id <- data[,cluster_col]
    # impute numeric inputs
    if(!impute_per_cluster){
      if(imputation=="Mean"){
        data <- data %>% mutate_at(vars(num_cols), ~tidyr::replace_na(., mean(.,na.rm = TRUE))) %>% as.data.frame()
      }
      if(imputation=="Median"){
        data <- data %>% mutate_at(vars(num_cols), ~tidyr::replace_na(., median(.,na.rm = TRUE))) %>% as.data.frame()
      }
      if(imputation=="Zero"){
        data <- data %>% mutate_at(vars(num_cols), ~tidyr::replace_na(., 0)) %>% as.data.frame()
      }
    }else{
      if(imputation=="Mean"){
        for(sbj in unique(data[,cluster_col]) ){
          for(col in num_cols){
            data[which(data[,cluster_col]==sbj & is.na(data[,col])), col] <- mean(data[which(data[,cluster_col]==sbj), col], na.rm=TRUE)
          }
        }
      }
      if(imputation=="Median"){
        for(sbj in unique(data[,cluster_col]) ){
          for(col in num_cols){
            data[which(data[,cluster_col]==sbj & is.na(data[,col])), col] <- median(data[which(data[,cluster_col]==sbj), col], na.rm=TRUE)
          }
        }
      }
      if(imputation=="Zero"){
        for(sbj in unique(data[,cluster_col]) ){
          for(col in num_cols){
            data[which(data[,cluster_col]==sbj & is.na(data[,col])), col] <- 0
          }
        }
      }
    }
    # winsorize numeric columns
    if(winsorizing){data[,num_cols] <- winsorize(data[,num_cols])}
    # aggregation if required
    if(aggregation){
      df_y <- NULL
      df_fct <- NULL
      df_num <- NULL
      # y tag - max 
      if(dict_data[y_col, "type"]=="num"){
        df_y <- data %>% group_by(data[,cluster_col]) %>% summarise_at(vars(y_col), ~mean(.,na.rm = TRUE)) %>% as.data.frame()
        colnames(df_y) <- c(cluster_col, y_col)
      }else if(dict_data[y_col, "type"]=="fct"){
        df_y <- data %>% group_by(data[,cluster_col]) %>% summarise_at(vars(y_col), ~max(.,0,na.rm = TRUE)) %>% as.data.frame()
        colnames(df_y) <- c(cluster_col, y_col) 
      }
      # x num - mean
      if(length(num_cols)>0){
        df_num <- data %>% group_by(data[,cluster_col]) %>% summarise_at(vars(num_cols), ~mean(.,na.rm = TRUE)) %>% as.data.frame()
        colnames(df_num) <- c(cluster_col, num_cols)
      }
      #x fct - max
      if(length(fct_cols)>0){
        df_fct <- data %>% group_by(data[,cluster_col]) %>% 
          summarise_at(vars(fct_cols), ~max(.,na.rm = TRUE)) %>%
          mutate_at(vars(fct_cols), function(x) ifelse(is.infinite(x), 0, x)) %>%
          as.data.frame()
        colnames(df_fct) <- c(cluster_col, fct_cols)
      }
      data <- df_y
      if (!is.null(df_fct)){
        data <- merge(data, df_fct)
      }
      if (!is.null(df_num)){
        data <- merge(data, df_num)
      }
    }
    
    data <- assign.dict(data, dict_data)
    # reassign temporary data to test_data
    test_data <- data
    # keep data as internal training data
    data <- data_internal
    
  }
  
  #  --- run corresponding models --- 
  if(dict_data[y_col, "type"]=="fct"){
    results <- do_lrm_pip(data=data, 
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
                          test_data=test_data, 
                          test_data_org=test_data_org,
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
  
  # --- assign report object for the front end interface to the lowest level of reference ---
  
  #  model development reports 
  devel_model_info_tbl <- results$model_obj$cv_obj$model_info
  devel_score_summ_tbl <- results$model_obj$cv_obj$model_scores
  devel_score_summ_tbl <- devel_score_summ_tbl[,union(c("success_nfold","train_AUROC_mean","valid_AUROC_mean","train_AUROC_se","valid_AUROC_se"), colnames(devel_score_summ_tbl))]
  devel_cali_plot <- results$model_obj$cv_obj$calibration_curve
  devel_cv_eval_trace_tbl <- results$model_obj$cv_obj$cv_eval_trace
  
  # model inference reports
  infer_effect_plot_1d <- results$model_obj$effects_plot_1d
  infer_effect_plot_2d <- results$model_obj$effects_plot_2d
  infer_final_model_obj <- results$model_obj$mdl_obj # for print, anova and save locally
  
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
               infer_effect_plot_1d = infer_effect_plot_1d,
               infer_effect_plot_2d = infer_effect_plot_2d,
               infer_final_model_obj = infer_final_model_obj,
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
