front_lasso_select <- function(
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
  trim_by_label,
  trim_vec = c(-Inf, Inf), # trim relative time [from, to)
  time_unit = 1, # the increment scale of relative time
  pctcut_num_labels = c(),
  pctcut_num_vec = c(0.1, 99.9),
  pctcut_num_coerce=TRUE,
  filter_tag_labels=c(),
  imputation=c("None","Mean", "Median", "Zero")[1],
  impute_per_cluster=FALSE,
  winsorizing=FALSE,
  aggregate_per=c("cluster_trim_by_unit", "cluster")[2], # should set to be cluster wise or time unit wise
  # --- local ---
  trim_ctrl = TRUE,
  standardize=TRUE, # always set to be true
  test_data=NULL,
  y_map_func=c("fold_risk", "probability", "log_odds")[1],
  y_map_max=3,
  return_performance = TRUE
){
  
  x_select_mdls <- NULL
  x_select_mdls_grouped <- NULL
  
  # ---- pre-process ----
  x_cols_linear <- dict_data$varname[which(dict_data$label%in%x_labels_linear&dict_data$mlrole=="input"&dict_data$type=="num")]# linear numeric columns
  x_cols_nonlin_rcs5 <- dict_data$varname[which(dict_data$label%in%x_labels_nonlin_rcs5&dict_data$mlrole=="input"&dict_data$type=="num")]
  x_cols_nonlin_rcs4 <- dict_data$varname[which(dict_data$label%in%x_labels_nonlin_rcs4&dict_data$mlrole=="input"&dict_data$type=="num")]
  x_cols_nonlin_rcs3 <- dict_data$varname[which(dict_data$label%in%x_labels_nonlin_rcs3&dict_data$mlrole=="input"&dict_data$type=="num")]
  x_cols_fct <- dict_data$varname[which(dict_data$label%in%x_labels_fct & dict_data$type=="fct" & dict_data$unit!="tag01" & dict_data$mlrole=="input")]
  x_cols_tag <- dict_data$varname[which(dict_data$label%in%x_labels_tag & dict_data$type=="fct" & dict_data$unit=="tag01" & dict_data$mlrole=="input")]
  y_col_tag <- dict_data$varname[which(dict_data$label==y_label & dict_data$type=="fct" & dict_data$unit=="tag01" )]
  y_col_num <- dict_data$varname[which(dict_data$label==y_label & dict_data$type=="num" )]
  y_col <- union(y_col_tag, y_col_num)
  fct_cols <- unique(c(x_cols_fct, x_cols_tag, y_col_tag)) # group columns together by typr for data engineering
  num_cols <- unique(c(x_cols_linear, x_cols_nonlin_rcs3,x_cols_nonlin_rcs4,x_cols_nonlin_rcs5, y_col_num))
  cluster_col <- dict_data$varname[which(dict_data$label==cluster_label)]
  rel_time_col <- dict_data$varname[which(dict_data$label==trim_by_label)]
  trim_by_col <- dict_data$varname[which(dict_data$label==trim_by_label)]
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
                           trim_by_col = trim_by_col) # minimum data engineering = using default settings
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
                          impute_per_cluster = impute_per_cluster,
                          winsorizing = winsorizing,
                          aggregate_per = aggregate_per)
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
                               impute_per_cluster = impute_per_cluster,
                               winsorizing = winsorizing,
                               aggregate_per = aggregate_per)
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
                               impute_per_cluster = impute_per_cluster,
                               winsorizing = winsorizing,
                               aggregate_per = aggregate_per)
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
                             trim_by_col = trim_by_col)
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
                            impute_per_cluster = impute_per_cluster,
                            winsorizing = winsorizing,
                            aggregate_per = aggregate_per)
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
                                 impute_per_cluster = impute_per_cluster,
                                 winsorizing = winsorizing,
                                 aggregate_per = aggregate_per)
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
                                 impute_per_cluster = impute_per_cluster,
                                 winsorizing = winsorizing,
                                 aggregate_per = aggregate_per)
          data_ex <- bind_rows(data_cntrl, data_event)
        }
      }
      data_ex <- assign.dict(data_ex, dict_data)
    }
  },error=function(e){
    print("Error!")
    print(e)
  })
  
  
  if(length(y_col_tag)>0 ){
    family <- "binomial"
  } else if(length(y_col_num) > 0){
    family <- "gaussian"
  }
  print("---- lasso_x_select ----")
  tryCatch({
    x_select_mdls <- lasso_x_select(data = data_in,
                                    y_col = y_col,
                                    family = family,
                                    x_cols_nonlin_rcs3 = x_cols_nonlin_rcs3,
                                    x_cols_nonlin_rcs4 = x_cols_nonlin_rcs4,
                                    x_cols_nonlin_rcs5 = x_cols_nonlin_rcs5,
                                    x_cols_linear = x_cols_linear, 
                                    x_cols_fct = x_cols_fct,
                                    x_cols_tag = x_cols_tag,
                                    standardize = standardize,
                                    dict_data = dict_data)
  },error=function(e){
    print("Error!")
    print(e)
  })
  
  print("---- lasso_x_select_group ----")
  tryCatch({
    x_select_mdls_grouped <- lasso_x_select_group(data = data_in,
                                                  y_col = y_col,
                                                  family = family,
                                                  x_cols_nonlin_rcs3 = x_cols_nonlin_rcs3,
                                                  x_cols_nonlin_rcs4 = x_cols_nonlin_rcs4,
                                                  x_cols_nonlin_rcs5 = x_cols_nonlin_rcs5,
                                                  x_cols_linear = x_cols_linear, 
                                                  x_cols_fct = x_cols_fct,
                                                  x_cols_tag = x_cols_tag,
                                                  standardize = standardize,
                                                  dict_data = dict_data)
    
  },error=function(e){
    print("Error!")
    print(e)
  })
  
  print("----- lss_perform in -----")
  lss_perform_in <- NULL
  tryCatch({
    stopifnot(return_performance)
    lss_perform_in <- lss_perform(
      mdl_obj = x_select_mdls_grouped$lasso_optimal, # a grouped lasso regression model object
      df = data_in, # a dataset to test out performance on
      y_map_func = y_map_func, # response type
      y_map_max = y_map_max, # response upper cutoff
      rel_time_col=rel_time_col
    )
  },error=function(e){
    print("Error!")
    print(e)
  })
  
  print("----- lss_perform inorg -----")
  lss_perform_inorg <- NULL
  tryCatch({
    stopifnot(return_performance)
    lss_perform_inorg <- lss_perform(
      mdl_obj = x_select_mdls_grouped$lasso_optimal, # a grouped lasso regression model object
      df = data_inorg, # a dataset to test out performance on
      y_map_func = y_map_func, # response type
      y_map_max = y_map_max, # response upper cutoff
      rel_time_col=rel_time_col
    )
  },error=function(e){
    print("Error!")
    print(e)
  })
  
  print("----- lss_perform ex -----")
  lss_perform_ex <- NULL
  tryCatch({
    stopifnot(return_performance)
    lss_perform_ex <- lss_perform(
      mdl_obj = x_select_mdls_grouped$lasso_optimal, # a grouped lasso regression model object
      df = data_ex, # a dataset to test out performance on
      y_map_func = y_map_func, # response type
      y_map_max = y_map_max, # response upper cutoff
      rel_time_col=rel_time_col
    )
  },error=function(e){
    print("Error!")
    print(e)
  })
  
  print("----- lss_perform exorg -----")
  lss_perform_exorg <- NULL
  tryCatch({
    stopifnot(return_performance)
    lss_perform_exorg <- lss_perform(
      mdl_obj = x_select_mdls_grouped$lasso_optimal, # a grouped lasso regression model object
      df = data_exorg, # a dataset to test out performance on
      y_map_func = y_map_func, # response type
      y_map_max = y_map_max, # response upper cutoff
      rel_time_col=rel_time_col
    )
  },error=function(e){
    print("Error!")
    print(e)
  })
  # model performance reports on new dataset 
  perform_in_df_hat <- lss_perform_in$df_hat
  perform_inorg_df_hat <- lss_perform_inorg$df_hat
  perform_ex_df_hat <- lss_perform_ex$df_hat
  perform_exorg_df_hat <- lss_perform_exorg$df_hat
  
  perform_in_fitted_eff_plot <- lss_perform_in$fitted_eff_plot
  perform_inorg_fitted_eff_plot <- lss_perform_inorg$fitted_eff_plot
  perform_ex_fitted_eff_plot <- lss_perform_ex$fitted_eff_plot
  perform_exorg_fitted_eff_plot <- lss_perform_exorg$fitted_eff_plot
  
  perform_in_tte_plot <- lss_perform_in$tte_plot
  perform_inorg_tte_plot <- lss_perform_inorg$tte_plot
  perform_ex_tte_plot <- lss_perform_ex$tte_plot
  perform_exorg_tte_plot <- lss_perform_exorg$tte_plot
  
  perform_in_cali_plot <- lss_perform_in$cali_plot
  perform_inorg_cali_plot <- lss_perform_inorg$cali_plot
  perform_ex_cali_plot <- lss_perform_ex$cali_plot
  perform_exorg_cali_plot <- lss_perform_exorg$cali_plot
  
  perform_in_scores_plot <- lss_perform_in$scores_plot
  perform_inorg_scores_plot <- lss_perform_inorg$scores_plot
  perform_ex_scores_plot <- lss_perform_ex$scores_plot
  perform_exorg_scores_plot<- lss_perform_exorg$scores_plot
  
  perform_in_scores_tbl <- lss_perform_in$scores_all_final
  perform_inorg_scores_tbl <- lss_perform_inorg$scores_all_final
  perform_ex_scores_tbl <- lss_perform_ex$scores_all_final
  perform_exorg_scores_tbl <- lss_perform_exorg$scores_all_final
  
  
  # selected coef df
  vars_selected_df <- NULL
  x_select_obj <- x_select_mdls_grouped # grouped lasso regression
  if (!is.null(x_select_obj)){
    lasso_optimal <- x_select_obj$lasso_optimal
    coef_df <- data.frame(coef=as.numeric(lasso_optimal$beta))
    coef_df$vars <- as.character(rownames(lasso_optimal$beta))
    coef_df$coef_abs <- abs(coef_df$coef)
    coef_df_all <- coef_df
    vars_selected_df <- data.frame(vars_selected = coef_df_all$vars[which(coef_df_all$coef!=0)])
  }
  group_df <- x_select_obj$lasso_optimal$group_info
  colnames(group_df)[which(colnames(group_df) =="x_colname")] <- "vars_selected"
  vars_selected_df <- merge(vars_selected_df, group_df, all.x=TRUE)
  vars_selected_df$var_type <- gsub("^.*_", "", vars_selected_df$x_group)
  vars_selected_df$raw_vars_selected <- NA
  for (i in 1:nrow(vars_selected_df)){
    vars_selected_df$raw_vars_selected[i] <- gsub(paste0("_",vars_selected_df$var_type[i]),"",vars_selected_df$x_group[i])
  }
  group_lasso_vars_selected <- distinct( vars_selected_df[,c("raw_vars_selected", "var_type")] )
  
  return( list(group_lasso_vars_selected=group_lasso_vars_selected,
               x_select_mdls = x_select_mdls,
               x_select_mdls_grouped = x_select_mdls_grouped,
               score10fold = x_select_mdls_grouped$scores_final_10fold,
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
               perform_exorg_scores_tbl = perform_exorg_scores_tbl
               ))
}





# ##################### not run #############################
# # variable to use
# x_vars_linear = c("baby_weight",
#                   "baby_length",
#                   "baby_head",
#                   "ga_days",
#                   "demog_mage",
#                   "apgar1min",
#                   "apgar5min",
#                   "baby_temp"
#                   #"ca_fullpo",
#                   #"ca_enteralfeeds_date"
# )
# x_vars_tag <- c("baby_weight_under_fenton_10pct_factor_Yes",
#                 "baby_gender_factor_Male",
#                 "baby_multiple_factor_Yes",
#                 "baby_birthloc_factor_Born_outside_the_study_center",
#                 "chorio_clnc_factor_Yes",
#                 "chorio_hist_factor_Yes",
#                 "ld_membranes_factor_Yes",
#                 "membrane18hr_factor_Yes",
#                 "membrane7d_factor_Yes",
#                 "steroids_admin_factor_No",
#                 "any_antenatal_comp_factor_Yes",
#                 "antibio_admin_factor_Yes",
#                 "antibio_reason___2_factor_Yes",
#                 "antibio_reason___1_factor_Yes",
#                 "demog_mrace_baby_check_factor_Black_African_American",
#                 "demog_methnicity_baby_check_factor_Hispanic_or_Latino",
#                 "htn_hx_factor_Yes",
#                 "htn_priorpg_factor_Yes",
#                 "asthmahx_factor_Yes",
#                 "ant_hx_factor_Yes",
#                 "asthmapgprolong_factor_Yes",
#                 "m_smokehx_factor_Yes",
#                 "m_drugs_factor_Yes",
#                 "resuscit_options___1_factor_Yes",
#                 "resuscit_options___2_factor_Yes",
#                 "resuscit_options___3_factor_Yes",
#                 "resuscit_options___4_factor_Yes",
#                 "resuscit_options___5_factor_Yes",
#                 "resuscit_options___6_factor_Yes",
#                 "resuscit_options___7_factor_Yes",
#                 "indomethacin_admin_factor_Yes"
# )
# x_vars_fct <- c("baby_insurance_factor")
# 
# 
# data = data_ml
# dict_data = dict_ml
# y_label = dict_ml[which(dict_ml$varname=="primary_outcome_factor_Unfavorable"),"label"]
# cluster_label = dict_ml[which(dict_ml$varname=="subjectnbr"),"label"]
# x_labels_linear = dict_ml$label[which(dict_ml$varname%in%x_vars_linear)]
# x_labels_nonlin_rcs5 = c("Maternal age")
# x_labels_nonlin_rcs4 = c("Gestational Age")
# x_labels_nonlin_rcs3 = c("Birth weight")
# x_labels_fct = dict_ml$label[which(dict_ml$varname%in%x_vars_fct)]
# x_labels_tag = dict_ml$label[which(dict_ml$varname%in%x_vars_tag)]
# x_labels = unique(c(x_labels_linear,x_labels_nonlin_rcs5,x_labels_nonlin_rcs4,x_labels_nonlin_rcs3,x_labels_fct,x_labels_tag))
# # --- engineer ---
# trim_by_label = "Post-menstrual Age"
# trim_vec = c(-Inf, Inf) # trim relative time [from, to)
# time_unit = 7 # the increment scale of relative time
# pctcut_num_labels = c()
# pctcut_num_vec = c(0.1, 99.9)
# pctcut_num_coerce=TRUE
# filter_tag_labels=c()
# imputation=c("None","Mean", "Median", "Zero")[1]
# impute_per_cluster=FALSE
# winsorizing=FALSE
# aggregate_per=c("row", "cluster_trim_by_unit", "cluster")[3]
# # --- local ---
# trim_ctrl = TRUE
# standardize=TRUE # always set to be true
# test_data = data_ml[c(20000:40000),]
# y_map_func=c("fold_risk", "probability", "log_odds")[1]
# y_map_max=3
# return_performance = FALSE

