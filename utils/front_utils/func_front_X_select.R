front_X_select <- function(
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
  aggregation=TRUE, # should set to be true
  # --- local ---
  trim_ctrl = TRUE,
  standardize=TRUE # always set to be true
){
  
  x_select_mdls <- NULL
  x_select_mdls_grouped <- NULL
  if (!aggregation){
    warning("No repeated measures allowed, please check 'aggregation' in 'Setup' page")
  } 
  
  # This function is for feature selection using lasso regression but compared with ridge regression
  
  
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
  
  # --- preprocessing ---
  # ---- prepare engineered training and validation dataset (internal) ----
  print("---- prepare engineered train and validation dataset (internal) ----")
  tryCatch({
    if(trim_ctrl){
      data_in <- engineer(data = data,
                          num_cols = num_cols,
                          fct_cols = fct_cols,
                          cluster_col = cluster_col,
                          trim_by_col = trim_by_col,
                          trim_min = trim_vec[1]*time_unit,
                          trim_max = trim_vec[2]*time_unit,
                          pctcut_num_cols = pctcut_num_cols,
                          pctcut_num_vec = pctcut_num_vec,
                          pctcut_num_coerce = pctcut_num_coerce,
                          filter_tag_cols = filter_tag_cols,
                          imputation = imputation,
                          impute_per_cluster = impute_per_cluster,
                          winsorizing = winsorizing,
                          aggregation = aggregation)
    }else{
      if (all(unique(as.character(data[,y_col])) %in% c(1,0,NA))){
        data_event <- engineer(data = data[which(data[,y_col]==1),],
                               num_cols = num_cols,
                               fct_cols = fct_cols,
                               cluster_col = cluster_col,
                               trim_by_col = trim_by_col,
                               trim_min=trim_vec[1]*time_unit,
                               trim_max=trim_vec[2]*time_unit,
                               pctcut_num_cols = pctcut_num_cols,
                               pctcut_num_vec = pctcut_num_vec,
                               pctcut_num_coerce = pctcut_num_coerce,
                               filter_tag_cols = filter_tag_cols,
                               imputation = imputation,
                               impute_per_cluster = impute_per_cluster,
                               winsorizing = winsorizing,
                               aggregation = aggregation)
        data_cntrl <- engineer(data = data[which(data[,y_col]==0),],
                               num_cols = num_cols,
                               fct_cols = fct_cols,
                               cluster_col = cluster_col,
                               trim_by_col = trim_by_col,
                               trim_min=-Inf,
                               trim_max=Inf,
                               trim_keepna = TRUE,
                               pctcut_num_cols = pctcut_num_cols,
                               pctcut_num_vec = pctcut_num_vec,
                               pctcut_num_coerce = pctcut_num_coerce,
                               filter_tag_cols = filter_tag_cols,
                               imputation = imputation,
                               impute_per_cluster = impute_per_cluster,
                               winsorizing = winsorizing,
                               aggregation = aggregation)
        data_in <- bind_rows(data_cntrl, data_event)
      }
    }
    data_in <- assign.dict(data_in, dict_data)
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
  
  return( list(x_select_mdls = x_select_mdls,
               x_select_mdls_grouped = x_select_mdls_grouped
               ))
}