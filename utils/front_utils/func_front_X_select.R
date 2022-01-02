front_X_select <- function(
  # global parameters
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
  y_label="Primary outcome (EN)___Unfavorable", 
  cluster_label="PreVent study ID",
  imputation="None",
  impute_per_cluster=FALSE,
  winsorizing=FALSE,
  # local parameters
  standardize=TRUE, # always set to be true
  aggregation=TRUE, # always set to be true
  trim_ctrl=TRUE 
){
  
  x_select_mdls <- NULL
  if (!aggregation){
    return("No repeated measures allowed, please check 'aggregation' in 'Setup' page")
  } 
  
  # This function is for feature selection using lasso regression but compared with ridge regression
  
  
  # ---- pre-process ----
  x_cols_linear <- rownames(dict_data[which(dict_data$label_front%in%x_labels_linear&dict_data$mlrole=="input"&dict_data$type=="num"), ]) # linear numeric columns
  x_cols_nonlin_rcs5 <- rownames(dict_data[which(dict_data$label_front%in%x_labels_nonlin_rcs5&dict_data$mlrole=="input"&dict_data$type=="num"), ])
  x_cols_nonlin_rcs4 <- rownames(dict_data[which(dict_data$label_front%in%x_labels_nonlin_rcs4&dict_data$mlrole=="input"&dict_data$type=="num"), ])
  x_cols_nonlin_rcs3 <- rownames(dict_data[which(dict_data$label_front%in%x_labels_nonlin_rcs3&dict_data$mlrole=="input"&dict_data$type=="num"), ])
  x_cols_fct <- rownames(dict_data[which(dict_data$label_front%in%x_labels_fct & dict_data$type=="fct" & dict_data$unit!="tag01" & dict_data$mlrole=="input"), ])
  x_cols_tag <- rownames(dict_data[which(dict_data$label_front%in%x_labels_tag & dict_data$type=="fct" & dict_data$unit=="tag01" & dict_data$mlrole=="input"), ])
  y_col_tag <- rownames(dict_data[which(dict_data$label_front==y_label & dict_data$type=="fct" & dict_data$unit=="tag01" ),])
  y_col_num <- rownames(dict_data[which(dict_data$label_front==y_label & dict_data$type=="num" ),])
  y_col <- union(y_col_tag, y_col_num)
  fct_cols <- unique(c(x_cols_fct, x_cols_tag, y_col_tag)) # group columns together by typr for data engineering
  num_cols <- unique(c(x_cols_linear, x_cols_nonlin_rcs3,x_cols_nonlin_rcs4,x_cols_nonlin_rcs5, y_col_num))
  cluster_col <- rownames(dict_data[which(dict_data$label_front==cluster_label),])
  rel_time_col <- rownames(dict_data[which(dict_data$label_front==trim_by_label),])
  trim_by_col <- rownames(dict_data[which(dict_data$label_front==trim_by_label), ])
  
  # --- preprocessing ---
  data_in <- NULL
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
  
  x_select_mdls <- lss_x_select(data=data_in,
                               dict_data = dict_data,
                               y_col = y_col,
                               x_cols_nonlin_rcs3 = x_cols_nonlin_rcs3,
                               x_cols_nonlin_rcs4 = x_cols_nonlin_rcs4,
                               x_cols_nonlin_rcs5 = x_cols_nonlin_rcs5,
                               x_cols_linear=x_cols_linear, 
                               x_cols_fct=x_cols_fct,
                               x_cols_tag=x_cols_tag,
                               standardize=standardize)
  
  return(x_select_mdls)
}