front_viz_2d_stats <- function(
  data,
  dict_data,
  y_label,
  x_label1,
  x_label2,
  cluster_label,
  # --- engineering ---
  trim_by_label, #trim by time column
  trim_vec = c(-Inf, Inf), 
  time_unit = 1,
  pctcut_num_labels = c(), # cutoff by percentile of one or more numeric variable
  pctcut_num_vec = c(0, 100),
  pctcut_num_coerce = TRUE,
  filter_tag_labels = c(), # tag columns
  imputation=c("None","Mean", "Median", "Zero")[1],
  impute_per_cluster=FALSE,
  winsorizing=TRUE,
  aggregation=FALSE
){
  # ---- Usage ----
  
   
  # ---- Arguments ----
  
  
  # ---- Value ----
  
  tryCatch({
    data <- assign.dict(data, dict_data, overwrite = TRUE)
    dict_data <- get.dict(data)
  },error=function(e){
    print("--- Skip refine dictionary from data ---")
    print(e)
  })
  
  # find columns names from input labels
  cluster_col <- rownames(dict_data[which(dict_data$label_front==cluster_label),])
  x_col1 <- rownames(dict_data[which(dict_data$label_front%in%x_label1), ])
  x_col2 <- rownames(dict_data[which(dict_data$label_front%in%x_label2), ])
  y_col <- rownames(dict_data[which(dict_data$label_front==y_label),])
  trim_by_col <- rownames(dict_data[which(dict_data$label_front==trim_by_label), ])
  pctcut_num_cols <- rownames(dict_data[which(dict_data$label_front%in%pctcut_num_labels), ])
  filter_tag_cols <- rownames(dict_data[which(dict_data$label_front%in%filter_tag_labels & dict_data$unit=="tag01"), ])
  num_cols <- intersect( dict_data$varname[which(dict_data$type=="num")], c(x_col1, x_col2, y_col) )
  fct_cols <- intersect( dict_data$varname[which(dict_data$type=="fct")], c(x_col1, x_col2, y_col))
  
  data <- engineer(data = data,
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
  
  
  
  # --- 2d heatmap plots ----
  plot_2d_stats <- NULL
  if(dict_data[x_col1,"type"]=="num" & dict_data[x_col2,"type"]=="num"){
    plot_2d_stats <- viz_2d_stats(data=data,
                                  dict_data = dict_data,
                                  x_col1 = x_col1,
                                  x_col2 = x_col2,
                                  y_col = y_col,
                                  cluster_col=cluster_col)
  }
  
  
  
  return(plot_2d_stats)
}
