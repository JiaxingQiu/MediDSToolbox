front_uni_heatmap <- function(data=subset_df(data_ml,"40w"),
                              dict_data=dict_ml,
                              trim_by_label="Post-menstrual Age",
                              trim_vec = c(22, 40),
                              num_labels, 
                              y_label, 
                              cluster_label, 
                              num_adjust_label=NULL, 
                              method="logit_rcs", 
                              pct=TRUE,
                              imputation="None",
                              winsorizing=FALSE,
                              aggregation=FALSE,
                              time_unit=7,
                              impute_per_cluster=FALSE,
                              trim_ctrl=TRUE,
                              y_map_func=c("fold_risk", "probability", "log_odds")[1],
                              y_map_max=3
){
  
  
  trim_by_col <- rownames(dict_data[which(dict_data$label_front==trim_by_label), ])
  num_cols <- intersect(rownames(dict_data[which(dict_data$label_front%in%num_labels), ]), rownames(dict_data[which(dict_data$mlrole=="input"&dict_data$type=="num"), ]))
  y_col <- rownames(dict_data[which(dict_data$label_front==y_label),])
  fct_cols <- c(y_col)
  cluster_col <- rownames(dict_data[which(dict_data$label_front==cluster_label),])
  
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
  data <- assign.dict(data, dict_data, overwrite = TRUE)
  
  num_adjust_col <- NULL
  if(num_adjust_label!="None") num_adjust_col <- rownames(dict_data[which(dict_data$label_front==num_adjust_label),])
  # ---- run back end functions ----
  if(method=='Kernel Density Estimates'){
    df_result_all <- uni_kde_nums(data, num_cols, pct=pct)
    plot_obj <- ggplot(df_result_all, aes(x=pctl,y=var_name))+
      geom_tile(aes(fill=density_scaled)) +
      labs(fill="KDE",x="Percentile") + 
      theme(axis.title.y=element_blank()) + 
      scale_fill_gradientn(colours = terrain.colors(10))
  }else{
    df_result_all <- uni_tag_nums(data, num_cols, y_col, cluster_col, num_adjust_col, method=method, pct=pct, y_map_func=y_map_func, y_map_max=y_map_max)
    var_order <- df_result_all %>% group_by(var_name) %>% summarise(max_prob=max(yhat)) %>% arrange(max_prob) %>% as.data.frame()
    df_result_all_sort <- dplyr::left_join(var_order,df_result_all)
    plot_obj <- ggplot(df_result_all_sort, aes(x=pctl,y=var_name))+
      geom_tile(aes(fill=yhat)) +
      labs(fill=y_map_func,x="Percentile") + 
      theme(axis.title.y=element_blank()) + 
      scale_fill_gradientn(colours = rev(rainbow(7))) +
      scale_y_discrete(limits=var_order$var_name)
    
  }
  return(plot_obj)
  
}


