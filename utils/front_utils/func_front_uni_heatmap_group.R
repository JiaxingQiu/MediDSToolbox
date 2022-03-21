front_uni_heatmap_group <- function(
  data,
  dict_data,
  num_labels, 
  y_label, 
  cluster_label,
  # --- engineer ---
  trim_by_label=NULL,
  trim_vec = c(-Inf, Inf),
  time_unit=1,
  pctcut_num_labels=c(),
  pctcut_num_vec=c(0.1, 99.9),
  pctcut_num_coerce = TRUE,
  filter_tag_labels=c(),
  imputation="None",
  impute_per_cluster=FALSE,
  winsorizing=FALSE,
  aggregate_per=c("row", "cluster_trim_by_unit", "cluster")[1],
  # --- local ---
  trim_ctrl=TRUE,
  num_adjust_label=NULL, 
  method="logit_rcs", 
  pct=TRUE,
  y_map_func=c("fold_risk", "probability", "log_odds")[1],
  y_map_max=3,
  label_y=TRUE,
  group_label=NULL
){
  # find the column name in data for the group by label
  group_col <- dict_data$varname[which(dict_data$label==group_label)]
  if(length(group_col)==0){
    heatmap_obj <- front_uni_heatmap(data=data,
                                     dict_data=dict_data,
                                     num_labels=num_labels, 
                                     y_label=y_label, 
                                     cluster_label=cluster_label,
                                     # --- engineer ---
                                     trim_by_label=trim_by_label,
                                     trim_vec = trim_vec,
                                     time_unit=time_unit,
                                     pctcut_num_labels=pctcut_num_labels,
                                     pctcut_num_vec=pctcut_num_vec,
                                     pctcut_num_coerce=pctcut_num_coerce,
                                     filter_tag_labels=filter_tag_labels,
                                     imputation=imputation,
                                     impute_per_cluster=impute_per_cluster,
                                     winsorizing=winsorizing,
                                     aggregate_per=aggregate_per,
                                     # --- local ---
                                     trim_ctrl=trim_ctrl,
                                     num_adjust_label=num_adjust_label, 
                                     method=method, 
                                     pct=pct,
                                     y_map_func=y_map_func,
                                     y_map_max=y_map_max,
                                     label_y=label_y)
    
    return(list(plot_obj = heatmap_obj$plot_obj,
                df_result_all_sort = heatmap_obj$df_result_all_sort))
  }else{
    group_levels <- unique(as.character( data[,group_col] ))
    # collect plot data for each level in a list
    plot_df_list <- list()
    for(i in 1:length(group_levels)){
      plot_df <- NULL
      tryCatch({
        data_sub <- data[which(as.character(data[,group_col])==group_levels[i]),]
        heatmap_obj <- front_uni_heatmap(data=data_sub,
                                         dict_data=dict_data,
                                         num_labels=num_labels, 
                                         y_label=y_label, 
                                         cluster_label=cluster_label,
                                         # --- engineer ---
                                         trim_by_label=trim_by_label,
                                         trim_vec = trim_vec,
                                         time_unit=time_unit,
                                         pctcut_num_labels=pctcut_num_labels,
                                         pctcut_num_vec=pctcut_num_vec,
                                         pctcut_num_coerce=pctcut_num_coerce,
                                         filter_tag_labels=filter_tag_labels,
                                         imputation=imputation,
                                         impute_per_cluster=impute_per_cluster,
                                         winsorizing=winsorizing,
                                         aggregate_per=aggregate_per,
                                         # --- local ---
                                         trim_ctrl=trim_ctrl,
                                         num_adjust_label=num_adjust_label, 
                                         method=method, 
                                         pct=pct,
                                         y_map_func=y_map_func,
                                         y_map_max=y_map_max,
                                         label_y=label_y)
        plot_df <- heatmap_obj$df_result_all_sort
      },error=function(e){
        print(e)
      })
      if(is.null(plot_df)){
        plot_df_list[[i]] <- paste0("skip level ",group_levels[i], " in ", group_label) #data.frame(skip=group_levels[i])
      }else{
        plot_df_list[[i]] <- plot_df
      }
    }
    names(plot_df_list) <- group_levels
    plot_df_all <- data.frame()
    for(l in group_levels){
      if(!is.character( plot_df_list[[l]] )){
        plot_df <- plot_df_list[[l]]
        plot_df$level <- l
        plot_df_all <- bind_rows(plot_df_all, plot_df)
      }
    }
    plot_obj <- ggplot(plot_df_all, aes(x=pctl,y=level))+
      geom_tile(aes(fill=yhat)) +
      labs(fill=y_map_func,x="Percentile",y=NULL) + 
      scale_fill_gradientn(colours = rev(rainbow(7))) +
      facet_wrap(~var_name, nrow=5, scales = "free") +
      theme_minimal()
    if("c_score" %in% colnames(plot_df_all)){
      plot_obj <- plot_obj + geom_text(aes(label=c_label), hjust="left", na.rm = TRUE, check_overlap = TRUE)
    }
    return(list(plot_obj = plot_obj,
                df_result_all_sort = plot_df_all ))
  }
}
