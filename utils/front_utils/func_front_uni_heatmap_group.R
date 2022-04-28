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
  y_map_max=2,
  label_y=TRUE,
  group_label=NULL,
  layout_ncol = 5,
  x_raw_scale = FALSE,
  heat_limits = NULL,
  legend.position="right",
  sample_per_cluster = NULL
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
                                     label_y=label_y,
                                     sample_per_cluster=sample_per_cluster)
    
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
                                         label_y=label_y,
                                         sample_per_cluster=sample_per_cluster)
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
    if (!x_raw_scale){
      plot_obj <- ggplot(plot_df_all, aes(x=pctl,y=level))+
        geom_tile(aes(fill=yhat)) +
        labs(fill=y_map_func,x="Percentile",y=NULL) + 
        scale_fill_gradientn(colours = rev(rainbow(7))) +
        facet_wrap(~var_name, ncol=layout_ncol, scales = "free") +
        theme_minimal()
      if("c_score" %in% colnames(plot_df_all)){
        plot_obj <- plot_obj + geom_text(aes(label=c_label), hjust="left", na.rm = TRUE, check_overlap = TRUE)
      }
    }else{
      if( length(heat_limits)<2 ){
        heat_limits <- c(min(plot_df_all$yhat,na.rm = TRUE), min(y_map_max,max(plot_df_all$yhat,na.rm = TRUE))) 
      }
      x_label_map <- plot_df_all %>% group_by(var_name) %>% summarise(q0 = paste0(round(mean(raw_value[which(round(pctl,2)==0)],na.rm=TRUE),2),"\n0th"),
                                                       q25 = paste0(round(mean(raw_value[which(round(pctl,2)==0.25)],na.rm=TRUE),2),"\n25th"),
                                                       q50 = paste0(round(mean(raw_value[which(round(pctl,2)==0.50)],na.rm=TRUE),2),"\n50th"),
                                                       q75 = paste0(round(mean(raw_value[which(round(pctl,2)==0.75)],na.rm=TRUE),2),"\n75th"),
                                                       q100 = paste0(round(mean(raw_value[which(round(pctl,2)==1.0)],na.rm=TRUE),2),"\n100th") ) %>% as.data.frame()
      plot_list <- list()
      i=1
      for(var_name in sort(unique(plot_df_all$var_name))){
        plot_list[[i]] <- ggplot(plot_df_all[which(plot_df_all$var_name==var_name),], aes(x=pctl,y=level))+
          geom_tile(aes(fill=yhat)) +
          labs(subtitle=var_name,fill=y_map_func,x=NULL,y=NULL)+
          scale_fill_gradientn(limits = heat_limits, colours = rev(rainbow(7)))  +
          scale_x_continuous(breaks = c(0,0.25,0.5,0.75,1),
                             labels = x_label_map[which(x_label_map$var_name==var_name),c("q0","q25","q50","q75","q100")] )+
          theme(legend.position = legend.position) +
          theme_minimal() 
        if("c_score" %in% colnames(plot_df_all)){
          plot_list[[i]] <- plot_list[[i]] + geom_text(data=plot_df_all[which(plot_df_all$var_name==var_name),], aes(label=c_label), hjust="left", na.rm = TRUE, check_overlap = TRUE)
        }
        i <- i + 1
      }
      plot_obj <- ggpubr::ggarrange(plotlist = plot_list, 
                                    ncol=layout_ncol, 
                                    nrow=ceiling(n_distinct(plot_df_all$var_name)/layout_ncol),
                                    common.legend = TRUE, 
                                    legend = legend.position)
    }
    
    
    return(list(plot_obj = plot_obj,
                df_result_all_sort = plot_df_all ))
  }
}
