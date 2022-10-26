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
  method=c("logit_rcs", "loess", "mean", "bootstrap")[1], 
  pct=TRUE,
  y_map_func=c("fold_risk", "probability", "log_odds")[1],
  y_map_max=2,
  label_y=TRUE,
  label_y_order = c(), # order of the levels of y
  group_label=NULL,
  layout_ncol = 5,
  x_raw_scale = FALSE,
  heat_limits = NULL,
  legend.position="right",
  sample_per_cluster = NULL
){
  
  library(RColorBrewer)
  palette_diy <- colorRampPalette(brewer.pal(10, "Spectral"))
  
  # init objects to return
  plot_obj <- NULL
  plot_list <- NULL
  
  # find the column name in data for the group by label
  group_col <- dict_data$varname[which(dict_data$label==group_label)]
  if(length(group_col)==0){
    print("-- non-grouping uni-heat --")
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
                                     layout_ncol = layout_ncol,
                                     heat_limits = heat_limits,
                                     y_map_func=y_map_func,
                                     y_map_max=y_map_max,
                                     label_y=label_y,
                                     sample_per_cluster=sample_per_cluster)
    
    return(list(plot_obj = heatmap_obj$plot_obj,
                df_result_all_sort = heatmap_obj$df_result_all_sort))
  }else{
    print("-- grouped uni-heat --")
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
                                         layout_ncol = layout_ncol,
                                         heat_limits = heat_limits,
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
    if(pct){
      if (!x_raw_scale){
        if( length(heat_limits)<2 ){
          heat_limits <- c(min(plot_df_all$yhat,na.rm = TRUE), min(y_map_max,max(plot_df_all$yhat,na.rm = TRUE))) 
        }
        # fix upper and lower boundary if forced
        plot_df_all$yhat[which(plot_df_all$yhat<heat_limits[1])] <- heat_limits[1]
        plot_df_all$yhat[which(plot_df_all$yhat>heat_limits[2])] <- heat_limits[2]
        print(paste0("using limits of [",paste0(heat_limits,collapse=", "),"] heatmap legend"))
        
        plot_obj <- ggplot(plot_df_all, aes(x=pctl,y=level))+
          geom_tile(aes(fill=yhat)) +
          labs(fill=y_map_func,x="Percentile",y=NULL) + 
          scale_fill_gradientn(limits = heat_limits, 
                               colours = rev(palette_diy(8)),
                               na.value =NA) +
          facet_wrap(~var_name, ncol=layout_ncol, scales = "free") +
          theme_minimal()
        if("c_score" %in% colnames(plot_df_all)){
          plot_obj <- plot_obj + geom_text(aes(label=c_label), hjust="left", na.rm = TRUE, check_overlap = TRUE)
        }
      }else{
        if( length(heat_limits)<2 ){
          heat_limits <- c(min(plot_df_all$yhat,na.rm = TRUE), min(y_map_max,max(plot_df_all$yhat,na.rm = TRUE))) 
        }
        # fix upper and lower boundary if forced
        plot_df_all$yhat[which(plot_df_all$yhat<heat_limits[1])] <- heat_limits[1]
        plot_df_all$yhat[which(plot_df_all$yhat>heat_limits[2])] <- heat_limits[2]
        print(paste0("using limits of [",paste0(heat_limits,collapse=", "),"] heatmap legend"))
        
        x_label_map <- plot_df_all %>% group_by(var_name) %>% summarise(q0 = paste0("0th\n", round(mean(raw_value[which(round(pctl,2)==0)],na.rm=TRUE),1)),
                                                                        q25 = paste0("25th\n", round(mean(raw_value[which(round(pctl,2)==0.25)],na.rm=TRUE),1)),
                                                                        q50 = paste0("50th\n", round(mean(raw_value[which(round(pctl,2)==0.50)],na.rm=TRUE),1)),
                                                                        q75 = paste0("75th\n", round(mean(raw_value[which(round(pctl,2)==0.75)],na.rm=TRUE),1)),
                                                                        q100 = paste0("100th\n", round(mean(raw_value[which(round(pctl,2)==1.0)],na.rm=TRUE),1)) ) %>% as.data.frame()
        plot_list <- list()
        i=1
        for(var_name in sort(unique(plot_df_all$var_name))){
          plot_list[[i]] <- ggplot(plot_df_all[which(plot_df_all$var_name==var_name),], aes(x=pctl,y=level))+
            geom_tile(aes(fill=yhat)) +
            labs(subtitle=var_name,fill=y_map_func,x=NULL,y=NULL)+
            scale_fill_gradientn(limits = heat_limits, 
                                 colours = rev(palette_diy(8)),
                                 na.value = NA)  +
            scale_x_continuous(breaks = c(0,0.25,0.5,0.75,1),
                               labels = #c("0th","25th","50th","75th","100th")
                                 x_label_map[which(x_label_map$var_name==var_name),c("q0","q25","q50","q75","q100")] 
            )+
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
    }else{
      plot_list <- list()
      name_list <- c()
      i=1
      if( length(heat_limits)<2 ){
        heat_limits <- c(min(plot_df_all$yhat,na.rm = TRUE), min(y_map_max,max(plot_df_all$yhat,na.rm = TRUE))) 
      }
      # fix upper and lower boundary if forced
      plot_df_all$yhat[which(plot_df_all$yhat<heat_limits[1])] <- heat_limits[1]
      plot_df_all$yhat[which(plot_df_all$yhat>heat_limits[2])] <- heat_limits[2]
      print(paste0("using limits of [",paste0(heat_limits,collapse=", "),"] heatmap legend"))
      
      for(var_name in sort(unique(plot_df_all$var_name))){
        plot_df_all_var <- plot_df_all[which(plot_df_all$var_name==var_name),]
        plot_df_all_var$raw_value <- as.numeric(sub("\\(", "", stringr::str_split_fixed(cut(plot_df_all_var$raw_value, 15),",",2)[,1]))
        plot_df_all_var$raw_value_rank <- cut(plot_df_all_var$raw_value, 15, labels = FALSE)
        x_map_label <- as.data.frame(distinct(plot_df_all_var[,c("raw_value_rank","raw_value")]))
        x_map_label <- x_map_label[which(x_map_label$raw_value_rank%in%c(1,5,10,15)),]
        if(max(x_map_label$raw_value,na.rm=TRUE)<=1){
          x_map_label$raw_value <- round(x_map_label$raw_value,1)
        }else{
          x_map_label$raw_value <- round(x_map_label$raw_value)
        }
        # resolution <- (max(plot_df_all_var$raw_value,na.rm=TRUE)-min(plot_df_all_var$raw_value,na.rm=TRUE))/15
        # plot_df_all_var$raw_value <- floor(plot_df_all_var$raw_value/resolution)*resolution
        plot_df_all_final <- data.frame()
        for(l in unique(plot_df_all_var$level)){
          plot_df_all_var_l <- plot_df_all_var[which(plot_df_all_var$level==l),]
          plot_df_all_final_l <- plot_df_all_var_l[,c("raw_value_rank","yhat")]#data.frame(approx(plot_df_all_var_l$raw_value_rank, plot_df_all_var_l$yhat, n=max(plot_df_all_var_l$raw_value_rank,na.rm=TRUE)))
          colnames(plot_df_all_final_l) <- c("raw_value_rank","yhat")
          plot_df_all_final_l$level <- l
          if("c_label" %in% colnames(plot_df_all)){
            plot_df_all_final_l$c_label <- NA
            plot_df_all_final_l$c_label[which(plot_df_all_final_l$raw_value==min(plot_df_all_final_l$raw_value,na.rm=TRUE))] <- unique(plot_df_all_var_l$c_label[which(!is.na(plot_df_all_var_l$c_label))])
          }
          plot_df_all_final <- bind_rows(plot_df_all_final, plot_df_all_final_l)
        }
        #plot_df_all_final <- plot_df_all_var
        plot_df_all_final$level <- stringr::str_wrap(plot_df_all_final$level, width=10)
        if(length(label_y_order)>0){
          plot_df_all_final$level <- factor(plot_df_all_final$level, levels = rev(stringr::str_wrap(label_y_order, width=10)))
        }
        plot_df_all_final <- plot_df_all_final[which(!is.na(plot_df_all_final$level)),]
        plot_list[[i]] <- ggplot(plot_df_all_final, aes(x=raw_value_rank,y=level))+
          geom_tile(aes(fill=yhat)) +
          labs(subtitle=gsub("[^[:alnum:]]+"," ",var_name),fill=gsub("[^[:alnum:]]+"," ",y_map_func),x=NULL,y=NULL)+
          scale_fill_gradientn(limits = heat_limits, 
                               colours = rev(palette_diy(8)),
                               #colours = rev(rainbow(7)),
                               na.value = NA)  +
          scale_x_continuous(breaks=x_map_label$raw_value_rank,labels=x_map_label$raw_value) +
          theme_minimal() 
        if("c_label" %in% colnames(plot_df_all)){
          plot_list[[i]] <- plot_list[[i]] + geom_text(data=plot_df_all_final, aes(label=c_label), hjust="left", na.rm = TRUE, check_overlap = TRUE)
        }
        i <- i + 1
        name_list <- c(name_list, var_name)
      }
      names(plot_list) <- name_list
      layout_ncol <- min(length(plot_list), layout_ncol)
      plot_obj <- ggpubr::ggarrange(plotlist = plot_list, 
                                    ncol=layout_ncol, 
                                    nrow=ceiling(n_distinct(plot_df_all$var_name)/layout_ncol),
                                    common.legend = TRUE, 
                                    legend = "right")
    }
    
    
    return(list(plot_obj = plot_obj,
                plot_list = plot_list ))
  }
}




# ############################################ not run #############################################
# data = data_txp_final
# dict_data = dict_txp_final
# num_labels = dict_txp_final$label[which(dict_txp_final$type=="num")][1]
# y_label = "Positive culture VS Negative culture and No blood culture"
# cluster_label = "Subject ID"
# # --- setup ---
# trim_by_label = "Time to infection"
# trim_vec = c(-12,24)
# time_unit= 1
# pctcut_num_labels=c()
# pctcut_num_vec=c(0.1, 99.9)
# pctcut_num_coerce = TRUE
# filter_tag_labels=c()
# imputation="None"
# impute_per_cluster=FALSE
# winsorizing=TRUE
# aggregate_per=c("row", "cluster_trim_by_unit", "cluster")[1]
# # --- local ---
# trim_ctrl=FALSE
# num_adjust_label = NULL
# method=c("logit_rcs", "loess", "mean", "bootstrap")[1]
# pct = FALSE
# y_map_func=c("fold_risk", "probability", "log_odds")[1]
# y_map_max=10
# label_y=TRUE
# group_label="Transplant Type"
# layout_ncol = 1
# x_raw_scale = FALSE
# heat_limits = NULL
# legend.position="right"
# sample_per_cluster = NULL





