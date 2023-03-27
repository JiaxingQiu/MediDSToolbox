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
  aggregate_conditioned_on_labels = c(),
  # --- local ---
  new_dd = NULL,
  trim_ctrl=TRUE,
  num_adjust_label=NULL, 
  method=c("logit_rcs", "loess", "mean", "bootstrap")[1], 
  nknots=NULL,# number of knots to use in logit_rcs if provided
  y_map_func=c("fold_risk", "probability", "log_odds")[1],
  y_map_max=Inf,
  #label_y_order = c(), # order of the levels of y
  group_label=NULL,
  group_levels=c(),# order of levels of group
  layout_ncol = 3,
  heat_limits = NULL,
  sort_c=TRUE, # order the plot from top to bottom by c-stat, otherwise by maximum of y_hat
  round_c=2,# decimal places to keep for c-stat
  sample_per_cluster = NULL
){
  if(!method=="logit_rcs"){
    nknots <- NULL
  }
  
  library(RColorBrewer)
  palette_diy <- colorRampPalette(brewer.pal(10, "Spectral"))
  
  # init objects to return
  plot_obj <- NULL
  plot_list <- NULL
  plot_df <- NULL
  
  #### make the plot ####
  print("-- overall non-grouped uni-heat --")
  heatmap_obj_all <- front_uni_heatmap(data=data,
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
                                   aggregate_conditioned_on_labels = aggregate_conditioned_on_labels,
                                   # --- local ---
                                   new_dd = new_dd,
                                   trim_ctrl=trim_ctrl,
                                   num_adjust_label=num_adjust_label,
                                   method=method,
                                   nknots=nknots,
                                   layout_ncol = layout_ncol,
                                   heat_limits = heat_limits,
                                   y_map_func=y_map_func,
                                   y_map_max=y_map_max,
                                   round_c=round_c,
                                   sort_c=sort_c,
                                   sample_per_cluster=sample_per_cluster)
  # find objects to return
  plot_obj <- heatmap_obj_all$plot_obj
  plot_list <- heatmap_obj_all$plot_list
  plot_df <- heatmap_obj_all$df_result_all_sort
  
  # find the column name in data for the group by label
  group_col <- dict_data$varname[which(dict_data$label==group_label)]
  if(length(group_col)>0){
    #### make the plot ####
    print("-- grouped uni-heat --")
    if(length(group_levels)==0){
      group_levels <- unique(as.character( data[,group_col] ))
    }else{
      group_levels <- group_levels[which(group_levels%in%unique(as.character( data[,group_col] )))]
    }
    # create new data dist for all groups to make prediction on the same data scale of x
    df_all <- heatmap_obj_all$df_result_all_sort[,c("x_name", "x_raw")]
    
    if(is.null(new_dd)){
      for(x in unique(df_all$x_name)){
        new_dd_x <- data.frame(x = df_all$x_raw[which(df_all$x_name==x)])
        colnames(new_dd_x) <- x
        if(is.null(new_dd)){
          new_dd <- new_dd_x
        }else{
          new_dd <- bind_cols(new_dd, new_dd_x)
        }
      }
    }
    # until now new_dd is always provided
    # x_pctl will be integers
    # collect plot data for each level in a list
    plot_df_all <- data.frame()
    for(g in group_levels){
      plot_df <- NULL
      tryCatch({
        data_sub <- data[which(as.character(data[,group_col])==g),]
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
                                         aggregate_conditioned_on_labels = aggregate_conditioned_on_labels,
                                         # --- local ---
                                         new_dd = new_dd,
                                         trim_ctrl=trim_ctrl,
                                         num_adjust_label=num_adjust_label, 
                                         method=method, 
                                         nknots=nknots,
                                         layout_ncol = layout_ncol,
                                         heat_limits = heat_limits,
                                         y_map_func=y_map_func,
                                         y_map_max=y_map_max,
                                         round_c=round_c,
                                         sample_per_cluster=sample_per_cluster)
        plot_df <- heatmap_obj$df_result_all_sort
        if(!is.null(plot_df)){
          plot_df$group_level <- g
          plot_df_all <- bind_rows(plot_df_all, plot_df)
        }else{
          print(paste0("failed level ",group_levels[i], " in ", group_label) )
        }
      },error=function(e){
        print(e)
      })
    }
    
    #### make level stratified plots ####
    group_levels <- intersect(group_levels, unique(plot_df_all$group_level))
    group_level_df <- data.frame(group_lorder=rev(seq(1,length(group_levels),1)), group_level = group_levels)
    plot_df_all <- merge(plot_df_all, group_level_df, all.x = TRUE)
    
    # fix heat color legend 
    if( length(heat_limits)<2 ){
      heat_limits <- c(min(plot_df_all$y_hat,na.rm = TRUE), min(y_map_max, max(plot_df_all$y_hat,na.rm = TRUE))) 
    }
    theme_obj <- theme_minimal()+
      theme(plot.subtitle = element_text(hjust = 0.5, face="bold", colour="black", size = 15),
            axis.ticks.length=unit(-0.25, "cm"),
            axis.text.y = element_text(face="bold", colour="black", size=12),
            axis.title.y = element_blank(),
            axis.text.x = element_text(face="bold", colour="black", size=12),
            axis.title.x = element_text(face="bold", colour="black", size=14),
            legend.title = element_text(face="bold", colour="black", size=14),
            legend.text = element_text(face="bold", colour="black", size=12)) 
    # make sure yellow is assigned to fold risk of 1
    if( max(plot_df_all$y_hat, na.rm=TRUE) > 1){
      color_values <- as.numeric(c(seq(min(plot_df_all$y_hat, na.rm=TRUE), 1, length.out=4),
                                   seq(1, max(plot_df_all$y_hat, na.rm=TRUE), length.out=4)))
    }else{
      color_values <- as.numeric(seq(min(plot_df_all$y_hat, na.rm=TRUE), max(plot_df_all$y_hat, na.rm=TRUE), length.out=8))
    }
    
    scale_fill_gradientn_obj <- scale_fill_gradientn(limits = heat_limits, 
                                                     values=scales::rescale(color_values),
                                                     colours = rev(palette_diy(8)),
                                                     na.value = NA)
    plot_list <- list()
    for(x in unique(plot_df_all$x_name)){
      subdf <- plot_df_all[which(plot_df_all$x_name==x),]
      plot_list[[x]] <- ggplot(subdf, aes(x=x_pctl,y=group_lorder))+ # stringr::str_wrap(gsub("[^[:alnum:]]+"," ",level))
        geom_tile(aes(fill=y_hat)) +
        geom_text(aes(label=c_label), hjust=0, vjust=-0.2, na.rm = TRUE, check_overlap = TRUE, size = 5) + 
        labs(subtitle=gsub("[^[:alnum:]]+"," ",x),fill=gsub("[^[:alnum:]]+"," ",y_map_func),x=NULL,y=NULL)+
        theme_obj + 
        scale_fill_gradientn_obj+
        scale_y_continuous(expand = c(0,0),
                           breaks = group_level_df$group_lorder, 
                           labels = stringr::str_wrap(group_level_df$group_level,width=5) )
      
      # map x raw value back to each x axis in the plot list
      x_label_map <- NULL
      breaks <- as.numeric(c(min(subdf$x_pctl,na.rm=TRUE),
                             round(quantile(subdf$x_pctl,0.05,na.rm=TRUE)),
                             round(quantile(subdf$x_pctl,0.25,na.rm=TRUE)),
                             round(quantile(subdf$x_pctl,0.5,na.rm=TRUE)),
                             round(quantile(subdf$x_pctl,0.75,na.rm=TRUE)),
                             round(quantile(subdf$x_pctl,0.95,na.rm=TRUE)),
                             max(subdf$x_pctl,na.rm=TRUE) ))
      x_label_map <- subdf %>% 
        filter(x_pctl%in%breaks) %>% 
        group_by(x_pctl) %>% summarise(x_raw = round(mean(x_raw,na.rm=TRUE),1)) %>%
        as.data.frame()
      x_label_map <- merge(data.frame(x_pctl=breaks), x_label_map, all.x=TRUE)
      
      plot_list[[x]] <- plot_list[[x]] +
        scale_x_continuous(breaks = x_label_map$x_pctl[-c(2,6)], 
                           labels = x_label_map$x_raw[-c(2,6)],
                           sec.axis = sec_axis(~.,
                                               breaks = x_label_map$x_pctl[c(2,6)], 
                                               labels = x_label_map$x_raw[c(2,6)] ) )
      
      
      if("y_logodds_signif"%in%colnames(subdf)){
        plot_list[[x]] <-  plot_list[[x]] +
          geom_point(aes(y=ifelse(y_logodds_signif%in%c(1),NA,group_lorder) ))
      }
      
      
    }
    
    layout_ncol <- min(length(plot_list), layout_ncol)
    plot_obj <- ggpubr::ggarrange(plotlist = plot_list, 
                                  ncol=layout_ncol, 
                                  nrow=ceiling(length(plot_list)/layout_ncol),
                                  common.legend = TRUE, 
                                  legend = "right")
    
    # find objects to return
    plot_obj <- plot_obj
    plot_list <- plot_list
    plot_df <- plot_df_all
  }
  
  # ------- create signature of illness plot --------
  # sort dataframe by c or ymax
  if(!"group_level" %in% colnames(plot_df) ){
    if(sort_c&("c_score" %in% colnames(plot_df))){
      df_order <- plot_df %>% group_by(x_name) %>% summarise(max_c=max(c_score)) %>% arrange(-max_c) %>% as.data.frame()
      plot_df_sort <- dplyr::left_join(df_order,plot_df)
      plot_df_sort$x_label <- paste0(stringr::str_wrap( gsub("_"," ",plot_df_sort$x_name), width = 15 ), "\nC = ",round(plot_df_sort$max_c,4))
    }else{
      df_order <- plot_df %>% group_by(x_name) %>% summarise(max_y=max(y_hat)) %>% arrange(-max_y) %>% as.data.frame()
      plot_df_sort <- dplyr::left_join(df_order,plot_df)
      plot_df_sort$x_label <- paste0(stringr::str_wrap( gsub("_"," ",plot_df_sort$x_name), width = 15 ), "\ny_max = ",round(plot_df_sort$max_y,4))
    }
  }else{
    if(sort_c&("c_score" %in% colnames(plot_df))){
      # sort by score
      df_order1 <- plot_df %>% group_by(x_name) %>% summarise(max_c=max(c_score),group_level=unique(group_level)) %>% arrange(-max_c) %>% as.data.frame()
      # sort by group level
      df_order2 <- plot_df %>% group_by(x_name,group_level) %>% summarise(max_c=max(c_score)) %>% arrange(group_level) %>% as.data.frame()
      # final order
      df_order <- merge(df_order1[,c("x_name","group_level")], df_order2, all.x=TRUE) 
      plot_df_sort <- dplyr::left_join(df_order,plot_df)
      plot_df_sort$x_label <- paste0(stringr::str_wrap( gsub("_"," ",plot_df_sort$x_name), width = 15 ), "\nC = ",round(plot_df_sort$max_c,4))
      plot_df_sort$x_label <- paste0(plot_df_sort$x_label, "\nGroup: ", plot_df_sort$group_level)
    }else{
      # sort by score
      df_order1 <- plot_df %>% group_by(x_name) %>% summarise(max_y=max(y_hat),group_level=unique(group_level)) %>% arrange(-max_y) %>% as.data.frame()
      # sort by group level
      df_order2 <- plot_df %>% group_by(x_name,group_level) %>% summarise(max_y=max(y_hat)) %>% arrange(group_level) %>% as.data.frame()
      # final order
      df_order <- merge(df_order1[,c("x_name","group_level")], df_order2, all.x=TRUE) 
      plot_df_sort <- dplyr::left_join(df_order,plot_df)
      plot_df_sort$x_label <- paste0(stringr::str_wrap( gsub("_"," ",plot_df_sort$x_name), width = 15 ), "\ny_max = ",round(plot_df_sort$max_y,4))
      plot_df_sort$x_label <- paste0(plot_df_sort$x_label, "\nGroup: ", plot_df_sort$group_level)
    }
  }  
  
  plot_df_sort$x_label <- factor(plot_df_sort$x_label, levels=unique(plot_df_sort$x_label))
  
  plot_signat <- uni_signature_illness(plot_df=plot_df_sort)
  plot_signat$plot_obj <- plot_signat$plot_obj + labs(x=NULL,y=gsub("_"," ",y_map_func))
  
  return(list(plot_obj = plot_obj,
              plot_list = plot_list,
              plot_df = plot_df,
              plot_obj_signat = plot_signat$plot_obj,
              plot_df_signat = plot_signat$plot_df))
}



uni_signature_illness <- function(plot_df){
  

  if(!"x_label" %in%colnames(plot_df) ){
    plot_df$x_label <- plot_df$x_name
  }
  plot_df_cuts <- NULL
  plot_df_base <- NULL
  
  # prepare red curves
  plot_df$y_hat_red <- ifelse(plot_df$y_logodds_signif==1,1,NA)*plot_df$y_hat
  # get baseline hline for x
  plot_df_base <- plot_df %>% group_by(x_label) %>% summarise(y_hat_baseline = mean(y_hat_baseline,na.rm=TRUE)) %>% as.data.frame()
  
  
  plot_obj <- ggplot(data = plot_df, aes(x=x_raw, y=y_hat))+
    theme_bw() +
    geom_line(color="white")+
    geom_line(aes(y=y_hat_red), color="red") +
    geom_hline(data = plot_df_base, mapping = aes(yintercept=y_hat_baseline), linetype="solid") + 
    facet_wrap(~x_label, scales = "free", ncol=4) + #"free_x"
    geom_ribbon(aes(ymin=y_hat_lower, ymax=y_hat_upper), alpha=0.2) +
    coord_cartesian(ylim=c(min(plot_df$y_hat_red,na.rm=TRUE)-0.1,max(plot_df$y_hat_red,na.rm=TRUE)+0.1))
  
  
  # add upper and lower cuts for x value
  if(n_distinct(plot_df$x_pctl)==201 & !(2.5 %in% unique(plot_df$x_pctl) & 97.5 %in% unique(plot_df$x_pctl)) ){
    plot_df$x_pctl <- (plot_df$x_pctl-1)/2
  }
  if(2.5 %in% unique(plot_df$x_pctl) & 97.5 %in% unique(plot_df$x_pctl)){
    plot_df_cuts <- plot_df %>% group_by(x_label) %>%
      summarise(x_raw_qt_l =unique(x_raw[x_pctl==2.5]),
                x_raw_qt_l_txt = "2.5th",
                x_raw_qt_u=unique(x_raw[x_pctl==97.5]),
                x_raw_qt_u_txt = "97.5th") %>% 
      as.data.frame()
    plot_obj <- plot_obj + 
      geom_vline(data=plot_df_cuts, mapping = aes(xintercept=x_raw_qt_l), linetype="dashed")+
      geom_text(data=plot_df_cuts, mapping=aes(x=x_raw_qt_l, y=max(plot_df$y_hat_red,na.rm=TRUE)-0.05, label=x_raw_qt_l_txt),hjust=0)+
      geom_vline(data=plot_df_cuts, mapping = aes(xintercept=x_raw_qt_u),linetype="dashed")+
      geom_text(data=plot_df_cuts, mapping=aes(x=x_raw_qt_u, y=max(plot_df$y_hat_red,na.rm=TRUE)-0.05, label=x_raw_qt_u_txt),hjust=1)
      
  }else{
    print("2.5th and 97.5th not found")
  }
  
  
  # merge additional lines back to data frame
  if(!is.null(plot_df_base)){
    plot_df <- merge(plot_df[,c("x_label",setdiff(colnames(plot_df), colnames(plot_df_base)))], plot_df_base, all.x = TRUE, all.y = FALSE)
    
  }
  if(!is.null(plot_df_cuts)){
    plot_df <- merge(plot_df[,c("x_label",setdiff(colnames(plot_df), colnames(plot_df_cuts)))], plot_df_cuts, all.x = TRUE, all.y = FALSE)
  }
  
  return(list(plot_obj = plot_obj,
              plot_df = plot_df))
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





