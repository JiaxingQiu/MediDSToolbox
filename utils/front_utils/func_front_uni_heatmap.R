front_uni_heatmap <- function(
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
  new_dd = NULL, # new datadist to be predicted on if given
  trim_ctrl=TRUE,
  num_adjust_label=NULL, 
  method=c("logit_rcs", "loess", "mean", "bootstrap")[1], 
  y_map_func=c("fold_risk", "probability", "log_odds")[1],
  y_map_max=Inf,
  layout_ncol = 5,
  heat_limits = NULL,
  sort_c=TRUE, # order the plot from top to bottom by c-stat, otherwise by maximum of y_hat
  round_c=2,# decimal places to keep for c-stat
  sample_per_cluster = NULL
){
  library(RColorBrewer)
  palette_diy <- colorRampPalette(brewer.pal(10, "Spectral"))
  
  tryCatch({
    data <- assign.dict(data, dict_data, overwrite = TRUE)
    dict_data <- get.dict(data)
  },error=function(e){
    print("--- Skip refine dictionary from data ---")
    print(e)
  })
  
  # ---- translate front end labels to column names ----
  trim_by_col <- dict_data$varname[which(dict_data$label==trim_by_label)]
  num_cols <- intersect(dict_data$varname[which(dict_data$label%in%num_labels)], dict_data$varname[which(dict_data$type=="num")])#dict_data$mlrole=="input"&
  y_col <- dict_data$varname[which(dict_data$label==y_label)]
  fct_cols <- c(y_col)
  cluster_col <- dict_data$varname[which(dict_data$label==cluster_label)]
  pctcut_num_cols <- dict_data$varname[which(dict_data$label%in%pctcut_num_labels)]
  filter_tag_cols <- dict_data$varname[which(dict_data$label%in%filter_tag_labels)]
  num_adjust_col <- dict_data$varname[which(dict_data$label==num_adjust_label)]
  num_cols <- union(num_cols, num_adjust_col)
  
  plot_obj <- NULL
  plot_list <- NULL
  df_result_all_sort <- NULL
  
  # ---- engineering ----
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
                        aggregate_per = aggregate_per,
                        sample_per_cluster=sample_per_cluster)
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
                             aggregate_per = aggregate_per,
                             sample_per_cluster=sample_per_cluster)
      data_cntrl <- engineer(data = data[which(data[,y_col]==0),],
                             num_cols = num_cols,
                             fct_cols = fct_cols,
                             cluster_col = cluster_col,
                             trim_by_col = trim_by_col,
                             trim_min=-Inf,
                             trim_max=Inf,
                             trim_keepna = TRUE,
                             trim_step_size = time_unit,
                             pctcut_num_cols = pctcut_num_cols,
                             pctcut_num_vec = pctcut_num_vec,
                             pctcut_num_coerce = pctcut_num_coerce,
                             filter_tag_cols = filter_tag_cols,
                             imputation = imputation,
                             impute_per_cluster = impute_per_cluster,
                             winsorizing = winsorizing,
                             aggregate_per = aggregate_per,
                             sample_per_cluster=sample_per_cluster)
      data_in <- bind_rows(data_cntrl, data_event)
    }
  }
  data <- assign.dict(data_in, dict_data, overwrite = TRUE)
  
  # ---- run back end functions ----
  if(method=='Kernel Density Estimates'){
    df_result_all <- uni_kde_nums(data, num_cols)
    plot_obj <- ggplot(df_result_all, aes(x=x_pctl,y=x_name))+
      geom_tile(aes(fill=density_scaled)) +
      labs(fill="KDE",x="Percentile") + 
      theme(axis.title.y=element_blank()) + 
      scale_fill_gradientn(colours = terrain.colors(10))
  }else{
    #### run the model ####
    df_result_all <- uni_tag_nums(data, 
                                  num_cols, 
                                  tag_col=y_col, 
                                  cluster_col, 
                                  num_adjust_col, 
                                  method=method, 
                                  y_map_func=y_map_func, 
                                  y_map_max=y_map_max,
                                  new_dd=new_dd)
    stopifnot(!is.null(df_result_all))
    if ( "c_score" %in% colnames(df_result_all)){
      df_result_all$c_label <- NA
      for (x in unique(df_result_all$x_name) ){
        c <- round(unique(df_result_all$c_score[which(df_result_all$x_name==x)]),round_c)
        df_result_all$c_label[which(df_result_all$x_name==x)][1] <- paste0("C = ", c)
      }
    }else{
      df_result_all$c_label <- NA
    }
    if(sort_c&("c_score" %in% colnames(df_result_all))){
      df_order <- df_result_all %>% group_by(x_name) %>% summarise(max_c=max(c_score)) %>% arrange(max_c) %>% as.data.frame()
    }else{
      df_order <- df_result_all %>% group_by(x_name) %>% summarise(max_y=max(y_hat)) %>% arrange(max_y) %>% as.data.frame()
    }
    df_result_all_sort <- dplyr::left_join(df_order,df_result_all)
    
    
    #### make the plot ####
    # fix heat color legend 
    if( length(heat_limits)<2 ){
      heat_limits <- c(min(df_result_all_sort$y_hat,na.rm = TRUE), min(y_map_max, max(df_result_all_sort$y_hat,na.rm = TRUE))) 
    }
    df_result_all_sort$y_hat[which(df_result_all_sort$y_hat<heat_limits[1])] <- heat_limits[1]
    df_result_all_sort$y_hat[which(df_result_all_sort$y_hat>heat_limits[2])] <- heat_limits[2]
    print(paste0("using limits of [",paste0(heat_limits,collapse=", "),"] heatmap legend"))
    # make sure yellow is assigned to fold risk of 1
    if( max(df_result_all_sort$y_hat, na.rm=TRUE) > 1){
      color_values <- as.numeric(c(seq(min(df_result_all_sort$y_hat, na.rm=TRUE), 1, length.out=4),
                                   seq(1, max(df_result_all_sort$y_hat, na.rm=TRUE), length.out=4)))
    }else{
      color_values <- as.numeric(seq(min(df_result_all_sort$y_hat, na.rm=TRUE), max(df_result_all_sort$y_hat, na.rm=TRUE), length.out=8))
    }
    scale_fill_gradientn_obj <- scale_fill_gradientn(limits = heat_limits, 
                                       values=scales::rescale(color_values),
                                       colours = rev(palette_diy(8)),
                                       na.value = NA)
    theme_obj <- theme_minimal()+
      theme(plot.subtitle = element_text(hjust = 0.5, face="bold", colour="black", size = 15),
            axis.ticks.length=unit(-0.25, "cm"),
            axis.text.y = element_text(face="bold", colour="black", size=12),
            axis.title.y = element_blank(),
            axis.text.x = element_text(face="bold", colour="black", size=12),
            axis.title.x = element_text(face="bold", colour="black", size=14),
            legend.title = element_text(face="bold", colour="black", size=14),
            legend.text = element_text(face="bold", colour="black", size=12)) 
    
    
    # plot in one object
    plot_obj <- ggplot(df_result_all_sort, aes(x=x_pctl,y=x_name))+
      geom_tile(aes(fill=y_hat)) +
      geom_text(aes(label=c_label), hjust=0, vjust=-0.2, na.rm = TRUE, check_overlap = TRUE, size = 5) + #"left"
      labs(fill=gsub("_"," ",y_map_func),x=NULL) + #"Percentile"
      theme_obj + 
      scale_fill_gradientn_obj +
      scale_y_discrete(expand = c(0,0),
                       limits=df_order$x_name, 
                       labels=stringr::str_wrap(gsub("_"," ",df_order$x_name), width=20))
    # indicate significancy
    if("y_logodds_signif"%in%colnames(df_result_all_sort)){
      plot_obj <-  plot_obj +
        geom_point(aes(y=ifelse(y_logodds_signif%in%c(1),NA,x_name) ))
    }
    
    
    # plots in list
    plot_list <- list()
    for(x in rev(df_order$x_name)){
      subdf <- df_result_all_sort[which(df_result_all_sort$x_name==x),]
      subdf <- subdf[order(subdf$x_pctl),]
      
      # map x raw value back to each x axis in the plot list if x in scale of percentile 
      plot_list[[x]] <- ggplot(subdf, aes(x=x_pctl,y=x_name))+
        geom_tile(aes(fill=y_hat)) +
        geom_text(aes(label=c_label), hjust=0, vjust=-0.2, na.rm = TRUE, check_overlap = TRUE, size = 5) + 
        labs(subtitle=gsub("_"," ",x),fill=y_map_func,x=NULL,y=NULL)+
        theme_obj + 
        theme(axis.text.y =  element_blank())+
        scale_fill_gradientn_obj
      
      # map x raw value back to each x axis in the plot list if x in scale of percentile 
      x_label_map <- NULL
      rows <- c(1, 
                max(round(length(subdf$x_pctl)*0.05),1),
                round(length(subdf$x_pctl)*0.25), 
                round(length(subdf$x_pctl)*0.5), 
                round(length(subdf$x_pctl)*0.75), 
                min(round(length(subdf$x_pctl)*0.95),length(subdf$x_pctl)), 
                length(subdf$x_pctl))
      x_label_map <- subdf[rows,c("x_pctl","x_raw")]
      x_label_map$x_labels <- paste0(c(0,5,25,50,75,95,100),"th\n",round(x_label_map$x_raw,1))
      
      plot_list[[x]] <- plot_list[[x]] +
        scale_x_continuous(breaks = x_label_map$x_pctl[c(1,3,4,5,7)],
                           labels = x_label_map$x_labels[c(1,3,4,5,7)],
                           # Add a second axis and specify its features
                           sec.axis = sec_axis(~., 
                                               breaks = x_label_map$x_pctl[c(2,6)],
                                               labels = x_label_map$x_labels[c(2,6)] )
                           ) + 
        scale_y_discrete(expand = c(0,0))
      # indicate significancy
      if("y_logodds_signif"%in%colnames(subdf)){
        plot_list[[x]] <-  plot_list[[x]] +
          geom_point(aes(y=ifelse(y_logodds_signif%in%c(1),NA,x_name) ))
      }
      
    }
    # if(all(df_result_all_sort$x_pctl<=1)){
    # }else{
    #   
    #   # plots in list
    #   plot_list <- list()
    #   for(x in rev(df_order$x_name)){
    #     subdf <- df_result_all_sort[which(df_result_all_sort$x_name==x),]
    #     
    #     # using raw x scale
    #     plot_list[[x]] <- ggplot(subdf, aes(x=x_raw,y=x_name))+
    #       geom_tile(aes(fill=y_hat)) +
    #       geom_text(aes(label=c_label), hjust=0, vjust=-0.2, na.rm = TRUE, check_overlap = TRUE, size = 5) + 
    #       labs(subtitle=gsub("_"," ",x),fill=y_map_func,x=NULL,y=NULL)+
    #       theme_obj + 
    #       theme(axis.text.y =  element_blank())+
    #       scale_fill_gradientn_obj + 
    #       scale_y_discrete(expand = c(0,0)) 
    #   }
    #   
    #   if("y_logodds_signif"%in%colnames(subdf)){
    #     plot_list[[x]] <-  plot_list[[x]] +
    #       geom_point(aes(y=ifelse(y_logodds_signif%in%c(1),NA,x_name) ))
    #   }
    #   # plot in raw scale in one object
    #   layout_ncol <- min(length(plot_list), layout_ncol)
    #   plot_obj <- ggpubr::ggarrange(plotlist = plot_list, 
    #                                 ncol=layout_ncol, 
    #                                 nrow=ceiling(length(plot_list)/layout_ncol),
    #                                 common.legend = TRUE, 
    #                                 legend = "right")
    #   
    # }
  }
  
    
  return(list(plot_obj = plot_obj,
              plot_list = plot_list,
              df_result_all_sort = df_result_all_sort))
  
}

############################################ not run #############################################
# data = data_ml
# dict_data = dict_ml
# # --- setup ---
# trim_by_label = "Post-menstrual Age"
# trim_vec = c(22, 40)
# time_unit= 7
# trim_ctrl=FALSE
# imputation="None"
# impute_per_cluster=FALSE
# winsorizing=TRUE
# aggregate_per=c("row", "cluster_trim_by_unit", "cluster")[2]
# pctcut_num_labels=c()
# pctcut_num_vec=c(0.1, 99.9)
# pctcut_num_coerce = TRUE
# filter_tag_labels=c("On respiratory support with endotracheal tube (EN) == Yes", "Any  Doses of any medication today")
# # --- local ---
# num_labels = dict_data$label[which(dict_data$type=="num")][c(1:5)]
# y_label = "Primary outcome (EN) == Unfavorable"
# cluster_label = "PreVent study ID"
# num_adjust_label = NULL
# method="logit_rcs"
# y_map_func=c("fold_risk", "probability", "log_odds")[1]
# y_map_max=3
# sample_per_cluster = NULL



