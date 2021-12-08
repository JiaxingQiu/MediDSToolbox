front_multi_heatmap <- function(data=subset_df(data_ml,"40w"),
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
                              impute_per_cluster=FALSE){
  
  
  trim_by_col <- rownames(dict_data[which(dict_data$label_front==trim_by_label), ])
  data <- data %>% filter(data[,trim_by_col]>=trim_vec[1]*time_unit & data[,trim_by_col]<trim_vec[2]*time_unit ) %>% as.data.frame()
  
  
  num_cols <- intersect(rownames(dict_data[which(dict_data$label_front%in%num_labels), ]), rownames(dict_data[which(dict_data$mlrole=="input"&dict_data$type=="num"), ]))
  tag_col <- rownames(dict_data[which(dict_data$label_front==y_label),])
  cluster_col <- rownames(dict_data[which(dict_data$label_front==cluster_label),])
  
  # impute numeric inputs
  if(!impute_per_cluster){
    if(imputation=="Mean"){
      data <- data %>% mutate_at(vars(num_cols), ~tidyr::replace_na(., mean(.,na.rm = TRUE))) %>% as.data.frame()
    }
    if(imputation=="Median"){
      data <- data %>% mutate_at(vars(num_cols), ~tidyr::replace_na(., median(.,na.rm = TRUE))) %>% as.data.frame()
    }
    if(imputation=="Zero"){
      data <- data %>% mutate_at(vars(num_cols), ~tidyr::replace_na(., 0)) %>% as.data.frame()
    }
  }else{
    if(imputation=="Mean"){
      for(sbj in unique(data[,cluster_col]) ){
        for(col in num_cols){
          data[which(data[,cluster_col]==sbj & is.na(data[,col])), col] <- mean(data[which(data[,cluster_col]==sbj), col], na.rm=TRUE)
        }
      }
    }
    if(imputation=="Median"){
      for(sbj in unique(data[,cluster_col]) ){
        for(col in num_cols){
          data[which(data[,cluster_col]==sbj & is.na(data[,col])), col] <- median(data[which(data[,cluster_col]==sbj), col], na.rm=TRUE)
        }
      }
    }
    if(imputation=="Zero"){
      for(sbj in unique(data[,cluster_col]) ){
        for(col in num_cols){
          data[which(data[,cluster_col]==sbj & is.na(data[,col])), col] <- 0
        }
      }
    }
  }
  if(winsorizing){
    data[,num_cols] <- winsorize(data[,num_cols])
  }
  if(aggregation){
    df_tag <- NULL
    df_num <- NULL
    # y tag - max 
    df_tag <- data %>% group_by(data[,cluster_col]) %>% summarise_at(vars(tag_col), ~max(.,0,na.rm = TRUE)) %>% as.data.frame()
    colnames(df_tag) <- c(cluster_col, tag_col)
    # x num - mean
    if(length(num_cols)>0){
      df_num <- data %>% group_by(data[,cluster_col]) %>% summarise_at(vars(num_cols), ~mean(.,na.rm = TRUE)) %>% as.data.frame()
      colnames(df_num) <- c(cluster_col, num_cols)
    }
    
    data <- df_tag
    
    if (!is.null(df_num)){
      data <- merge(data, df_num)
    }
  }
  data <- assign.dict(data, dict_data)
  
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
    
    df_result_all <- uni_tag_nums(data, num_cols, tag_col, cluster_col, num_adjust_col, method=method, pct=pct)
    var_order <- df_result_all %>% group_by(var_name) %>% summarise(max_prob=max(prob)) %>% arrange(max_prob) %>% as.data.frame()
    df_result_all_sort <- dplyr::left_join(var_order,df_result_all)
    plot_obj <- ggplot(df_result_all_sort, aes(x=pctl,y=var_name))+
      geom_tile(aes(fill=prob)) +
      labs(fill="Probability",x="Percentile") + 
      theme(axis.title.y=element_blank()) + 
      scale_fill_gradientn(colours = rev(rainbow(7))) +
      scale_y_discrete(limits=var_order$var_name)
    
  }
  return(plot_obj)
  
}


