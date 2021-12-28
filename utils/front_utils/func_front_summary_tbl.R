front_summary_tbl <- function(data = subset_df(data_ml,"40w"),
                              dict_data = dict_ml,
                              trim_by_label="Post-menstrual Age",
                              trim_vec = c(22, 40),
                              time_unit = 7,
                              stratify_by="Primary outcome (EN)___Unfavorable", 
                              cluster_label=cluster_front_labels[1]){
  
  
  
  #as.numeric(gsub('[^0-9.]','',df_name))
  trim_by_col <- intersect(colnames(data),rownames(dict_data[which(dict_data$label_front==trim_by_label), ]))
  if (length(trim_by_col)>0){
    data <- data %>% filter(data[,trim_by_col]>=trim_vec[1]*time_unit & data[,trim_by_col]<trim_vec[2]*time_unit ) %>% as.data.frame()
  }
  
  num_cols <- intersect(colnames(data), rownames(dict_data[which(dict_data$mlrole=="input"&dict_data$type=="num"), ]) )
  fct_cols <- intersect(colnames(data), rownames(dict_data[which(dict_data$mlrole=="input"&dict_data$type=="fct"&dict_data$unit!="tag01"), ]) )
  tag_cols <- intersect(colnames(data), rownames(dict_data[which(dict_data$mlrole=="input"&dict_data$type=="fct"&dict_data$unit=="tag01"), ]) )
  
  clu_col <- intersect(colnames(data), rownames(dict_data[which(dict_data$label_front==cluster_label),]) )
  stratify_col <- intersect(colnames(data), rownames(dict_data[which(dict_data$label_front==stratify_by),]) )
  data$key <- data[,clu_col]
  
  # --- summary table ---
  res_df <- NULL
  num_detail_df <- NULL
  fct_detail_df <- NULL
  rsps_df <- NULL
  tryCatch({
    # convert numeric stratify_col to discrete IQR groups
    df_agg <- data.frame(key=unique(data$key),stringsAsFactors=FALSE)
    if(!is.null(num_cols)){
      df <- data %>% group_by(key) %>% 
        summarise_at(num_cols, list(mean = ~mean(., na.rm=TRUE))) %>% 
        as.data.frame()
      colnames(df)<-c('key',num_cols)
      df_agg <- merge(df_agg, df, by='key',all.x=TRUE)
    }
    if(!is.null(fct_cols)){
      df <- data %>% group_by(key) %>% 
        summarise_at(fct_cols, list(ever = ~factor(ifelse(max(., na.rm=TRUE)>0,"Yes","No"), levels = c('No','Yes')))) %>% 
        as.data.frame()
      colnames(df)<-c('key',fct_cols)
      df_agg <- merge(df_agg, df, by='key',all.x=TRUE)
    }
    if(!is.null(stratify_col)){
      if (dict_data[stratify_col,"type"]=="num"){
        # aggregate by mean
        df <- data %>% group_by(key) %>% 
          summarise_at(stratify_col, list(mean = ~mean(., na.rm=TRUE))) %>% 
          as.data.frame()
        colnames(df)<-c('key',stratify_col)
        # cut by quantiles
        df[,paste0(stratify_col,"_IQR")] <- NA
        dict_data[paste0(stratify_col,"_IQR"),] <- ""
        df[which(df[,stratify_col]<quantile(df[,stratify_col],0.25,na.rm = TRUE)),paste0(stratify_col,"_IQR")] <- "[0th,25th)"
        df[which(df[,stratify_col]>=quantile(df[,stratify_col],0.25,na.rm = TRUE) & df[,stratify_col]<quantile(df[,stratify_col],0.5,na.rm = TRUE)),paste0(stratify_col,"_IQR")] <- "[25th,50th)"
        df[which(df[,stratify_col]>=quantile(df[,stratify_col],0.50,na.rm = TRUE) & df[,stratify_col]<quantile(df[,stratify_col],0.75,na.rm = TRUE)),paste0(stratify_col,"_IQR")] <- "[50th,75th)"
        df[which(df[,stratify_col]>=quantile(df[,stratify_col],0.75,na.rm = TRUE)),paste0(stratify_col,"_IQR")] <- "[75th,100th]"
        dict_data[paste0(stratify_col,"_IQR"),"label"] <- as.character( paste0("IQR of ",dict_data[stratify_col,"label"]) )
        stratify_col <- paste0(stratify_col,"_IQR")
        df_agg <- merge(df_agg, df[, c('key',stratify_col)], by='key',all.x=TRUE)
      }
      
      else if (dict_data[stratify_col,"type"]=="fct"){
        df <- data %>% group_by(key) %>% 
          summarise_at(stratify_col, list(ever = ~factor(ifelse(max(., na.rm=TRUE)>0,"Yes","No"), levels = c('No','Yes')))) %>% 
          as.data.frame()
        colnames(df)<-c('key',stratify_col)
        df_agg <- merge(df_agg, df, by='key',all.x=TRUE)
      }
      
    }
    
    for (var in colnames(df_agg)){
      label(df_agg[,var]) <- dict_data[var,"label"]
    }
    
    # summary table stratefied by response
    tbl_obj <- summ_tbl(data=df_agg, num_vars = num_cols, fct_vars = fct_cols, num_denom = "avail", fct_denom = "known", keys=c('key'),y=c(stratify_col) )
    tbl_st <- print(tbl_obj$tbl, varLabels = TRUE)
    res_df <- as.data.frame(tbl_st)%>%tibble::rownames_to_column()
    res_df <- res_df[which(res_df$rowname!="n.1"),]
    res_df$rowname  <- gsub("..median..IQR..", " median[IQR]", res_df$rowname )
    res_df$rowname  <- gsub("..mean..SD..", " mean(SD)", res_df$rowname )
    res_df$rowname  <- gsub("..Yes....", " = Yes", res_df$rowname )
    colnames(res_df)[which(!colnames(res_df)%in%c("rowname","Overall"))] <- paste0(stratify_col," = ",colnames(res_df)[which(!colnames(res_df)%in%c("rowname","Overall"))]) 
    
    # numeric and factor predictor details
    num_detail_df <- tbl_obj$num_detail_df
    fct_detail_df <- tbl_obj$fct_detail_df
    
    # factor response details
    if(dict_data$type[which(dict_data$varname==stratify_col)]=="fct"){
      
      data$y <- data[,stratify_col]
      data$clu_col <- data[,clu_col]
      data$trim_by_col <- data[,trim_by_col]
      rsps_df <- data %>% 
        filter(!is.na(trim_by_col)) %>%
        group_by(y) %>% summarise(
        n_cluster = n_distinct(clu_col),
        n_episode = sum(diff(trim_by_col)<0), # number of episodes counted by the jump of relative time
        n_row = n()
      ) %>% as.data.frame()
    }
    
    
  },error=function(e){
    print(e)
  })
  
  # --- NA plot ---
  for (fct_col in fct_cols){
    data[which(data[,fct_col]=="None_level"), fct_col] <- NA
  }
  na_obj <-  Hmisc::naclus(data[,c(num_cols,fct_cols)])
  
  return(list("summ_df" = res_df, 
              "num_detail_df" = num_detail_df,
              "fct_detail_df" = fct_detail_df,
              "rsps_df" = rsps_df,
              "na_obj"=na_obj))
  
}
