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
  aggregation=TRUE # always set to be true
  
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
  # group columns together by typr for data engineering
  fct_cols <- union(x_cols_fct, x_cols_tag) 
  num_cols <- union(union(union(x_cols_linear, x_cols_nonlin_rcs3),x_cols_nonlin_rcs4),x_cols_nonlin_rcs5)
  
  y_col <- rownames(dict_data[which(dict_data$label_front==y_label),])
  cluster_col <- rownames(dict_data[which(dict_data$label_front==cluster_label),])
  rel_time_col <- rownames(dict_data[which(dict_data$label_front==trim_by_label),])
  trim_by_col <- rownames(dict_data[which(dict_data$label_front==trim_by_label), ])
  
  # keep original df to keep temperal information
  data_org <- data
  data_org$cluster_id <- data_org[,cluster_col]
  data_org$rel_time <-  unlist(data_org %>%
                                 group_by(cluster_id) %>%
                                 group_map(~(.x[,rel_time_col] - max(.x[,rel_time_col],na.rm=TRUE)) ), use.names = FALSE)
  
  data_org <- assign.dict(data_org, dict_data)
  
  data <- data %>% filter(data[,trim_by_col]>=trim_vec[1]*time_unit & data[,trim_by_col]<trim_vec[2]*time_unit ) %>% as.data.frame()
  data$cluster_id <- data[,cluster_col]
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
  
  
  # winsorize numeric columns
  if(winsorizing){
    data[,num_cols] <- winsorize(data[,num_cols])
  }
  # aggregatin if required
  if(aggregation){
    df_y <- NULL
    df_fct <- NULL
    df_num <- NULL
    
    # y tag - max 
    if(dict_data[y_col, "type"]=="num"){
      df_y <- data %>% group_by(data[,cluster_col]) %>% summarise_at(vars(y_col), ~mean(.,na.rm = TRUE)) %>% as.data.frame()
      colnames(df_y) <- c(cluster_col, y_col)
    }else if(dict_data[y_col, "type"]=="fct"){
      df_y <- data %>% group_by(data[,cluster_col]) %>% summarise_at(vars(y_col), ~max(.,0,na.rm = TRUE)) %>% as.data.frame()
      colnames(df_y) <- c(cluster_col, y_col) 
    }
    # x num - mean
    if(length(num_cols)>0){
      df_num <- data %>% group_by(data[,cluster_col]) %>% summarise_at(vars(num_cols), ~mean(.,na.rm = TRUE)) %>% as.data.frame()
      colnames(df_num) <- c(cluster_col, num_cols)
    }
    #x fct - max
    if(length(fct_cols)>0){
      df_fct <- data %>% group_by(data[,cluster_col]) %>% summarise_at(vars(fct_cols), ~max(.,0,na.rm = TRUE)) %>% as.data.frame()
      colnames(df_fct) <- c(cluster_col, fct_cols)
    }
    data <- df_y
    if (!is.null(df_fct)){
      data <- merge(data, df_fct)
    }
    if (!is.null(df_num)){
      data <- merge(data, df_num)
    }
  }
  data <- assign.dict(data, dict_data)
  
  x_select_mdls <- do_x_select(data=data,
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