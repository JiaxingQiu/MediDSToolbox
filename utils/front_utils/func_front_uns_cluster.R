front_front_uns_cluster <- function(
  # global parameters (unsupervised setup page)
  data = data_ml,
  dict_data = dict_ml,
  cluster_label = "PreVent study ID",
  trim_by_label="Post-menstrual Age",
  trim_vec = c(22, 40),
  time_unit=7,
  pctcut_num_labels = c("Birth weight"), # cutoff by percentile of one or more numeric variable
  pctcut_num_vec = c(0.1, 99.9),
  coerce = TRUE,
  filter_tag_labels = c("On respiratory support without endotracheal tube  (EN)___Yes"), # tag columns
  imputation="None",
  impute_per_cluster=FALSE,
  winsorizing=FALSE,
  aggregation=TRUE, # always set to be true
  # local parameters
  input_labels=dict_ml$label[which(dict_ml$varname%in%colnames(data_ml%>%select(starts_with("cpd_"))) )],
  nc_vec = c(2,15),
  min_nobs_per_clst=2,
  max_iter=5
  
){
  
  # --- preprocessing ----
  # data dictionary must have valid type information 
  dict_data <- dict_data[which(dict_data$type!=""),]
  
  
  # prepare setup
  input_cols <- rownames(dict_data[which(dict_data$label_front%in%input_labels), ])
  stopifnot(length(input_cols)>=1)
  num_cols <- rownames(dict_data[which(dict_data$type=="num"),])
  fct_cols <- rownames(dict_data[which(dict_data$type=="fct"&dict_data$unit=="tag01"),])
  
  input_cols <- intersect(input_cols, union(num_cols, fct_cols))# refine unsupervised input cols
  cluster_col <- rownames(dict_data[which(dict_data$label_front==cluster_label),])
  stopifnot(length(cluster_col)<=1)
  trim_by_col <- rownames(dict_data[which(dict_data$label_front==trim_by_label), ])
  pctcut_num_cols <- rownames(dict_data[which(dict_data$label_front%in%pctcut_num_labels), ])
  filter_tag_cols <- rownames(dict_data[which(dict_data$label_front%in%filter_tag_labels & dict_data$unit=="tag01"), ])
  
  # trim data by time conditions
  data <- data %>% filter(data[,trim_by_col]>=trim_vec[1]*time_unit & data[,trim_by_col]<trim_vec[2]*time_unit ) %>% as.data.frame()
  # cutoff data by percentile of a numeric variable
  for(pctcut_num_col in pctcut_num_cols){
    quantiles <- quantile( data[,pctcut_num_col], c(as.numeric(pctcut_num_vec[1])/100, as.numeric(pctcut_num_vec[2])/100 ), na.rm =TRUE)
    if(coerce){
      # if coerce extremum
      data[,pctcut_num_col] <- ifelse(data[,pctcut_num_col] < quantiles[1], quantiles[1], data[,pctcut_num_col])
      data[,pctcut_num_col] <- ifelse(data[,pctcut_num_col] > quantiles[2], quantiles[2], data[,pctcut_num_col])
    }else{
      # otherwise remove extremum
      data[,pctcut_num_col] <- ifelse(data[,pctcut_num_col] < quantiles[1], NA, data[,pctcut_num_col])
      data[,pctcut_num_col] <- ifelse(data[,pctcut_num_col] > quantiles[2], NA, data[,pctcut_num_col])
    }
  }
  # filter by all given tag columns 
  if(length(filter_tag_cols)>0){
    for(col in filter_tag_cols){
      data <- data[which(data[,col]==1),]
    }
  }
  
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
  
  
  # aggregation if required
  if(aggregation){
    df_fct <- NULL
    df_num <- NULL
    # x num - mean
    if(length(num_cols)>0){
      df_num <- data %>% group_by(data[,cluster_col]) %>% summarise_at(vars(num_cols), ~mean(.,na.rm = TRUE)) %>% as.data.frame()
      colnames(df_num) <- c(cluster_col, num_cols)
    }
    # x fct - max
    if(length(fct_cols)>0){
      df_fct <- data %>% group_by(data[,cluster_col]) %>% 
        summarise_at(vars(fct_cols), ~max(.,na.rm = TRUE)) %>%
        mutate_all(function(x) ifelse(is.infinite(x), 0, x)) %>%
        as.data.frame()
      colnames(df_fct) <- c(cluster_col, fct_cols)
    }
    # initiate agg df
    df_agg <- data.frame(key = unique(data[,cluster_col]))
    colnames(df_agg) <- cluster_col
    if (!is.null(df_fct)){
      df_agg <- merge(df_agg, df_fct)
    }
    if (!is.null(df_num)){
      df_agg <- merge(df_agg, df_num)
    }
    data <- df_agg
  }
  
  data <- assign.dict(data, dict_data)
  
  
  # number of clusters boundaries
  nc_min <- nc_vec[1]
  nc_max <- nc_vec[2]
  # create unsupervised clustering object
  uns_cluster_obj <- NULL
  tryCatch({
    uns_cluster_obj <- uns_cluster_kmeans(
      data= data, # dataframe with one row per one observation subject (i.e. baby day / baby)
      dict_data=get.dict(data), # dictionary for data
      input_cols=input_cols, # input variables to be used to train clustering model
      nc_max=nc_max, # maximum number of clusters, default 15, must be [1,20]
      nc_min=nc_min, # minimum number of cluster you expect kmeans to split your observations [1,20]
      min_nobs_per_clst=min_nobs_per_clst, # mininal number of observations allowed in a cluster, if a cluster has less than this number of observation, it will be removed
      max_iter=max_iter
    )
  },error = function(e){
    print(e)
    warning("Error --- unsupervised kmeans clustering failed")
  })
  return(uns_cluster_obj)
  
}


