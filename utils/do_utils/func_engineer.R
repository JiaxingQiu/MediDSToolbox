engineer <- function(
  data, # dataframe object to engineer
  trim_by_col = NULL,
  trim_min = -Inf, # [from,
  trim_max = Inf, # to)
  trim_keepna = FALSE,
  num_cols = c(),
  fct_cols = c(),
  cluster_col = NULL, 
  imputation=c("None","Mean", "Median", "Zero")[1],
  impute_per_cluster=FALSE,
  winsorizing=FALSE,
  aggregation=FALSE
){
  
  # ---- validate inputs ----
  trim_by_col <- intersect(colnames(data), trim_by_col)
  num_cols <- intersect(colnames(data), num_cols)
  fct_cols <- intersect(colnames(data), fct_cols)
  cluster_col <- intersect(colnames(data), cluster_col)
  if (trim_max<=trim_min){
    trim_min <- -Inf
    trim_max <- Inf
  }
  
  # ---- trim data by trim_by_col  ----
  if (length(trim_by_col)>0){
    if (trim_keepna){
      data <- data[which((data[,trim_by_col]>=trim_min & data[,trim_by_col]<trim_max)|is.na(data[,trim_by_col])),]
    }else{
      data <- data[which(data[,trim_by_col]>=trim_min & data[,trim_by_col]<trim_max),]
    }
  }
  # ---- impute num_cols ----
  if(!impute_per_cluster){
    # global imputation
    if(imputation=="Mean") data <- data %>% mutate_at(vars(num_cols), ~tidyr::replace_na(., mean(.,na.rm = TRUE))) %>% as.data.frame()
    if(imputation=="Median") data <- data %>% mutate_at(vars(num_cols), ~tidyr::replace_na(., median(.,na.rm = TRUE))) %>% as.data.frame()
    if(imputation=="Zero") data <- data %>% mutate_at(vars(num_cols), ~tidyr::replace_na(., 0)) %>% as.data.frame()
    
  }else{
    # cluster-wise imputation
    if(imputation=="Mean") data %>% group_by(vars(cluster_col)) %>% mutate_at(vars(num_cols),  ~tidyr::replace_na(., mean(.,na.rm = TRUE)) ) %>% as.data.frame()
    if(imputation=="Median") data %>% group_by(vars(cluster_col)) %>% mutate_at(vars(num_cols),  ~tidyr::replace_na(., median(.,na.rm = TRUE)) ) %>% as.data.frame()
    if(imputation=="Zero")  data %>% group_by(vars(cluster_col)) %>% mutate_at(vars(num_cols),  ~tidyr::replace_na(., 0)) %>% as.data.frame()
  }
  # winsorize numeric columns
  if(winsorizing){
    data[,num_cols] <- winsorize(data[,num_cols])
  }
  # aggregation if required
  if(aggregation & length(cluster_col)>0){
    df_key <- NULL
    df_fct <- NULL
    df_num <- NULL
    df_key <- data.frame(cluster_col = unique(data[,cluster_col]), stringsAsFactors = FALSE)
    colnames(df_key) <- cluster_col
    # num - mean
    if(length(num_cols)>0){
      df_num <- data %>% group_by(data[,cluster_col]) %>% summarise_at(vars(num_cols), ~mean(.,na.rm = TRUE)) %>% as.data.frame()
      colnames(df_num) <- c(cluster_col, num_cols)
    }
    # fct - max
    if(length(fct_cols)>0){
      df_fct <- data %>% group_by(data[,cluster_col]) %>% 
        summarise_at(vars(fct_cols), ~max(.,na.rm = TRUE)) %>%
        mutate_at(vars(fct_cols), function(x) ifelse(is.infinite(x), 0, x)) %>%
        as.data.frame()
      colnames(df_fct) <- c(cluster_col, fct_cols)
    }
    data <- df_key
    if (!is.null(df_fct)){
      data <- merge(data, df_fct)
    }
    if (!is.null(df_num)){
      data <- merge(data, df_num)
    }
  }
  return(data)
}
