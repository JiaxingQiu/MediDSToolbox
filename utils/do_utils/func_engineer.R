engineer <- function(
  data, # dataframe object to engineer
  num_cols = c(), # vector of numeric columns
  fct_cols = c(), # vector of factor columns
  cluster_col, # cluster column
  trim_by_col=NULL,
  #--- trim data ---
  trim_min = -Inf, # [from,
  trim_max = Inf, # to)
  trim_keepna = FALSE, # whether or not to keep NA in relative time variable
  pctcut_num_cols=c(),
  pctcut_num_vec=c(0.1, 99.9),
  pctcut_num_coerce = TRUE,
  filter_tag_cols=c(),
  #--- decorate data ---
  imputation=c("None","Mean", "Median", "Zero")[1],
  impute_per_cluster=FALSE,
  winsorizing=FALSE,
  aggregation=FALSE
){
  
  # ---- validate inputs ----
  if (trim_max<=trim_min){
    trim_min <- -Inf
    trim_max <- Inf
  }
  trim_by_col <- intersect(colnames(data), trim_by_col)
  num_cols <- intersect(colnames(data), num_cols)
  num_cols <- union(num_cols, trim_by_col)
  num_cols <- union(num_cols, pctcut_num_cols)
  fct_cols <- intersect(colnames(data), fct_cols)
  fct_cols <- union(fct_cols, filter_tag_cols)
  cluster_col <- intersect(colnames(data), cluster_col)
  
  # ---- initiate dataframe to return "data_engineered" ----
  # in the returned dataframe, only trim_by_col, num_cols, fct_cols, cluster_cols will be in the columns
  data_engineered <- data
  
  print("---- Engineering ----")
  # ---- trim data by trim_by_col(relative time indicator)  ----
  if (length(trim_by_col)>0){
    if (trim_keepna){
      data <- data[which((data[,trim_by_col]>=trim_min & data[,trim_by_col]<trim_max)|is.na(data[,trim_by_col])),]
    }else{
      data <- data[which(data[,trim_by_col]>=trim_min & data[,trim_by_col]<trim_max),]
    }
  }
  # ---- cutoff numeric variables by percentiles ----
  for(pctcut_num_col in pctcut_num_cols){
    tryCatch({
      quantiles <- quantile( data[,pctcut_num_col], c(as.numeric(pctcut_num_vec[1])/100, as.numeric(pctcut_num_vec[2])/100 ), na.rm =TRUE)
      if(pctcut_num_coerce){
        # if coerce extremum
        data[,pctcut_num_col] <- ifelse(data[,pctcut_num_col] < quantiles[1], quantiles[1], data[,pctcut_num_col])
        data[,pctcut_num_col] <- ifelse(data[,pctcut_num_col] > quantiles[2], quantiles[2], data[,pctcut_num_col])
      }else{
        # otherwise remove extremum
        data[,pctcut_num_col] <- ifelse(data[,pctcut_num_col] < quantiles[1], NA, data[,pctcut_num_col])
        data[,pctcut_num_col] <- ifelse(data[,pctcut_num_col] > quantiles[2], NA, data[,pctcut_num_col])
      }
    },error=function(e){
      print(paste0("--- Skip percentile cutoff for num variable ",pctcut_num_col," ---"))
      print(e)
    })
  }
  
  # ---- subset / filter dataset when all given tag columns == 1 ----
  if(length(filter_tag_cols)>0){
    for(col in filter_tag_cols){
      tryCatch({
        data <- data[which(data[,col]==1),]
      },error=function(e){
        print(paste0("--- Skip one-hot filter for tag varibale", col, " ---"))
        print(e)
      })
    }
  }
  # ---- impute num_cols ----
  if(!impute_per_cluster){
    # global imputation
    if(imputation=="Mean") data <- data %>% mutate_at(vars(all_of(num_cols)), ~tidyr::replace_na(., mean(.,na.rm = TRUE))) %>% as.data.frame()
    if(imputation=="Median") data <- data %>% mutate_at(vars(all_of(num_cols)), ~tidyr::replace_na(., median(.,na.rm = TRUE))) %>% as.data.frame()
    if(imputation=="Zero") data <- data %>% mutate_at(vars(all_of(num_cols)), ~tidyr::replace_na(., 0)) %>% as.data.frame()
  }else{
    # cluster-wise imputation
    if(imputation=="Mean") data %>% group_by(vars(cluster_col)) %>% mutate_at(vars(all_of(num_cols)),  ~tidyr::replace_na(., mean(.,na.rm = TRUE)) ) %>% as.data.frame()
    if(imputation=="Median") data %>% group_by(vars(cluster_col)) %>% mutate_at(vars(all_of(num_cols)),  ~tidyr::replace_na(., median(.,na.rm = TRUE)) ) %>% as.data.frame()
    if(imputation=="Zero")  data %>% group_by(vars(cluster_col)) %>% mutate_at(vars(all_of(num_cols)),  ~tidyr::replace_na(., 0)) %>% as.data.frame()
  }
  # winsorize numeric columns 
  if(winsorizing){
    data[,num_cols] <- winsorize(data[,num_cols])
  }
  # aggregation if required
  if(aggregation & length(cluster_col)>0){
    df_key <- NULL
    df_tag <- NULL
    df_cat <- NULL
    df_num <- NULL
    df_key <- data.frame(cluster_col = unique(data[,cluster_col]), stringsAsFactors = FALSE)
    colnames(df_key) <- cluster_col
    # num - mean
    if(length(num_cols)>0){
      df_num <- data %>% group_by(data[,cluster_col]) %>% 
        summarise_at(vars(all_of(num_cols)), ~mean(.,na.rm = TRUE)) %>% 
        as.data.frame()
      colnames(df_num) <- c(cluster_col, num_cols)
    }
    # fct 
    if(length(fct_cols)>0){
      # (tag) - max
      tag_cols <- c()
      cat_cols <- c()
      for (fct_col in fct_cols){
        if(all(unique(as.character(data[,fct_col]) )%in%c("0","1",NA) )) {
          tag_cols <- c(tag_cols, fct_col)
        }else {
          cat_cols <- c(cat_cols, fct_col)
        }
      }
      df_tag <- data %>% group_by(data[,cluster_col]) %>% 
        summarise_at(vars(all_of(tag_cols)), ~max(.,0, na.rm = TRUE)) %>%
        mutate_at(vars(all_of(tag_cols)), function(x) ifelse(is.infinite(x), 0, x)) %>%
        as.data.frame()
      colnames(df_tag) <- c(cluster_col, tag_cols)
      df_cat <- data %>% group_by(data[,cluster_col]) %>% 
        summarise_at(vars(all_of(cat_cols)), ~unique(.)[which(!is.na(unique(.)))][1] ) %>% # use the first not na value
        as.data.frame()
      colnames(df_cat) <- c(cluster_col, cat_cols)
    }
    data <- df_key
    if (!is.null(df_tag)){
      data <- merge(data, df_tag)
    }
    if (!is.null(df_cat)){
      data <- merge(data, df_cat)
    }
    if (!is.null(df_num)){
      data <- merge(data, df_num)
    }
  }
  data_engineered <- data[,c(cluster_col, trim_by_col, num_cols, fct_cols)]
  
  
  
  # ---- return final engineered dataset ----
  return(data_engineered)
}








# ############################### not run ######################################
# 
# data = data_ml # dataframe object to engineer
# num_cols = c("baby_weight_lin") # vector of numeric columns
# fct_cols = c("delivery_mode_factor", "posair_ynunk_drvd___tag_factor_Yes") # vector of factor columns
# cluster_col = "subjectnbr" # cluster column
# trim_by_col = NULL
# #--- trim data ---
# trim_min = -Inf # [from,
# trim_max = Inf # to)
# trim_keepna = FALSE # whether or not to keep NA in relative time variable
# pctcut_num_cols=c()
# pctcut_num_vec=c(0.1, 99.9)
# pctcut_num_coerce = TRUE
# filter_tag_cols=c()
# #--- decorate data ---
# imputation=c("None","Mean", "Median", "Zero")[1]
# impute_per_cluster=FALSE
# winsorizing=FALSE
# aggregation=TRUE


