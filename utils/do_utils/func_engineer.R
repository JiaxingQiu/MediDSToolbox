engineer <- function(
  data, # dataframe object to engineer
  num_cols = c(), # vector of numeric columns
  fct_cols = c(), # vector of factor columns
  cluster_col, # cluster column
  trim_by_col,
  #--- trim data ---
  trim_min = -Inf, # [from,
  trim_max = Inf, # to)
  trim_step_size = 1,
  trim_keepna = FALSE, # whether or not to keep NA in relative time variable
  pctcut_num_cols=c(),
  pctcut_num_vec=c(0.1, 99.9),
  pctcut_num_coerce = TRUE,
  filter_tag_cols=c(),
  #--- decorate data ---
  imputation=c("None", "Mean", "Median", "Zero")[1],
  impute_per_cluster=FALSE,
  winsorizing=FALSE,
  aggregate_per=c("row", "cluster_trim_by_unit", "cluster")[1]
){
  
  # ---- validate inputs ----
  trim_step_size <- max(1,as.numeric(trim_step_size),na.rm=TRUE)
  trim_min <- trim_min*trim_step_size
  trim_max <- trim_max*trim_step_size
  if (trim_max<=trim_min){
    trim_min <- -Inf
    trim_max <- Inf
  }
  trim_by_col <- intersect(colnames(data), trim_by_col)
  cluster_col <- intersect(colnames(data), cluster_col)
  num_cols <- intersect(colnames(data), num_cols)
  num_cols <- union(num_cols, trim_by_col)
  num_cols <- union(num_cols, pctcut_num_cols)
  fct_cols <- intersect(colnames(data), fct_cols)
  fct_cols <- union(fct_cols, cluster_col)
  fct_cols <- union(fct_cols, filter_tag_cols)
  
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
        # if coerce extrema
        data[,pctcut_num_col] <- ifelse(data[,pctcut_num_col] < quantiles[1], quantiles[1], data[,pctcut_num_col])
        data[,pctcut_num_col] <- ifelse(data[,pctcut_num_col] > quantiles[2], quantiles[2], data[,pctcut_num_col])
      }else{
        # otherwise remove extrema
        data[,pctcut_num_col] <- ifelse(data[,pctcut_num_col] < quantiles[1], NA, data[,pctcut_num_col])
        data[,pctcut_num_col] <- ifelse(data[,pctcut_num_col] > quantiles[2], NA, data[,pctcut_num_col])
      }
    },error=function(e){
      print(paste0("--- Skip percentile cutoff for num variable ",pctcut_num_col," ---"))
      print(e)
    })
  }
  # filter data with complete numeric cutoffs
  data <- data[which(complete.cases(data[,pctcut_num_cols])),]
  
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
  # winsorize numeric columns except trim by col
  if(winsorizing){
    data[,setdiff(num_cols,trim_by_col)] <- winsorize(data[,setdiff(num_cols,trim_by_col)])
  }
  # aggregation if required
  if(aggregate_per %in% c("cluster_trim_by_unit", "cluster") ){ 
    df_key <- NULL
    df_tag <- NULL
    df_cat <- NULL
    df_num <- NULL
    
    if (aggregate_per=="cluster_trim_by_unit"){
      # rounding trim by column as multiple value of trim by step size / time unit
      data[,trim_by_col] <- floor(data[,trim_by_col]/trim_step_size)*trim_step_size
      data$key_col <- paste0(data[,cluster_col], "___", floor(data[,trim_by_col]/trim_step_size))
    }else if (aggregate_per=="cluster"){
      data$key_col <- data[,cluster_col]
    }
    # key 
    df_key <- data.frame(key_col = unique(data$key_col), stringsAsFactors = FALSE)
    # num - mean
    if(length(num_cols)>0){
      df_num <- data %>% group_by(key_col) %>% 
        summarise_at(vars(all_of(num_cols)), ~mean(.,na.rm = TRUE)) %>% 
        as.data.frame()
      colnames(df_num) <- c("key_col", num_cols)
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
      df_tag <- data %>% group_by(key_col) %>% 
        summarise_at(vars(all_of(tag_cols)), ~max(.)) %>%
        mutate_at(vars(all_of(tag_cols)), function(x) ifelse(is.infinite(x), NA, x)) %>%
        as.data.frame()
      colnames(df_tag) <- c("key_col", tag_cols)
      df_cat <- data %>% group_by(key_col) %>% 
        summarise_at(vars(all_of(cat_cols)), ~unique(.)[which(!is.na(unique(.)))][1] ) %>% # use the first not na value
        as.data.frame()
      colnames(df_cat) <- c("key_col", cat_cols)
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
  data_engineered <- data[, intersect(colnames(data),unique(c("key_col",cluster_col, trim_by_col, num_cols, fct_cols)))]
  
  
  # ---- return final engineered dataset ----
  return(data_engineered)
}








# ############################### not run ######################################
# 
# data = data_ml # dataframe object to engineer
# num_cols = c("baby_weight_lin") # vector of numeric columns
# fct_cols = c("delivery_mode_factor", "posair_ynunk_drvd___tag_factor_Yes") # vector of factor columns
# cluster_col = "subjectnbr" # cluster column
# trim_by_col = "pma_days"
# #--- trim data ---
# trim_min = 35 # [from,
# trim_max = 37 # to)
# trim_step_size = 7
# trim_keepna = FALSE # whether or not to keep NA in relative time variable
# pctcut_num_cols=c()
# pctcut_num_vec=c(0.1, 99.9)
# pctcut_num_coerce = TRUE
# filter_tag_cols=c()
# #--- decorate data ---
# imputation=c("None","Mean", "Median", "Zero")[1]
# impute_per_cluster=FALSE
# winsorizing=FALSE
# aggregate_per=c("row", "cluster_trim_by_unit", "cluster")[2]


