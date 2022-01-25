front_uns_cluster <- function(
  # global parameters (unsupervised setup page)
  data,
  dict_data,
  cluster_label,
  # --- engineer ---
  trim_by_label,
  trim_vec = c(-Inf, Inf),
  time_unit=1,
  pctcut_num_labels = c(), # cutoff by percentile of one or more numeric variable
  pctcut_num_vec = c(0.1, 99.9),
  pctcut_num_coerce = TRUE,
  filter_tag_labels=c(),
  imputation="None",
  impute_per_cluster=FALSE,
  winsorizing=FALSE,
  aggregate_per=c("row", "cluster_trim_by_unit", "cluster")[1],
  # --- local ---
  input_labels=dict_ml$label[which(dict_ml$varname%in%colnames(data_ml%>%select(starts_with("ih_")&ends_with("_dur_prop"))))],
  nc_vec = c(2,15),
  min_nobs_per_clst=2,
  max_iter=5
  
){
  
  tryCatch({
    data <- assign.dict(data, dict_data, overwrite = TRUE)
    dict_data <- get.dict(data)
  },error=function(e){
    print("--- Skip refine dictionary from data ---")
    print(e)
  })
  
  
  # ---- translate front end labels to column names ----
  cluster_col <- dict_data$varname[which(dict_data$label==cluster_label)]
  trim_by_col <- dict_data$varname[which(dict_data$label==trim_by_label)]
  pctcut_num_cols <- dict_data$varname[which(dict_data$label%in%pctcut_num_labels)]
  filter_tag_cols <- dict_data$varname[which(dict_data$label%in%filter_tag_labels & dict_data$unit=="tag01")]
  input_cols <- intersect(dict_data$varname[which(dict_data$label%in%input_labels)],colnames(data))
  stopifnot(length(input_cols)>=1)
  num_cols <- intersect(input_cols, dict_data$varname[which(dict_data$type=="num")])
  fct_cols <- intersect(input_cols, dict_data$varname[which(dict_data$type=="fct"&dict_data$unit=="tag01")])
  
  # ---- engineering ----
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
                      aggregate_per = aggregate_per)
  data <- assign.dict(data_in, dict_data, overwrite = TRUE)
  
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


