front_viz_1d_stats <- function(
  data,
  dict_data,
  y_label, # num or fct(tag) y / response variable
  x_label, # num or fct
  # setup
  cluster_label,
  trim_by_label, #trim by time column
  trim_vec = c(-Inf, Inf), 
  time_unit = 1,
  pctcut_num_labels = c(), # cutoff by percentile of one or more numeric variable
  pctcut_num_vec = c(0, 100),
  pctcut_num_coerce = TRUE,
  filter_tag_labels = c(), # tag columns
  imputation=c("None","Mean", "Median", "Zero")[1],
  impute_per_cluster=FALSE,
  winsorizing=FALSE,
  aggregate_per=c("row", "cluster_trim_by_unit", "cluster")[1],
  # distributions
  group_by_label="None", # fct
  optimized_smoother=FALSE,
  label_peak=FALSE,
  span=0.5
){
  # ---- Usage ----
  # uni eda: visualize detailed information of response against one X variable(uni)
  
  # ---- Arguments ----
  # data : dataframe object
  # dict_data: dictionary for associated dataframe
  # trim_by_label: label of the time-related variable to filter from
  # trim_vec: vector of start and ends of trim_by variable
  # time_unit: trim_by var resolution
  # filter_tag_labels: filter data where all the input tag columns are 1 
  # x_label: x axis in the relationship
  # y_label: y_axis in the relationship
  # group_by_label: make multiple plots in one figure given group_by variable  
  
  # ---- Value ----
  # uni_eda object: plot_1d_stats
  
  tryCatch({
    data <- assign.dict(data, dict_data, overwrite = TRUE)
    dict_data <- get.dict(data)
  },error=function(e){
    print("--- Skip refine dictionary from data ---")
    print(e)
  })
  
  # find columns names from input labels
  x_col <- dict_data$varname[which(dict_data$label%in%x_label)]
  stopifnot(length(x_col)<=1)
  y_col <- dict_data$varname[which(dict_data$label==y_label)]
  stopifnot(length(y_col)<=1)
  cluster_col <- dict_data$varname[which(dict_data$label==cluster_label)]
  stopifnot(length(cluster_col)<=1)
  group_by_col <- dict_data$varname[which(dict_data$label==group_by_label)]
  stopifnot(length(group_by_col)<=1)
  num_cols <- intersect( dict_data$varname[which(dict_data$type=="num")], c(x_col, y_col, group_by_col) )
  fct_cols <- intersect( dict_data$varname[which(dict_data$type=="fct")], c(x_col, y_col, group_by_col))
  trim_by_col <- dict_data$varname[which(dict_data$label==trim_by_label)]
  pctcut_num_cols <- dict_data$varname[which(dict_data$label%in%pctcut_num_labels)]
  filter_tag_cols <- dict_data$varname[which(dict_data$label%in%filter_tag_labels & dict_data$unit=="tag01")]
  
  
  data <- engineer(data = data,
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
  
  
  
  # --- conditional distribution plots ----
  plot_1d_stats_obj <- NULL
  try({
    if(!optimized_smoother){
      plot_1d_stats_obj <- viz_1d_stats(data=data,
                                        dict_data = dict_data,
                                        x_col = x_col,
                                        y_col = y_col,
                                        cluster_col = cluster_col,
                                        group_by_col = group_by_col,
                                        label_peak = label_peak,
                                        span=span)
    }else{
      plot_1d_stats_obj <- viz_1d_stats_optimize(data=data,
                                                 dict_data = dict_data,
                                                 x_col = x_col,
                                                 y_col = y_col,
                                                 cluster_col = cluster_col,
                                                 group_by_col = group_by_col)
    }
    
    if (length(group_by_col)>=1){
      plot_1d_stats_obj$p_pct <-  plot_1d_stats_obj$p_pct + ggtitle(paste0("Percentile (group by ", group_by_label,")"))
      plot_1d_stats_obj$p_mean <-  plot_1d_stats_obj$p_mean + ggtitle(paste0("Mean (group by ", group_by_label,")"))
      plot_1d_stats_obj$p_denom <-  plot_1d_stats_obj$p_denom + ggtitle(paste0("Denominator (group by ", group_by_label,")"))
      plot_1d_stats_obj$p_violin <-  plot_1d_stats_obj$p_violin + ggtitle(paste0("Violin + Box (group by ", group_by_label,")"))
    }
    
  },TRUE)
  return(plot_1d_stats_obj)
}
