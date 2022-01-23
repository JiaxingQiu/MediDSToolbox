front_viz_alluvial <- function(
  data,
  dict_data,
  y_label,
  cluster_label, 
  # --- engineer ---
  trim_by_label, #trim by time column
  trim_vec = c(-Inf, Inf), 
  time_unit = 1,
  pctcut_num_labels = c(), # cutoff by percentile of one or more numeric variable
  pctcut_num_vec = c(0, 100),
  pctcut_num_coerce = TRUE,
  filter_tag_labels = c(), # tag columns
  imputation=c("None","Mean", "Median", "Zero")[1],
  impute_per_cluster=FALSE,
  winsorizing=TRUE,
  # alluvial
  time_label, # x axis is time-related breaks
  time_breaks = c(), # vector of time window breaks
  time_quantity = c("average", "1st record")[1], # quantify responce y in each time window by average or 1st record
  tag_labels = c(), # additional tag status
  includeNA=TRUE
){
  
  tryCatch({
    data <- assign.dict(data, dict_data, overwrite = TRUE)
    dict_data <- get.dict(data)
  },error=function(e){
    print("--- Skip refine dictionary from data ---")
    print(e)
  })
  
  # find columns names from input labels
  time_col <- dict_data$varname[which(dict_data$label%in%time_label)]
  y_col <- dict_data$varname[which(dict_data$label==y_label)]
  cluster_col <- dict_data$varname[which(dict_data$label==cluster_label)]
  tag_cols <- dict_data$varname[which(dict_data$label%in%tag_labels)]
  trim_by_col <- dict_data$varname[which(dict_data$label==trim_by_label)]
  pctcut_num_cols <- dict_data$varname[which(dict_data$label%in%pctcut_num_labels)]
  filter_tag_cols <- dict_data$varname[which(dict_data$label%in%filter_tag_labels & dict_data$unit=="tag01")]
  num_cols <- intersect( dict_data$varname[which(dict_data$type=="num")], c(time_col, y_col, tag_cols) )
  fct_cols <- intersect( dict_data$varname[which(dict_data$type=="fct")], c(time_col, y_col, tag_cols))
  
  # engineer the data
  data_engineered <- engineer(data = data,
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
                   aggregate_per = "row") # always set aggregation to be false
  
  
  
  data <- data_engineered
  plot_obj <- NULL
  print("---- viz_alluvial ----")
  tryCatch({
    plot_obj <- viz_alluvial(
      data = data,
      dict_data = dict_data,
      time_col = time_col, # x axis is time-related breaks
      time_unit = time_unit, # time unit in each break window
      time_breaks = time_breaks, # vector of time window breaks
      time_quantity = time_quantity, # quantify responce y in each time window by average or 1st record
      y_col = y_col, # responce y variable (only numeric)
      cluster_col = cluster_col,
      tag_cols = tag_cols, # additional tag status
      includeNA=includeNA
    )
  },error=function(e){
    print("Error")
    print(e)
  })
    
  return(plot_obj)
  
}

# ################## not run #######################
# data = data_ml
# dict_data = dict_ml
# y_label = "Desat_75 number of events per day" # responce y variable (only numeric)
# cluster_label = "PreVent study ID"
# 
# # --- engineer ---
# trim_by_label = "Post-menstrual Age" #trim by time column
# trim_vec = c(22, 37)
# time_unit = 7
# pctcut_num_labels = c() # cutoff by percentile of one or more numeric variable
# pctcut_num_vec = c(0, 100)
# pctcut_num_coerce = TRUE
# filter_tag_labels = c() # tag columns
# imputation=c("None","Mean", "Median", "Zero")[1]
# impute_per_cluster=FALSE
# winsorizing=TRUE
# 
# # --- local ---
# time_label = "Post-menstrual Age" # x axis is time-related breaks
# time_breaks = c(28,36,40) # vector of time window breaks
# time_quantity = c("average", "1st record")[2] # quantify responce y in each time window by average or 1st record
# tag_labels = c("Date of death tag", "Exit date tag", "On respiratory support with endotracheal tube (derived from intub/extub time)") # additional tag status
