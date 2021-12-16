front_viz_alluvial <- function(
  data = subset_df(data_viz,"40w"),
  dict_data = dict_viz,
  # setup
  trim_by_label = "Post-menstrual Age", #trim by time column
  trim_vec = c(22, 40), 
  time_unit = 7,
  pctcut_num_labels = c("Birth weight"), # cutoff by percentile of one or more numeric variable
  pctcut_num_vec = c(0.1, 99.9),
  coerce = TRUE,
  filter_tag_labels = c("Site (EN)___Alabama",
                        "On respiratory support without endotracheal tube  (EN)___Yes"), # tag columns
  # alluvial
  time_label = "Post-menstrual Age", # x axis is time-related breaks
  time_breaks = c(28,36,40), # vector of time window breaks
  time_quantity = c("average", "1st record")[2], # quantify responce y in each time window by average or 1st record
  y_label = "Birth weight", # responce y variable (only numeric)
  cluster_label = "PreVent study ID",
  tag_labels = c("Date of death tag", "Exit date tag", "On respiratory support with endotracheal tube (derived from intub/extub time)") # additional tag status
  
  
){
  
  
  
  # data dictionary must have valid type information 
  dict_data <- dict_data[which(dict_data$type!=""),]
  
  # find columns names from input labels
  time_col <- rownames(dict_data[which(dict_data$label_front%in%time_label), ])
  y_col <- rownames(dict_data[which(dict_data$label_front==y_label),])
  cluster_col <- rownames(dict_data[which(dict_data$label_front==cluster_label),])
  tag_cols <- rownames(dict_data[which(dict_data$label_front%in%tag_labels), ])
  trim_by_col <- rownames(dict_data[which(dict_data$label_front==trim_by_label), ])
  pctcut_num_cols <- rownames(dict_data[which(dict_data$label_front%in%pctcut_num_labels), ])
  filter_tag_cols <- rownames(dict_data[which(dict_data$label_front%in%filter_tag_labels & dict_data$unit=="tag01"), ])
  
  # trim data by time conditions
  data <- data %>% filter(data[,trim_by_col]>=as.numeric(trim_vec[1])*time_unit & data[,trim_by_col]<as.numeric(trim_vec[2])*time_unit ) %>% as.data.frame()
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
  
  plot_obj <- NULL
  try({
    plot_obj <- viz_alluvial(
      data = data,
      dict_data = dict_data,
      time_col = time_col, # x axis is time-related breaks
      time_unit = time_unit, # time unit in each break window
      time_breaks = time_breaks, # vector of time window breaks
      time_quantity = time_quantity, # quantify responce y in each time window by average or 1st record
      y_col = y_col, # responce y variable (only numeric)
      cluster_col = cluster_col,
      tag_cols = tag_cols # additional tag status
    )
  },TRUE)
    
  return(plot_obj)
  
}

