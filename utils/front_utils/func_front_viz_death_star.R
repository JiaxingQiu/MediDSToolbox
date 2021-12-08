front_viz_death_star <- function(
  # data
  data = subset_df(data_viz, "40w"),
  dict_data = dict_viz,
  # setup
  time_unit = 7,
  trim_by_label = "Post-menstrual Age",
  trim_vec = c(22, 40),
  pctcut_num_labels = c("Birth weight"), # cutoff by percentile of one or more numeric variable
  pctcut_num_vec = c(0.1, 99.9),
  coerce = TRUE,
  filter_tag_labels = c("Site (EN)___Alabama",
                        "On respiratory support without endotracheal tube  (EN)___Yes"), # tag columns
  # death star
  # user control
  y_label = "ABD_v3 duration proportion per day", # num or fct(tag) y / response variable
  sort_by_label = "Post-menstrual Age", 
  align_by_label = "Chronological Age", 
  cluster_label = "PreVent study ID",
  group_by_label = "None", #"None" # fct
  tag_label = "None",
  scale = c("Raw","Percentile (2D)", "Percentile (1D)")[1],
  # developer control
  offset_label = "Post-menstrual Age", 
  na_label = "Post-menstrual Age", # set any value outside the range of this column NA
  na_vec = c(22, 40), 
  default_tag_labels = c("Date of birth tag", "Date of death tag")
){
  
  # data dictionary must have valid type information 
  dict_data <- dict_data[which(dict_data$type!=""),]
  
  # find columns names from input labels
  trim_by_col <- rownames(dict_data[which(dict_data$label_front==trim_by_label), ])
  pctcut_num_cols <- rownames(dict_data[which(dict_data$label_front%in%pctcut_num_labels), ])
  filter_tag_cols <- rownames(dict_data[which(dict_data$label_front%in%filter_tag_labels & dict_data$unit=="tag01"), ])
  
  y_col <- rownames(dict_data[which(dict_data$label_front==y_label),])
  sort_col <- rownames(dict_data[which(dict_data$label_front==sort_by_label),])
  align_col <- rownames(dict_data[which(dict_data$label_front==align_by_label),])
  cluster_col <- rownames(dict_data[which(dict_data$label_front==cluster_label),])
  group_by_col <- rownames(dict_data[which(dict_data$label_front==group_by_label),])
  tag_col <- rownames(dict_data[which(dict_data$label_front==tag_label),])
  offset_col <- rownames(dict_data[which(dict_data$label_front==offset_label),])
  na_col <- rownames(dict_data[which(dict_data$label_front==na_label),])
  default_tag_cols <- rownames(dict_data[which(dict_data$label_front%in%default_tag_labels & dict_data$unit=="tag01"), ])
  
  na_vec <- as.numeric(na_vec)
  trim_vec <- as.numeric(trim_vec)
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
  plot_obj <- viz_death_star(
    data = data,
    dict_data = dict_data,
    time_unit=time_unit,
    y_col=y_col,
    sort_col=sort_col, 
    align_col=align_col,
    cluster_col=cluster_col,
    group_by_col = group_by_col,
    tag_col = tag_col, 
    offset_col = offset_col,
    na_col=na_col,
    na_vec=na_vec,
    default_tag_cols= default_tag_cols,# default tags that always colored black in a death star plot
    scale=scale)
  
  

  
  return(plot_obj)
}
