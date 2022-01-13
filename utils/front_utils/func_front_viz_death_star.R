front_viz_death_star <- function(
  # data
  data,
  dict_data,
  # setup
  trim_by_label,
  time_unit = 1,
  trim_vec = c(-Inf, Inf),
  pctcut_num_labels = c(), # cutoff by percentile of one or more numeric variable
  pctcut_num_vec = c(0.1, 99.9),
  pctcut_num_coerce = TRUE,
  filter_tag_labels = c(),
  cluster_label = "PreVent study ID", # tag columns
  # death star
  # user control
  y_label = "IH (SPO2<85%) >=50s event counts per day", # num or fct(tag) y / response variable
  sort_by_label = "Post-menstrual Age", 
  align_by_label = "Chronological Age", 
  group_by_label = "None", #"None" # fct
  tag_label = "None",
  scale = c("Raw","Percentile (2D)", "Percentile (1D)")[1],
  # developer control
  offset_label = "Post-menstrual Age", 
  default_tag_labels = c("Date of birth tag", "Date of death tag")
){
  
  # warning global engineering doesn't affect death star plot because this is plot of individual trajectrories
  # data dictionary must have valid type information 
  dict_data <- dict_data[which(dict_data$type!=""),]
  
  # find columns names from input labels
  trim_by_col <- dict_data$varname[which(dict_data$label==trim_by_label)]
  pctcut_num_cols <-dict_data$varname[which(dict_data$label%in%pctcut_num_labels)]
  filter_tag_cols <-dict_data$varname[which(dict_data$label%in%filter_tag_labels & dict_data$unit=="tag01")]
  
  y_col <-dict_data$varname[which(dict_data$label==y_label)]
  sort_col <-dict_data$varname[which(dict_data$label==sort_by_label)]
  align_col <-dict_data$varname[which(dict_data$label==align_by_label)]
  cluster_col <-dict_data$varname[which(dict_data$label==cluster_label)]
  group_by_col <-dict_data$varname[which(dict_data$label==group_by_label)]
  tag_col <-dict_data$varname[which(dict_data$label==tag_label)]
  offset_col <-dict_data$varname[which(dict_data$label==offset_label)]
  default_tag_cols <-dict_data$varname[which(dict_data$label%in%default_tag_labels & dict_data$unit=="tag01")]
  
  trim_vec <- as.numeric(trim_vec)
  # trim data by time conditions
  data <- data %>% filter(data[,trim_by_col]>=as.numeric(trim_vec[1])*time_unit & data[,trim_by_col]<as.numeric(trim_vec[2])*time_unit ) %>% as.data.frame()
  # cutoff data by percentile of a numeric variable
  for(pctcut_num_col in pctcut_num_cols){
    quantiles <- quantile( data[,pctcut_num_col], c(as.numeric(pctcut_num_vec[1])/100, as.numeric(pctcut_num_vec[2])/100 ), na.rm =TRUE)
    if(pctcut_num_coerce){
      # if pctcut_num_coerce extremum
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
    default_tag_cols= default_tag_cols,# default tags that always colored black in a death star plot
    scale=scale)
  
  return(plot_obj)
}


# ############################# not run ###############################
# # data
# data = data_ml
# dict_data = dict_ml
# # setup
# trim_by_label = "Post-menstrual Age"
# # setup
# time_unit = 1
# trim_vec = c(-Inf, Inf)
# pctcut_num_labels = c() # cutoff by percentile of one or more numeric variable
# pctcut_num_vec = c(0.1, 99.9)
# pctcut_num_coerce = TRUE
# filter_tag_labels = c()
# cluster_label = "PreVent study ID" # tag columns
# # death star
# # user control
# y_label = "IH (SPO2<85%) >=50s event counts per day" # num or fct(tag) y / response variable
# sort_by_label = "Post-menstrual Age"
# align_by_label = "Chronological Age"
# group_by_label = "None" #"None" # fct
# tag_label = "None"
# scale = c("Raw","Percentile (2D)", "Percentile (1D)")[1]
# # developer control
# offset_label = "Post-menstrual Age"
# default_tag_labels = c("Date of birth tag", "Date of death tag")
