front_viz_1d_stats <- function(
  # data
  data = subset_df(data_viz, "40w"),
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
  # distributions
  y_label = "IH (SPO2<90%) >=10s event counts per day", # num or fct(tag) y / response variable
  x_label ="Post-menstrual Age", # num or fct
  cluster_label = "Subject ID",
  group_by_label = "GA weeks" # fct
  
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
  # uni_eda object : plot_1d_stats
  
  # data dictionary must have valid type information 
  dict_data <- dict_data[which(dict_data$type!=""),]
  
  # find columns names from input labels
  x_col <- rownames(dict_data[which(dict_data$label_front%in%x_label), ])
  stopifnot(length(x_col)<=1)
  y_col <- rownames(dict_data[which(dict_data$label_front==y_label),])
  stopifnot(length(y_col)<=1)
  cluster_col <- rownames(dict_data[which(dict_data$label_front==cluster_label),])
  stopifnot(length(cluster_col)<=1)
  group_by_col <- rownames(dict_data[which(dict_data$label_front==group_by_label),])
  stopifnot(length(group_by_col)<=1)
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
  
  # --- conditional distribution plots ----
  plot_1d_stats <- NULL
  try({
    plot_1d_stats <- viz_1d_stats(data=data,
                                  dict_data = dict_data,
                                  x_col = x_col,
                                  y_col = y_col,
                                  cluster_col = cluster_col,
                                  group_by_col = group_by_col)
    
  },TRUE)
  print(plot_1d_stats)
  
  return(plot_1d_stats)
}
