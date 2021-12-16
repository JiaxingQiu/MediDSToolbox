front_viz_2d_stats <- function(
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
  y_label = "IH ( Desat<80 [10s, 300s] ) duration(sec) per day", # num or fct(tag) y / response variable
  x_label1 ="Post-menstrual Age", # num 
  x_label2 = "Gestational Age" # num 
  
){
  # ---- Usage ----
  
   
  # ---- Arguments ----
  
  
  # ---- Value ----
  
  
  # data dictionary must have valid type information 
  dict_data <- dict_data[which(dict_data$type!=""),]
  
  # find columns names from input labels
  x_col1 <- rownames(dict_data[which(dict_data$label_front%in%x_label1), ])
  x_col2 <- rownames(dict_data[which(dict_data$label_front%in%x_label2), ])
  y_col <- rownames(dict_data[which(dict_data$label_front==y_label),])
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
  
  # --- 2d heatmap plots ----
  plot_2d_stats <- NULL
  if(dict_data[x_col1,"type"]=="num" & dict_data[x_col2,"type"]=="num"){
    plot_2d_stats <- viz_2d_stats(data=data,
                                  dict_data = dict_data,
                                  x_col1 = x_col1,
                                  x_col2 = x_col2,
                                  y_col = y_col)
  }
  
  
  
  return(plot_2d_stats)
}
