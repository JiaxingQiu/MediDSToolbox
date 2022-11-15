front_viz_0d_stats <- function(
    data,
    dict_data,
    y_label, # num or fct(tag) y / response variable
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
  
  # find columns names from input labels
  y_col <- dict_data$varname[which(dict_data$label==y_label)]
  stopifnot(length(y_col)<=1)
  cluster_col <- dict_data$varname[which(dict_data$label==cluster_label)]
  stopifnot(length(cluster_col)<=1)
  group_by_col <- dict_data$varname[which(dict_data$label==group_by_label)]
  stopifnot(length(group_by_col)<=1)
  num_cols <- intersect( dict_data$varname[which(dict_data$type=="num")], c(y_col, group_by_col) )
  fct_cols <- intersect( dict_data$varname[which(dict_data$type=="fct")], c(y_col, group_by_col))
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
  
  # ---- response distribution plots ----
  data$cluster <- as.character(data[,cluster_col])
  if(length(group_by_col)==0){
    data$group <- "All"
  }else{
    data$group <- as.character(data[,group_by_col])
  }
  for ( g_level in intersect(unique(as.character(data$group)),c("NA","Unknown"))  ){ #
    data$group[which(as.character(data$group)==g_level)] <- NA
  }
  
  plot_0d_stats_obj <- NULL
  # if y_col is numeric
  if(dict_data$type[which(dict_data$label==y_label)]=="num"){
    data$y <- as.numeric(as.character( data[,y_col] ))
    # histogram by Freedman-Diaconis rule
    q1 <- as.numeric(quantile(data[,y_col], 0.25, na.rm = TRUE))
    q3 <- as.numeric(quantile(data[,y_col], 0.75, na.rm = TRUE))
    iqr <- q3 - q1
    bin_width <- (2*iqr)/(sum(!is.na(data[,y_col]))^(1/3))
    bins <- min(100,ceiling((max(data[,y_col],na.rm=TRUE) - min(data[,y_col],na.rm=TRUE))/bin_width))
    data$bin <- cut(data$y, bins)
    height <- data %>% group_by(bin)%>%summarise(ny = sum(!is.na(y))) %>% select(ny) %>% max(na.rm = TRUE)
    hist_plot <- ggplot(data = data, aes(x=y, fill=group)) + 
      geom_histogram(position="dodge", bins = bins) + 
      geom_boxplot(position ="dodge2", notch=TRUE, varwidth =TRUE, width=height/15) + 
      labs(x=y_label, fill=NULL, title="Histogram") + 
      scale_fill_brewer(palette = "Pastel1", na.value=NA) +
      scale_colour_brewer(palette = "Set1", na.value=NA) +
      theme_bw()
    
    # density plot
    dens_plot <- ggplot(data=data, aes(x=y, color=group, fill=group)) + 
      geom_histogram(aes(y=..density..), fill="white", position="dodge", bins = bins)+
      geom_density(alpha=.2) + 
      labs(x=y_label, fill=NULL, color=NULL, title="Density") +
      scale_fill_brewer(palette = "Pastel1", na.value=NA) +
      scale_colour_brewer(palette = "Set1", na.value=NA) +
      theme_bw()
    
    plot_0d_stats_obj <- ggarrange(hist_plot, dens_plot, ncol=2, nrow=1, common.legend = TRUE, legend = "top")
  }
  # if y_col is factor
  if(dict_data$type[which(dict_data$label==y_label)]=="fct"){
    data$y <- as.factor(gsub("_level__","",as.character( data[,y_col] )))
    plot_0d_stats_obj <- ggplot(data = data, aes(x=y, fill=group)) + 
      geom_histogram(stat="count",position="dodge") + 
      labs(x=y_label, fill=NULL, title="Histogram") + 
      scale_fill_brewer(palette = "Pastel1", na.value=NA) +
      theme_bw()
  }
  
  
  
  return(plot_0d_stats_obj)
  
  
  
}