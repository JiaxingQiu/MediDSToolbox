front_viz_death_star <- function(
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
  # death star
  y_label = "IH ( Desat<80 [10s, 300s] ) duration(sec) per day", # num or fct(tag) y / response variable
  group_by_label = "Site (EN)",#"None" # fct
  tag_label = "None", 
  sort_by_label = "Post-menstrual Age", 
  align_by_label = "Chronological Age", 
  scale= c("Raw","Percentile (2D)", "Percentile (1D)")[1]
){
  
  # data dictionary must have valid type information 
  dict_data <- dict_data[which(dict_data$type!=""),]
  
  # find columns names from input labels
  trim_by_col <- rownames(dict_data[which(dict_data$label_front==trim_by_label), ])
  pctcut_num_cols <- rownames(dict_data[which(dict_data$label_front%in%pctcut_num_labels), ])
  filter_tag_cols <- rownames(dict_data[which(dict_data$label_front%in%filter_tag_labels & dict_data$unit=="tag01"), ])
  
  y_col <- rownames(dict_data[which(dict_data$label_front==y_label),])
  group_by_col <- rownames(dict_data[which(dict_data$label_front==group_by_label),])
  tag_col <- rownames(dict_data[which(dict_data$label_front==tag_label),])
  sort_col <- rownames(dict_data[which(dict_data$label_front==sort_by_label),])
  align_col <- rownames(dict_data[which(dict_data$label_front==align_by_label),])
  
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
  
  # heat
  data$measure <- as.numeric(as.character(data[,y_col]))
  # group
  if (length(group_by_col)==0) {
    data$group <- 0
  } else {
    # check group is unique per sbj
    if(!as.logical( dict_data[group_by_col,"unique_per_sbj"] ) ){
      data$group <- 0
    }else{
      data$group <- data[,group_by_col]
    }
  }
  # tag / mark
  if (length(tag_col)==0) {
    data$mark <- 0
  } else {
    data$mark <- data[,tag_col]
  }
  # sort
  data$sort <- as.numeric( as.character( data[,sort_col] ))
  # align
  data$relative_time <- data[,align_col]
  data$dop <-  data$relative_time - data$pma_days
 
  if (scale=="Raw"){
    data$measure_final <- data$measure
    label(data$measure_final) <- y_label
  }else if(scale=='Percentile (2D)'){
    data$measure_pct <- est_pctl(data$measure)
    data$measure_final <- data$measure_pct 
    label(data$measure_final) <- paste0("joint percentile of ", y_label)
  }else if(scale=='Percentile (1D)'){
    data$measure_scaled  <- NA
    data$measure_pct  <- NA
    for (t in unique(data$relative_time)){
      idx = data$relative_time==t # conditioned on time
      data$measure_scaled[which(idx)] <- (data$measure[which(idx)]-min(data$measure[which(idx)],na.rm=TRUE))/(max(data$measure[which(idx)],na.rm=TRUE)-min(data$measure[which(idx)],na.rm=TRUE))
      cuts <- seq(0,1,0.05)
      scaler <- data.frame(qt=as.vector(quantile(data$measure_scaled[which(idx)],cuts,na.rm=TRUE)),pt=cuts)
      scaler <- scaler %>% group_by(round(qt,4)) %>% summarise(pt=mean(pt)) %>% as.data.frame()
      colnames(scaler) <- c('qt','pt')
      for (i in 2:nrow(scaler)){
        data[which(idx & data$measure_scaled>=scaler$qt[i-1] & data$measure_scaled<scaler$qt[i]),'measure_pct']<-scaler$pt[i-1]
      }
    }
    data$measure_final <- data$measure_pct 
    label(data$measure_final) <- paste0("Age-grouped percentile of ", y_label)
  }
  
  # reset index
  new_idx_df <- data %>%
    group_by(subjectnbr) %>%
    summarise(sort_len = max(sort), group=unique(group))%>%
    arrange(group,desc(sort_len)) %>% as.data.frame()
  new_idx_df$new_idx <- 1:nrow(new_idx_df)
  
  data <- merge(data, new_idx_df, by=c('subjectnbr'), all.x=TRUE)

  
  data[which(data$pma_days>time_unit*40), "measure_final"] <- NA
  
  plot_obj <- ggplot(data, aes(x=relative_time/time_unit, y=new_idx)) +
    geom_tile(aes(fill=measure_final)) + 
    geom_point(aes(x=dop/time_unit),color='black',size=0.3, shape=1)+
    scale_fill_gradientn(colours = topo.colors(30)) +
    geom_point(data=data[which(data$baby_dob___tag==1),c('relative_time','new_idx')],color='black',size=0.3, shape=2) +
    geom_point(data=data[which(data$dod___tag==1),c('relative_time','new_idx')],color='black',size=1.3, shape=4) +
    geom_point(data=data[which(data$mark==1),c('relative_time','new_idx')], color='red',size=0.5, shape=3) +
    xlab(paste0('week relative to Day 0 of ',align_by_label))+
    ylab('subject index')+
    labs(fill=y_label) +
    theme(legend.position = "top" ) 
    

  
  return(plot_obj)
}
