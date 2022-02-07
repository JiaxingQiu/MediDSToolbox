viz_death_star <- function(
  # data
  data = data_viz,
  dict_data = dict_viz,
  # death star
  time_unit,
  y_col, # response column name as heat in heat map
  sort_col, 
  align_col,
  cluster_col,
  group_by_col = NULL,
  tag_col = "None", # color red / newly added tags 
  offset_col = NULL, # pma_days
  default_tag_cols = c(),# default tags that always colored black in a death star plot
  scale= c("Raw","Percentile (2D)", "Percentile (1D)")[1]
){
  
  # --- required arguments ---
  # heat
  stopifnot(length(y_col)>0)
  data$measure <- as.numeric(as.character(data[,y_col]))
  y_label <- ifelse(dict_data[which(dict_data$varname==y_col),"label"]=="", y_col, dict_data[which(dict_data$varname==y_col),"label"])
  # sort
  stopifnot(length(sort_col)>0)
  data$sort <- as.numeric( as.character( data[,sort_col] ))
  # align
  stopifnot(length(align_col)>0)
  data$relative_time <- data[,align_col]
  # cluster
  stopifnot(length(cluster_col)>0)
  data$cluster <- data[,cluster_col]
    
  # --- optional arguments ---
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
  # offset col
  if (length(offset_col)==0){
    data$offset <- 0
  }else{
    data$offset <- data[,offset_col]
  }
  
  # max cluster population 1000, under sample if surpassed 
  if(n_distinct(data[,cluster_col])>1000){
    cluster_list <- sample(unique(data[,cluster_col]), size=1000, replace = FALSE)
    data <- data[which(data[,cluster_col]%in%cluster_list),]
  }
  
  # scale / transform the response
  if (scale=="Raw"){
    data$measure_final <- data$measure
    label(data$measure_final) <- y_label
  }else if(scale=='Percentile (2D)'){
    data$measure_pct <- est_pctl(data$measure)
    data$measure_final <- data$measure_pct 
    label(data$measure_final) <- paste0("Joint percentile of ", y_label)
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
    label(data$measure_final) <- paste0("Conditioned percentile of ", y_label)
  }
  
  # reset index
  new_idx_df <- data %>%
    group_by(cluster) %>%
    summarise(sort_len = max(sort), group=unique(group))%>%
    arrange(group,desc(sort_len)) %>% as.data.frame()
  new_idx_df$new_idx <- 1:nrow(new_idx_df)
  
  data <- merge(data, new_idx_df, by=c('cluster'), all.x=TRUE)
  
  # relative time is usually relative to calendar time, but if offset col is not null, set "negative" start point to a time series
  data$init_time <-  data$relative_time - data$offset
  
  plot_obj <- ggplot(data, aes(x=relative_time/time_unit, y=new_idx)) +
    geom_tile(aes(fill=measure_final)) + 
    scale_fill_gradientn(colours = topo.colors(30)) +
    geom_point(aes(x=init_time/time_unit),color='black',size=0.3, shape=1)+
    geom_point(data=data[which(rowSums(data[,default_tag_cols]==1)>0),c('relative_time','new_idx')],color='black',size=0.5, shape=4) +
    geom_point(data=data[which(data$mark==1),c('relative_time','new_idx')], color='red',size=0.5, shape=3) +
    xlab(paste0(ifelse(dict_data[which(dict_data$varname==align_col),"label"]=="", align_col,dict_data[which(dict_data$varname==align_col),"label"]), 
                " by ",
                ifelse(dict_data[which(dict_data$varname==align_col),"unit"]=="", align_col,dict_data[which(dict_data$varname==align_col),"unit"]),
                ifelse(time_unit>1, paste0("/",time_unit),"") ))+
    ylab('subject / cluster index')+
    labs(fill=y_label) +
    theme(legend.position = "top" ) 
  
  
}
