viz_death_star <- function(
  # data
  data,
  dict_data,
  # death star
  time_unit,
  y_col, # response column name as heat in heat map
  sort_col, 
  align_col,
  cluster_col,
  group_by_col = NULL,
  tag_col = NULL, # color red / newly added tags 
  offset_col = NULL, # pma_days
  default_tag_cols = c(),# default tags that always colored black in a death star plot
  scale= c("Raw","Percentile (2D)", "Percentile (1D)")[1]
){
  
  ### format y axis digits
  scaleFUN <- function(x) sprintf("%1.0f", x)
  
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
    # # check group is unique per sbj
    # if(!as.logical( dict_data[which(dict_data$varname==group_by_col),"unique_per_sbj"] ) ){
    #   data$group <- 0
    # }else{
    data$group <- data[,group_by_col]
    # }
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
  if(n_distinct(data[,cluster_col])>2000){
    cluster_list <- sample(unique(data[,cluster_col]), size=2000, replace = FALSE)
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
  data <- merge(data, new_idx_df, by=intersect(colnames(new_idx_df),colnames(data)), all.x=TRUE)
  
  
  # add grouping text labels 
  data$group_text <- NA
  for(g in unique(as.character(data$group)) ){
    idx1 <- data$group==g
    y_loc <- round(min(data$new_idx[which(idx1)],na.rm=TRUE))#round(mean(data$new_idx[which(idx1)],na.rm=TRUE))
    idx2 <- data$new_idx==y_loc
    x_loc <- round(max(data$relative_time[which(idx1&idx2)],na.rm=TRUE))
    idx3 <- data$relative_time==x_loc
    data$group_text[which(idx1&idx2&idx3)] <- g
  }
  # relative time is usually relative to calendar time, but if offset col is not null, set "negative" start point to a time series
  # data$init_time <-  data$relative_time - data$offset # causing troubles 
  
  
  plot_obj <- ggplot(data, aes(x=relative_time/time_unit, y=new_idx)) +
    geom_tile(aes(fill=measure_final)) + 
    scale_fill_gradientn(colours = topo.colors(30),labels=scaleFUN, na.value =NA) +
    xlab(paste0(ifelse(dict_data[which(dict_data$varname==align_col),"label"]=="", align_col,dict_data[which(dict_data$varname==align_col),"label"]), 
                " by ",
                ifelse(dict_data[which(dict_data$varname==align_col),"unit"]=="", align_col,dict_data[which(dict_data$varname==align_col),"unit"]),
                ifelse(time_unit>1, paste0("/",time_unit),"") ))+
    ylab('subject / cluster index')+
    labs(fill=y_label) +
    theme(legend.position = "top" ) 
  
  
  # add group text info
  p_star <- plot_obj
  p_star$data$new_idx <- -(p_star$data$new_idx)
  y_breaks <- p_star$data$new_idx[which(!is.na(p_star$data$group_text))]
  y_labels <- p_star$data$group_text[which(!is.na(p_star$data$group_text))]
  p_star$data$group_text <- NA
  colorbreaks <- seq(from=floor(min(p_star$data$measure_final, na.rm=TRUE)),
                     to=ceiling(max(p_star$data$measure_final, na.rm=TRUE)),
                     length.out=5)
  p_star <- p_star + 
    #geom_point(aes(x=init_time/time_unit),color='black',size=0.3, shape=1)+
    geom_point(data=p_star$data[which(rowSums(p_star$data[,default_tag_cols]==1)>0),c('relative_time','new_idx')],color='red',size=0.1, shape=4) +
    #geom_point(data=p_star$data[which(p_star$data$mark==1),c('relative_time','new_idx')], color='black', size=0.1, shape=19) +
    geom_tile(data=p_star$data[which(p_star$data$mark==1),c('relative_time','new_idx','mark')],
              aes(x=relative_time/time_unit, y=new_idx, fill=mark),fill="black")+
    #geom_text(aes(label=group_text), hjust=-0.5, vjust = 0, size=5) + 
    scale_x_continuous(expand = c(0, 0))+
    scale_y_continuous(expand = c(0, 0), breaks=y_breaks, labels = stringr::str_wrap(y_labels, width=10))+
    #scale_fill_gradientn(colours = topo.colors(30), na.value = "black") +
    scale_fill_gradientn(
      limit = c(colorbreaks[1],
                colorbreaks[length(colorbreaks)]),
      values=scales::rescale(c(
      as.numeric(min(p_star$data$measure_final, na.rm=TRUE)),
      as.numeric(quantile(p_star$data$measure_final,0.25,na.rm = TRUE)),
      as.numeric(quantile(p_star$data$measure_final,0.5,na.rm = TRUE)),
      as.numeric(quantile(p_star$data$measure_final,0.70,na.rm = TRUE)),
      as.numeric(quantile(p_star$data$measure_final,0.80,na.rm = TRUE)),
      as.numeric(quantile(p_star$data$measure_final,0.90,na.rm = TRUE)),
      as.numeric(max(p_star$data$measure_final, na.rm=TRUE)))),
      breaks = colorbreaks,
      colours = c("#4C00FF",
                  "#0019FF", 
                  "#00A2FF", 
                  "#00FF2B", 
                  "#C3FF00", 
                  "#FFDC63", 
                  "#FFDB8B"),
      na.value = NA) +
    theme_dark(base_size = 16) +
    theme(axis.text.y = element_text(face="bold", colour="black", size=12),
          axis.title.y = element_blank(),
          axis.text.x = element_text(face="bold", colour="black", size=12),
          axis.title.x = element_text(face="bold", colour="black", size=14),
          legend.text = element_text(face="bold", colour="black", size=12),
          legend.title = element_text(face="bold", colour="black", size=14),
          legend.position = "bottom",
          panel.grid =  element_blank(),
          legend.key.height = unit(0.3,'cm'),
          legend.key.width = unit(1.2,'cm')) 
  
  
  plot_obj <- p_star
  return(plot_obj)
}
