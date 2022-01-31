viz_2d_stats<- function(
  data,
  dict_data,
  x_col1,
  x_col2,
  y_col,
  cluster_col,
  group_by_col
){
  
  data_org <- data
  # max cluster population 800, under sample if surpassed 
  if(n_distinct(data_org[,cluster_col])>800){
    cluster_list <- sample(unique(data_org[,cluster_col]), size=800, replace = FALSE)
    data_org <- data_org[which(data_org[,cluster_col]%in%cluster_list),]
    print("under sample population to be 800")
  }
  if(length(group_by_col)>0){
    df_plot_all <- data.frame()
    for (g in unique(data_org[,group_by_col])){
      data <- data_org[which(data_org[,group_by_col]==g),]
      if(all(as.logical( dict_data[c(x_col1, x_col2, y_col), "unique_per_sbj"] )) ){
        data <- dplyr::distinct(data[,c(x_col1, x_col2, y_col)])
      }
      fml <- formula(paste0("as.numeric(as.character(",y_col,")) ~ ",paste0("as.numeric(",x_col1,")+as.numeric(",x_col2,")")))
      mdl <- loess(fml, data, degree = 2, span=0.2)
      dd <- merge(data.frame(x_col1=seq(min(as.numeric(data[,x_col1]),na.rm=TRUE),max(as.numeric(data[,x_col1]),na.rm=TRUE),1)), 
                  data.frame(x_col2=seq(min(as.numeric(data[,x_col2]),na.rm=TRUE),max(as.numeric(data[,x_col2]),na.rm=TRUE),1)))
      colnames(dd) <- c(x_col1, x_col2)
      df_plot <- cbind(data.frame( y_fit = as.numeric(predict(mdl, dd)) ),dd)
      df_plot[,group_by_col] <- g
      df_plot_all <- bind_rows(df_plot_all, df_plot)
    }
  }
  
  
  p_raw <- ggplot(data=df_plot_all, aes(x=df_plot_all[,x_col1], y=df_plot_all[,x_col2]))+
    geom_tile(aes(fill=y_fit)) + 
    facet_wrap(~df_plot_all[,group_by_col]) +
    scale_fill_gradientn(colours = topo.colors(10)) +
    xlab(paste0(dict_data[x_col1,"label"], "    ", dict_data[x_col1,"unit"]))+
    ylab(paste0(dict_data[x_col2,"label"], "    ", dict_data[x_col2,"unit"]))+
    labs(fill=paste0("Estimated ",dict_data[y_col,"label"], "    ", dict_data[y_col,"unit"]))+
    theme(legend.position = "top" ) 
  
  p_pct <- ggplot(data=df_plot_all, aes(x=df_plot_all[,x_col1], y=df_plot_all[,x_col2]))+
    geom_tile(aes(fill=est_pctl(y_fit) )) + 
    facet_wrap(~df_plot_all[,group_by_col]) +
    scale_fill_gradientn(colours = topo.colors(10)) +
    xlab(paste0(dict_data[x_col1,"label"], "    ", dict_data[x_col1,"unit"]))+
    ylab(paste0(dict_data[x_col2,"label"], "    ", dict_data[x_col2,"unit"]))+
    labs(fill=paste0("Percentile of estimated ",dict_data[y_col,"label"]))+
    theme(legend.position = "top" ) 
  
   
  
  # ---- special cases ----
  
  if (x_col1=="pma_days" & x_col2=="ga_days"){
    df_plot_all$y_fit[which(df_plot_all[,x_col1]<df_plot_all[,x_col2])] <- NA
    p_raw <- ggplot(data=df_plot_all, aes(x=df_plot_all[,x_col1], y=df_plot_all[,x_col2]))+
      geom_tile(aes(fill=y_fit)) + 
      scale_fill_gradientn(colours = topo.colors(10)) +
      facet_wrap(~df_plot_all[,group_by_col]) +
      xlab(paste0(dict_data[x_col1,"label"], "    ", dict_data[x_col1,"unit"]))+
      ylab(paste0(dict_data[x_col2,"label"], "    ", dict_data[x_col2,"unit"]))+
      labs(fill=paste0("Estimated ",dict_data[y_col,"label"], "    ", dict_data[y_col,"unit"]))+
      theme(legend.position = "top" ) 
    
    p_pct <- ggplot(data=df_plot_all, aes(x=df_plot_all[,x_col1], y=df_plot_all[,x_col2]))+
      geom_tile(aes(fill=est_pctl(y_fit) )) + 
      scale_fill_gradientn(colours = topo.colors(10)) +
      facet_wrap(~df_plot_all[,group_by_col]) +
      xlab(paste0(dict_data[x_col1,"label"], "    ", dict_data[x_col1,"unit"]))+
      ylab(paste0(dict_data[x_col2,"label"], "    ", dict_data[x_col2,"unit"]))+
      labs(fill=paste0("Percentile of estimated ",dict_data[y_col,"label"]))+
      theme(legend.position = "top" ) 
  }
  if (grepl("_days", x_col1)) {
    x_breaks <- seq(min(df_plot_all[,x_col1], na.rm=TRUE), max(df_plot_all[,x_col1], na.rm = TRUE),7)
    x_labels <- seq(round(min(df_plot_all[,x_col1], na.rm=TRUE)/7), round(max(df_plot_all[,x_col1], na.rm = TRUE)/7),1)[1:length(x_breaks)]
    p_raw <- p_raw + scale_x_continuous(name=paste0(dict_data[x_col1,"label"], "    (week)"),
                                        breaks = x_breaks,
                                        labels = x_labels)
    p_pct <- p_pct + scale_x_continuous(name=paste0(dict_data[x_col1,"label"], "    (week)"),
                                        breaks = x_breaks,
                                        labels = x_labels)
  }
  
  if (grepl("_days", x_col2)) {
    y_breaks <- seq(min(df_plot_all[,x_col2], na.rm=TRUE), max(df_plot_all[,x_col2], na.rm = TRUE),7)
    y_labels <- seq(round(min(df_plot_all[,x_col2], na.rm=TRUE)/7), round(max(df_plot_all[,x_col2], na.rm = TRUE)/7),1)[1:length(y_breaks)]
    p_raw <- p_raw + scale_y_continuous(name=paste0(dict_data[x_col2,"label"], "    (week)"),
                                        breaks = y_breaks,
                                        labels = y_labels)
    p_pct <- p_pct + scale_y_continuous(name=paste0(dict_data[x_col2,"label"], "    (week)"),
                                        breaks = y_breaks,
                                        labels = y_labels)
  }
  
  
  # ---- plot result object to return ----
  plot_obj <- ggpubr::ggarrange(p_raw,
                                p_pct, 
                                ncol = 1)
 
  return(plot_obj)
}
