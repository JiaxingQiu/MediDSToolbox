
# within group sum of squares
wssplot <- function(df_mdl, nc=15, seed=333, plot_wss = FALSE){
  
  df_mdl <- df_mdl[complete.cases(df_mdl),]
  wss <- (nrow(df_mdl)-1)*sum(apply(df_mdl,2,stats::var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(df_mdl, centers=i, iter.max = 30)$withinss)}
  
  if (plot_wss){
    par(mfrow=c(2,2))
    plot(1:nc, wss, type="b", xlab="Number of Clusters",
         ylab="Within groups sum of squares")
    plot(1:nc, log(wss), type="b", xlab="Number of Clusters",
         ylab="log of Within groups sum of squares")
    plot(2:nc, diff(wss), type="b", xlab="Number of Clusters",
         ylab="diff( Within groups sum of squares )")
    plot(2:nc, diff(log(wss)), type="b", xlab="Number of Clusters",
         ylab="diff( log of Within groups sum of squares )")
    par(mfrow=c(1,1))
  }
  return(wss)
}

# 
cluster_heat <- function(data = data_ml,
                         dict_data = dict_ml,
                         dim_cols = colnames(data_ml%>%select(starts_with("cpd_"))) # dimentional columns to cluster rows
){
  
  # keep complete cases only
  df_mdl <- data[which(complete.cases(data[,dim_cols])),dim_cols]
  
  # save wss object
  wss <- wssplot(df_mdl=df_mdl)
  # find the biggest drop in wss
  n_centers <- c(2:15)[which(diff(wss)==min(diff(wss)) )]
  # train model on all df_mdl
  km_mdl <- kmeans(df_mdl[,setdiff(colnames(df_mdl),c("subjectnbr", "ts_utc"))], centers = n_centers)
  # plot clusters in pca space
  km_plot_obj <- ggplot2::autoplot(km_mdl, df_mdl, frame = TRUE)
  
  # get the frequency table of clusters in a data.frame
  df_km_mdl_cluster <- as.data.frame(table(km_mdl$cluster))
  colnames(df_km_mdl_cluster) <- c("cluster", "freq")
  df_km_mdl_cluster <- df_km_mdl_cluster[order(df_km_mdl_cluster$freq),]
  rownames(df_km_mdl_cluster) <- nrow(df_km_mdl_cluster):1
  df_km_mdl_cluster$cluster_sort <- rownames(df_km_mdl_cluster)
  
  
  # create result labeled daily record data.frame
  df_res <- km_plot_obj$df_mdl 
  df_res <- df_res %>% select(contains("cpd"), subjectnbr, ts_utc, cluster, PC1, PC2) %>% as.data.frame()
  # add frequency of clusters in a column
  df_res <- merge(df_res, df_km_mdl_cluster, all.x = TRUE)
  # add cluster id combind by cluster and frequency
  df_res$cluster_id <- paste0(df_res$cluster_sort," (",df_res$freq,")")
  cluster_plot_obj <- ggplot(df_res, aes(x=PC1, y=PC2, color=cluster_id)) + 
    geom_point(size=1.5) + 
    labs(x = km_plot_obj$labels$x,
         y = km_plot_obj$labels$y)+
    theme(legend.position = "top",
          legend.title = element_blank()) + 
    xlim(0,0.5) + 
    ylim(-0.5,0.5)
  
  # create centroid dataframe from the model object
  df_centroid_all <- data.frame()
  # loop through each cluster centroid ---- the imaginary or real location representing the center of the cluster. 
  for (i in rownames(km_mdl$centers) ){
    df_centroid <- as.data.frame(t(as.data.frame(km_mdl$centers)[i,]))
    colnames(df_centroid) <- c("event_counts")
    df_centroid$spo2 <- as.numeric(stringr::str_split_fixed(rownames(df_centroid), "_", 5)[,5])
    df_centroid$durs <- as.numeric(stringr::str_split_fixed(rownames(df_centroid), "_", 5)[,3])
    df_centroid$cluster_id <- paste0("cluster_",df_km_mdl_cluster$cluster_sort[which(df_km_mdl_cluster$cluster==i)],"_obs_",df_km_mdl_cluster$freq[which(df_km_mdl_cluster$cluster==i)])
    rownames(df_centroid) <- 1:nrow(df_centroid)
    # combine to global data.frame
    df_centroid_all <- bind_rows(df_centroid_all, df_centroid)
  }
  
  
  # ---- build the heatmap ----
  df_centroid_all$event_counts[which(df_centroid_all$event_counts >= 500)] <- 500
  color_values <- c(0, 5, 10, 50, 100, 150, 200, 250, 500)
  color_list <- c("white","light blue","blue","green","yellow","orange","red","dark red","brown","dark brown")[1:length(color_values)]
  
  centroid_plot_obj <- ggplot(data=df_centroid_all, aes(x=durs, y=spo2))+
    geom_tile(aes(fill=event_counts)) + 
    scale_fill_gradientn(
      colours = color_list,
      values = (color_values-min(color_values))/(max(color_values)-min(color_values)),
      breaks = color_values,
      labels = c(color_values[1:(length(color_values)-1)],paste0(">= ",color_values[length(color_values)]) ),
      limits = c(min(color_values), max(color_values)),
      na.value = NA) +
    xlab("Duration of event (second)")+
    ylab("Spo2 % threshold of event")+
    labs(fill="event counts per day")+
    theme(legend.position = "top",
          legend.key.width = unit(2,"cm")) +
    facet_wrap(~ cluster_id, ncol = 3) +
    scale_y_continuous(breaks = c(75,80,85,90,95),labels = c(75,80,85,90,95))+
    scale_x_continuous(breaks = c(0,25,50,75,100),labels = c(0,25,50,75,">=100"))
  
  plot_obj <- ggpubr::ggarrange( cluster_plot_obj, centroid_plot_obj, nrow=1, widths = c(0.5,1))
  
  return(plot_obj)
}
