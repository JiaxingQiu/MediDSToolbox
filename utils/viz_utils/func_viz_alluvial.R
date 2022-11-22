viz_alluvial <- function(
  data = subset_df(data_viz,"40w"),
  dict_data = dict_viz,
  time_col = "pma_days", # x axis is time-related breaks
  time_unit = 7, # time unit in each break window
  time_breaks = c(28,36,40), # vector of time window breaks
  time_quantity = c("average", "1st record")[2], # quantify responce y in each time window by average or 1st record
  y_col = c("baby_weight",NULL)[1], # responce y variable (only numeric or null)
  cluster_col = c("subjectnbr")[1],
  tag_cols = c("dod___tag", "exit_date___tag", "posair_ynunk"), # additional tag status,
  includeNA=TRUE
){
  
  plot_obj <- NULL
  if (length(dict_data[which(dict_data$varname == y_col),"type"])==0 ){
    # create a fake base column with num type 
    y_col <- "base"
    data[,y_col] <- NA
    dict_data_new <- data.frame(varname = y_col, type="num")
    dict_data <- bind_rows(dict_data, dict_data_new)
  }
  if (dict_data[which(dict_data$varname == y_col),"type"]=="num"){ # if responce is numerical, responce will be broken to percentile groups
    # coerce dtype of responce(y)
    data$y <- as.numeric(data[,y_col])
    data$time <- as.numeric(data[,time_col])
    data$cluster <- as.character(data[,cluster_col])
    
    # --- status of interest ---
    # pctl [0,25), [25,50), [50,75), [75,90), [90,100]
    # additional tags
    # time_breaks=c(28,36,40)
    time_breaks = as.numeric(time_breaks)
    # refine time breaks 
    if(length(time_breaks)>0){
      time_breaks <- time_breaks[which(time_breaks*time_unit>=min(data[,time_col],na.rm=TRUE) & time_breaks*time_unit<=max(data[,time_col],na.rm=TRUE))]
    }
    if(length(time_breaks)==0){
      time_breaks <- floor(as.numeric(quantile(data[,time_col], seq(0,1,0.1)))/time_unit)
    }
    
    df_all <- data.frame()
    if (time_quantity == "1st record"){
      time_snapshots_start <- time_breaks*time_unit
      time_snapshots_stop <- time_breaks*time_unit
    }else if(time_quantity == "average"){
      time_snapshots_start <- time_breaks*time_unit
      time_snapshots_stop <- time_breaks*time_unit+time_unit
    }
    
    for(i in 1:length(time_breaks)){
      snap = time_snapshots_start[i]
      df <- data %>% filter(time>=time_snapshots_start[i]&time<=time_snapshots_stop[i]) %>% 
        select(cluster, y, time) %>%
        group_by(cluster) %>%
        summarise(y = mean(y,na.rm=TRUE)) %>%
        as.data.frame()
      cluster <- as.character( unique(data$cluster) )
      df_id <- data.frame(cluster, stringsAsFactors = FALSE)
      df <- merge(df_id, df, by=c('cluster'), all.x=TRUE)
      
      
      df$status <- "NA" 
      intervals <- c(0, 0.25, 0.5, 0.75, 0.90, 0.95, 1)
      scaler <- data.frame(q = quantile(df$y,probs =intervals, na.rm=TRUE))
      scaler$p <- as.numeric( gsub("%","",rownames(scaler)) )
      scaler <- scaler %>% group_by(q) %>% summarise(min_p=min(p, na.rm = TRUE))
      q <- scaler$q
      p <- as.integer( scaler$min_p )
      
      for(j in 2:length(q)){
        df$status[which(df$y>=q[j-1]&df$y<q[j])] <- paste0("[",round(p[j-1]),"th, ", round(p[j]), "th)")
        if (j==length(q)){
          df$status[which(df$y>=q[j-1]&df$y<=q[j])] <- paste0("[",round(p[j-1]),"th, ", round(p[j]), "th]")
        } 
      }
      
      new_tag_list <- c()
      for(tag_col in tag_cols){
        data$tag_col <- data[,tag_col]
        sbj_tag <- data %>% 
          group_by(cluster) %>% 
          filter(time>=time_snapshots_start[i]&time<=time_snapshots_stop[i]) %>%
          filter(any(tag_col==1)) %>%
          select(cluster) %>%
          as.data.frame()
        df$status[df$cluster %in% sbj_tag$cluster] <- as.character(dict_data[tag_col,"label"])
        new_tag_list <- unique(c(new_tag_list, as.character(dict_data[tag_col,"label"])))
      }
      
      df$status <- factor(df$status,
                          levels = unique( c("NA",
                                     new_tag_list,
                                    "[0th, 25th)","[0th, 50th)","[0th, 75th)","[0th, 90th)","[0th, 95th)","[0th, 100th]",
                                    "[25th, 50th)","[50th, 75th)","[75th, 90th)","[90th, 95th)","[95th, 100th]") )
      )
      df$time_group <- time_breaks[i]
      df <- df[,c('cluster','time_group','status')]
      df_all <- bind_rows(df_all, df)
    }
    df_all$status_label <- NA
    for (stts in unique(df_all$status)){
      df_all$status_label[which(df_all$time_group==min(as.numeric(df_all$time_group[which(as.character( df_all$status )==as.character(stts))]), na.rm=TRUE) & 
                                  df_all$status==stts)] <- stts
    }
    if(!includeNA){
      df_all <- df_all[which(!df_all$status%in%c("NA",NA)),]
    }
    plot_obj <- ggplot(df_all,
                       aes(x = time_group, stratum = status, alluvium = cluster,
                           fill = status, label = status)) +
      labs(x="Time", y="Number of Subjects")+
      geom_flow() +
      geom_stratum(alpha = .5) +
      geom_text(stat = 'stratum',
                aes(label=status_label),
                size=4,
                hjust="left") +
      #scale_x_discrete(expand = c(.1, .1)) +
      theme(legend.position = "none"#,
            #axis.title.x=element_blank()
            ) 
    
  }
  
  return(plot_obj)
  
}

