viz_1d_stats <- function(
  data,
  dict_data,
  y_col,
  x_col,
  cluster_col,
  group_by_col=NULL,
  label_peak=FALSE,
  span=0.5
){
  ### format y axis digits
  scaleFUN <- function(x) sprintf("%4.0f", x)
  
  # visualize distribution information given any value of x 
  # x_col can be num / fct / tag
  # y_col can be num / tag
  
  # --- prepare the data frame ---
  data$y <- as.numeric(as.character( data[,y_col] )) # y can only be num or tag
  data$cluster <- as.character(data[,cluster_col])
  if(length(group_by_col)==0){
    data$group <- "All"
  }else{
    data$group <- as.character(data[,group_by_col])
  }
  for ( g_level in intersect(unique(as.character(data$group)),c("NA","Unknown"))  ){ #
    data$group[which(as.character(data$group)==g_level)] <- NA
  }
  data$x <- as.character(data[,x_col])
  
  if (dict_data[x_col,"type"]=="num"){
    data$x <- as.numeric(data[,x_col])
    if(n_distinct(data$x)>10000){
      x_cut <- stringr::str_split_fixed(gsub("]","",gsub("[(]","",as.character(cut(data$x,1000)))),",",2)[,1]
      data$x <- as.numeric(x_cut)
    }
  }
  
  # --- gates ---
  if(dict_data[x_col,"type"]=="fct"){
    plot_violin <- TRUE
    plot_mean <- FALSE
    plot_pct <- FALSE
    plot_denom <- TRUE
  }
  if(dict_data[x_col,"type"]=="num" & dict_data[y_col,"type"]=="fct"){
    plot_violin <- FALSE
    plot_mean <- TRUE
    plot_pct <- FALSE
    plot_denom <- TRUE
  }
  if(dict_data[x_col,"type"]=="num" & dict_data[y_col,"type"]=="num"){
    plot_violin <- FALSE
    plot_mean <- TRUE
    plot_pct <- TRUE
    plot_denom <- TRUE
  }
  
  
  
  # initiate plots
  p_violin <- NULL
  p_mean <- NULL
  p_pct <- NULL
  p_denom <- NULL
  p_1stat_set <- NULL
  p_1stat_set_raw <- NULL
  
  data_all <- data
  data_all$group <- "All"
  data_mean <- dplyr::distinct( dplyr::bind_rows(data_all, data))
  data_mean$group <- factor(data_mean$group,levels = union('All', sort(unique(data$group))))
  # --- mean ---
  if(plot_mean){
    p_mean <- ggplot(data_mean, aes(x=x, y=y)) +
      stat_summary(geom = "line", fun = mean) +
      stat_summary(geom = "ribbon", fun.data = mean_cl_normal, alpha = 0.3)+
      xlab(paste0(dict_data[x_col,"label"], "    ", dict_data[x_col,"unit"]))+
      ylab(paste0(dict_data[y_col,"label"], "    ", dict_data[y_col,"unit"]))+
      #ylim(min(data$y,na.rm=TRUE)*0.9, max(as.numeric(quantile(data$y, 0.99,na.rm = TRUE)),na.rm=TRUE)) + 
      ggtitle("Mean") +
      facet_wrap(~ group, ncol = 5) 
    if(grepl("_days",x_col)){
      x_breaks <- seq(min(data[,x_col], na.rm=TRUE), max(data[,x_col], na.rm = TRUE), 14)
      x_labels <- floor(x_breaks/7)
      p_mean <- p_mean + scale_x_continuous(name=paste0(dict_data[x_col,"label"], "    (week)"),
                                            breaks = x_breaks,
                                            labels = x_labels)
    }
    
  }
  
  # --- violin ---
  if(plot_violin){
    p_violin <- ggplot(data_mean, aes(x=x, y=y, fill=group)) +
      geom_boxplot(color = "black", width = 0.2, position = position_dodge(width=0.25), size=0.1, alpha = 0.4) +
      geom_split_violin(trim=FALSE, size=0.3, alpha=0.5)+ 
      xlab(paste0(dict_data[x_col,"label"], "    ", dict_data[x_col,"unit"]))+
      ylab(paste0(dict_data[y_col,"label"], "    ", dict_data[y_col,"unit"]))+
      #ylim(min(data$y,na.rm=TRUE)*0.9, max(as.numeric(quantile(data$y, 0.99,na.rm = TRUE)),na.rm=TRUE)) + 
      ggtitle("Violin + Box")+
      facet_wrap(~ group, ncol = 5)
    
  }
  
  # --- percentile ---
  intervals <- c(0.5, 0.25, 0.75, 0.90)#, 0.95
  df_all <- data %>% 
    group_by(x) %>% 
    summarise(n_sbj = n_distinct(cluster), # how many babies have the data
              q_val = quantile(y, probs = intervals, na.rm=TRUE), # quantiles
              percentile = as.factor(intervals)) %>%  
    as.data.frame()
  df_all$group <- 'All' 
  
  df_grouped <- data %>% 
    group_by(group, x) %>% 
    summarise(n_sbj = n_distinct(cluster),
              q_val = quantile(y, probs = intervals, na.rm=TRUE),
              percentile = as.factor(intervals)) %>% 
    as.data.frame()
  
  df <- dplyr::distinct( dplyr::bind_rows(df_all, df_grouped) )
  df$group <- factor(df$group,levels = union('All', sort(unique(data$group))))
  if(plot_pct){
    # tuning smoothing method and formula based on data size
    smooth_method <- NULL
    smooth_formula <- NULL
    if(nrow(df_all)<5e+03) {
      smooth_method <- "loess"
    }
    if(nrow(df_all)>=5e+06) {
      smooth_method <- "glm"
      smooth_formula <- "y ~ poly(x, 7)"
    }
    
    p_pct <- ggplot(df[!is.na(df$group),], aes(x=x, y=q_val)) +
      geom_line(aes(linetype=percentile),size=0.5, color='grey') +
      geom_smooth(aes(linetype=percentile, color=percentile),
                  size=0.5, 
                  method = smooth_method, 
                  formula = smooth_formula,
                  span=span) +
      scale_color_manual(values = c("black","red","black","black","black")) +
      scale_linetype_manual(values=c(5,1,2,3,4)) +
      xlab(paste0(dict_data[x_col,"label"], "    ", dict_data[x_col,"unit"]))+
      ylab(paste0(dict_data[y_col,"label"], "    ", dict_data[y_col,"unit"]))+
      ylim(min(df$q_val,na.rm=TRUE)*0.9, max(as.numeric(quantile(df$q_val,0.99,na.rm = TRUE)),na.rm=TRUE)) + 
      ggtitle("Percentile") +
      facet_wrap(~ group, ncol = 5) + 
      scale_y_continuous(labels=scaleFUN)
    
    if(grepl("_days",x_col)){
      x_breaks <- seq(min(data[,x_col], na.rm=TRUE), max(data[,x_col], na.rm = TRUE), 14)
      x_labels <- floor(x_breaks/7)
      p_pct <- p_pct + scale_x_continuous(name=paste0(dict_data[x_col,"label"], "    (week)"),
                                            breaks = x_breaks,
                                            labels = x_labels) 
    }
  }
  
  # --- denominator ---
  df_all_denom <- data %>% 
    group_by(x) %>% 
    summarise(n_sbj = n_distinct(cluster[!is.na(y)])) %>% 
    as.data.frame()
  df_all_denom$group="All"
  df_grouped_denom <- data %>% 
    group_by(group, x) %>% 
    summarise(n_sbj = n_distinct(cluster[!is.na(y)])) %>% 
    as.data.frame()
  df_denom <- dplyr::distinct( dplyr::bind_rows(df_all_denom, df_grouped_denom) )
  df_denom$group <- factor(df_denom$group,levels = union('All',sort(unique(data$group)))) 
  
  if(plot_denom) {
    p_denom <- ggplot(df_denom[!is.na(df_denom$group),], aes(x=x, y=n_sbj)) +
      geom_bar(stat="identity") + 
      facet_wrap(~ group, ncol = 5) +
      xlab(paste0(dict_data[x_col,"label"], "     ", dict_data[x_col,"unit"]))+
      ylab('# sbj w/ available data') +
      ggtitle("Denominator") + 
      scale_y_continuous(labels=scaleFUN)
    
    if(grepl("_days",x_col)){
      x_breaks <- seq(min(data[,x_col], na.rm=TRUE), max(data[,x_col], na.rm = TRUE), 14)
      x_labels <- floor(x_breaks/7)
      p_denom <- p_denom + 
        scale_x_continuous(name=paste0(dict_data[x_col,"label"], "    (week)"),
                                              breaks = x_breaks,
                                              labels = x_labels) 
    }
  }
  
  
  # make a summary table to return for user to download
  df_summ <- NULL
  try({
    df_summ_all <- data %>% 
      group_by(x) %>% 
      summarise(n_sbj = n_distinct(cluster), # how many babies have the data,
                n_sbj_avail = n_distinct(cluster[!is.na(y)]), # how many babies have responce data available
                q25 = quantile(y, probs = 0.25, na.rm=TRUE), # quantiles
                q50 = quantile(y, probs = 0.50, na.rm=TRUE), # quantiles
                q75 = quantile(y, probs = 0.75, na.rm=TRUE), # quantiles
                q90 = quantile(y, probs = 0.90, na.rm=TRUE), # quantiles
                q95 = quantile(y, probs = 0.95, na.rm=TRUE), # quantiles
                avg = mean(y, na.rm=TRUE)) %>%  
      as.data.frame()
    df_summ_all$group <- 'Overall' 
    
    df_summ_grouped <- data %>% 
      group_by(group, x) %>% 
      summarise(n_sbj = n_distinct(cluster), # how many babies have the data
                n_sbj_avail = n_distinct(cluster[!is.na(y)]), # how many babies have response data available
                q25 = quantile(y, probs = 0.25, na.rm=TRUE), # quantiles
                q50 = quantile(y, probs = 0.50, na.rm=TRUE), # quantiles
                q75 = quantile(y, probs = 0.75, na.rm=TRUE), # quantiles
                q90 = quantile(y, probs = 0.90, na.rm=TRUE), # quantiles
                q95 = quantile(y, probs = 0.95, na.rm=TRUE), # quantiles
                avg = mean(y, na.rm=TRUE)) %>% 
      as.data.frame()
    df_summ <- dplyr::distinct( dplyr::bind_rows(df_summ_all, df_summ_grouped) )
    colnames(df_summ) <- c(x_col,paste0(y_col,"__", c("n_sbj", "n_sbj_avail", "q25" ,  "q50",   "q75",   "q90", "q95",  "avg")), ifelse(length(group_by_col)==0, "Group", group_by_col) )
    df_summ <- df_summ[,union(c(x_col,ifelse(length(group_by_col)==0, "Group", group_by_col)),colnames(df_summ))]# change column sequence
  }, silent = FALSE)
  if(is.null(df_summ)){print("df_summ is null")}else{print(head(df_summ,10))}
  
  # statistics
  tryCatch({
    p_1stat_set <- list()
    y_lab_string <- y_col
    try({y_lab_string <- paste0(dict_data[y_col,"label"], "    ", dict_data[y_col,"unit"]) },TRUE)
    x_lab_string <- x_col
    try({x_lab_string <- paste0(dict_data[x_col,"label"], "    ", dict_data[x_col,"unit"]) },TRUE)
    group_by_string <- group_by_col
    i=0
    for (stt in c( "avg", "q25" , "q50", "q75", "q90", "q95")){
      # format title string for the plot from statistic name
      title_string <- NULL
      if ( stt %in% c("q25", "q75", "q90", "q95")){
        title_string <- gsub("q","",stt)
        title_string <- paste0(title_string, "% percentile")
      } else if(stt %in% c("avg")) {
        title_string <- "Mean"
      } else if (stt %in% c("q50")) {
        title_string <- "Median (50% percentile)"
      }
      i=i+1
      df_summ_grouped_plot <- df_summ_grouped[,c("group", "x", stt, "n_sbj_avail")]
      colnames(df_summ_grouped_plot) <- c("group", "x", "stt_value", "n_sbj_avail")
      
      # smooth modeling
      df_summ_grouped_plot$group_text <- NA
      df_summ_grouped_plot$stt_value_smoothed <- NA
      for(g in unique(as.character(df_summ_grouped_plot$group))){
        idx1 <- as.character(df_summ_grouped_plot$group)==g
        mdl <- loess("stt_value~x", data=df_summ_grouped_plot[which(idx1),], span = span)
        df_summ_grouped_plot$stt_value_smoothed[which(idx1)] <- predict(mdl, df_summ_grouped_plot[which(idx1),])
        y_loc <- max(df_summ_grouped_plot$stt_value_smoothed[which(idx1)],na.rm=TRUE)
        idx2 <- df_summ_grouped_plot$stt_value_smoothed==y_loc
        x_loc <- df_summ_grouped_plot$x[which(idx1&idx2)][1]
        idx3 <- df_summ_grouped_plot$x==x_loc
        df_summ_grouped_plot$group_text[which(idx1&idx2&idx3)] <- g
      }
      if(label_peak){
        p_1stat <- ggplot(df_summ_grouped_plot, aes(x=x, y=stt_value_smoothed, color=group)) + 
          geom_line(size=1)+
          geom_text(aes(label=group_text),size=5, color="black")+
          theme_bw() +
          ylab(y_lab_string) +
          xlab(x_lab_string) +
          scale_color_discrete(name=group_by_string) +
          ggtitle(title_string) + 
          scale_y_continuous(labels=scaleFUN)
      }else{
        p_1stat <- ggplot(df_summ_grouped_plot, aes(x=x, y=stt_value_smoothed, color=group)) + 
          # geom_smooth(se = FALSE,span=span) + #se=TRUE, fill="#F5F5F5"
          geom_line(size=1)+
          theme_bw() +
          ylab(y_lab_string) +
          xlab(x_lab_string) +
          scale_color_discrete(name=group_by_string) +
          ggtitle(title_string) + 
          scale_y_continuous(labels=scaleFUN)
      }
      
      p_1stat_denom <- ggplot(df_summ_grouped_plot, aes(x=x, y=n_sbj_avail, fill=group))+
        geom_bar(stat="identity", position="stack") +
        theme_bw() +
        ylab("Number of subjects") +
        xlab(x_lab_string) +
        scale_fill_discrete(name=group_by_string) + 
        ggtitle("Data Availability") + 
        scale_y_continuous(labels=scaleFUN)
      if(grepl("_days",x_col)){
        x_breaks <- seq(min(data[,x_col], na.rm=TRUE), max(data[,x_col], na.rm = TRUE), 14)
        x_labels <- floor(x_breaks/7)
        p_1stat <- p_1stat + scale_x_continuous(name=paste0(dict_data[x_col,"label"], "    (week)"),
                                                breaks = x_breaks,
                                                labels = x_labels)
        p_1stat_denom <- p_1stat_denom + scale_x_continuous(name=paste0(dict_data[x_col,"label"], "    (week)"),
                                                            breaks = x_breaks,
                                                            labels = x_labels)
      }
      p_1stat_pctall <- NULL
      if(!is.null(p_pct)){
        p_1stat_pctall <- p_pct +
          theme_bw() +
          ylab(y_lab_string) +
          xlab(x_lab_string) +
          theme(legend.position = "top") +
          ggtitle(NULL) + 
          scale_y_continuous(labels=scaleFUN)
        p_1stat_pctall$data <- p_1stat_pctall$data %>% filter(group =="All")
        if(grepl("_days",x_col)){
          p_1stat_pctall <- p_1stat_pctall + scale_x_continuous(name=paste0(dict_data[x_col,"label"], "    (week)"),
                                                                breaks = x_breaks,
                                                                labels = x_labels)
        }
        # coerce ylim of numerator and pct all
        # p_1stat <- p_1stat +
        #   ylim(ggplot2::ggplot_build(p_1stat_pctall)$layout$panel_scales_y[[1]]$range$range) +
        #   xlim(ggplot2::ggplot_build(p_1stat_pctall)$layout$panel_scales_x[[1]]$range$range)
      }
      
      # return set of figures by pieces
      p_1stat_set[[i]] <- list(pctall = p_1stat_pctall,
                               numer = p_1stat, 
                               denom = p_1stat_denom)
    }
    names(p_1stat_set) <- c("avg", "q25" , "q50", "q75", "q90", "q95")
  },error=function(e){
    print("skip 1d stat plot")
    print(e)
  })
  
  return(list(
    "p_denom" = p_denom,
    "p_violin" = p_violin,
    "p_pct" = p_pct,
    "p_mean" = p_mean,
    "df_summ" = df_summ,
    "p_1stat_set" = p_1stat_set
  ))
}
