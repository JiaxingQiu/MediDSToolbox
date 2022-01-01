viz_1d_stats <- function(
  data=data_viz,
  dict_data=dict_viz,
  y_col="mean_hr",
  x_col="skewness_spo2",
  cluster_col="id",
  group_by_col="bc_res_mix"
){
  
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
  for ( g_level in intersect(unique(as.character(data$group)),c("Unknown","NA"))  ){
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
  
  
  data_all <- data
  data_all$group <- "All"
  data_mean <- dplyr::distinct( dplyr::bind_rows(data_all, data))
  data_mean$group <- factor(data_mean$group,levels = union('All', sort(unique(data$group))))
  # --- mean ---
  if(plot_mean){
    p_mean <- ggplot(data_mean, aes(x=x, y=y)) +
      stat_summary(geom = "line", fun = mean) +
      stat_summary(geom = "ribbon", fun.data = mean_cl_normal, alpha = 0.3)+
      xlab(paste0(dict_data[x_col,"label_front"], "    ", dict_data[x_col,"unit"]))+
      ylab(paste0(dict_data[y_col,"label_front"], "    ", dict_data[y_col,"unit"]))+
      #ylim(min(data$y,na.rm=TRUE)*0.9, max(as.numeric(quantile(data$y, 0.99,na.rm = TRUE)),na.rm=TRUE)) + 
      ggtitle("Mean") +
      facet_wrap(~ group, ncol = 3) 
    if(grepl("_days",x_col)){
      x_breaks <- seq(min(data[,x_col], na.rm=TRUE), max(data[,x_col], na.rm = TRUE), 7)
      x_labels <- seq(round(min(data[,x_col], na.rm=TRUE)/7), round(max(data[,x_col], na.rm = TRUE)/7),1)[1:length(x_breaks)]
      p_mean <- p_mean + scale_x_continuous(name=paste0(dict_data[x_col,"label_front"], "    (week)"),
                                            breaks = x_breaks,
                                            labels = x_labels)
    }
    
  }
  
  # --- violin ---
  if(plot_violin){
    p_violin <- ggplot(data_mean, aes(x=x, y=y, fill=group)) +
      geom_boxplot(color = "black", width = 0.2, position = position_dodge(width=0.25), size=0.1, alpha = 0.4) +
      geom_split_violin(trim=FALSE, size=0.3, alpha=0.5)+ 
      xlab(paste0(dict_data[x_col,"label_front"], "    ", dict_data[x_col,"unit"]))+
      ylab(paste0(dict_data[y_col,"label_front"], "    ", dict_data[y_col,"unit"]))+
      ylim(min(data$y,na.rm=TRUE)*0.9, max(as.numeric(quantile(data$y, 0.99,na.rm = TRUE)),na.rm=TRUE)) + 
      ggtitle("Violin + Box")+
      facet_wrap(~ group, ncol = 3)
    
  }
  
  # --- percentile ---
  intervals <- c(0.5, 0.25, 0.75, 0.90, 0.95)
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
      geom_smooth(aes(linetype=percentile, color=percentile),size=0.5, method = smooth_method, formula = smooth_formula) +
      scale_color_manual(values = c("black","red","black","black","black")) +
      scale_linetype_manual(values=c(5,1,2,3,4)) +
      xlab(paste0(dict_data[x_col,"label_front"], "    ", dict_data[x_col,"unit"]))+
      ylab(paste0(dict_data[y_col,"label_front"], "    ", dict_data[y_col,"unit"]))+
      ylim(min(df$q_val,na.rm=TRUE)*0.9, max(as.numeric(quantile(df$q_val, 0.99,na.rm = TRUE)),na.rm=TRUE)) + 
      ggtitle("Percentile") +
      facet_wrap(~ group, ncol = 3) 
    
    if(grepl("_days",x_col)){
      x_breaks <- seq(min(data[,x_col], na.rm=TRUE), max(data[,x_col], na.rm = TRUE), 7)
      x_labels <- seq(round(min(data[,x_col], na.rm=TRUE)/7), round(max(data[,x_col], na.rm = TRUE)/7),1)[1:length(x_breaks)]
      p_pct <- p_pct + scale_x_continuous(name=paste0(dict_data[x_col,"label_front"], "    (week)"),
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
      facet_wrap(~ group, ncol = 3) +
      xlab(paste0(dict_data[x_col,"label_front"], "     ", dict_data[x_col,"unit"]))+
      ylab('# sbj w/ available data') +
      ggtitle("Denominator")
    
    if(grepl("_days",x_col)){
      x_breaks <- seq(min(data[,x_col], na.rm=TRUE), max(data[,x_col], na.rm = TRUE), 7)
      x_labels <- seq(round(min(data[,x_col], na.rm=TRUE)/7), round(max(data[,x_col], na.rm = TRUE)/7),1)[1:length(x_breaks)]
      p_denom <- p_denom + scale_x_continuous(name=paste0(dict_data[x_col,"label_front"], "    (week)"),
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
                q90 = quantile(y, probs = 0.95, na.rm=TRUE), # quantiles
                avg = mean(y, na.rm=TRUE)) %>%  
      as.data.frame()
    df_summ_all$group <- 'Overall' 
    
    df_summ_grouped <- data %>% 
      group_by(group, x) %>% 
      summarise(n_sbj = n_distinct(cluster), # how many babies have the data
                n_sbj_avail = n_distinct(cluster[!is.na(y)]), # how many babies have responce data available
                q25 = quantile(y, probs = 0.25, na.rm=TRUE), # quantiles
                q50 = quantile(y, probs = 0.50, na.rm=TRUE), # quantiles
                q75 = quantile(y, probs = 0.75, na.rm=TRUE), # quantiles
                q90 = quantile(y, probs = 0.90, na.rm=TRUE), # quantiles
                q90 = quantile(y, probs = 0.95, na.rm=TRUE), # quantiles
                avg = mean(y, na.rm=TRUE)) %>% 
      as.data.frame()
    df_summ <- dplyr::distinct( dplyr::bind_rows(df_summ_all, df_summ_grouped) )
    colnames(df_summ) <- c(x_col,paste0(y_col,"__", c("n_sbj", "n_sbj_avail", "q25" ,  "q50",   "q75",   "q90",   "avg")), ifelse(length(group_by_col)==0, "Group", group_by_col) )
    df_summ <- df_summ[,union(c(x_col,ifelse(length(group_by_col)==0, "Group", group_by_col)),colnames(df_summ))]# change column sequence
  }, silent = FALSE)
  if(is.null(df_summ)){print("df_summ is null")}else{print(head(df_summ,10))}
  
  return(list(
    "p_denom" = p_denom,
    "p_violin" = p_violin,
    "p_pct" = p_pct,
    "p_mean" = p_mean,
    "df_summ" = df_summ
  ))
}
