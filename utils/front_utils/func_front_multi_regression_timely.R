front_multi_regression_timely <- function(
  data = subset_df(data_ml, "40w"),
  dict_data=dict_ml,
  trim_by_label="Post-menstrual Age",
  trim_vec = c(24, 38), # trim vec controls the beginning and the end of models
  time_unit=7,
  x_labels_linear=c("Gestational Age", "pH associated with highest CO2 on blood gas"),
  x_labels_nonlin_rcs5=c("Maternal age"),
  x_labels_nonlin_rcs4=c("Gestational Age"),
  x_labels_nonlin_rcs3=c("Birth weight"),
  x_labels_fct = c("Site (EN)"),
  x_labels_tag = c("Baby Gender (EN)___Female"),
  x_labels=unique(c(x_labels_linear,x_labels_nonlin_rcs5,x_labels_nonlin_rcs4,x_labels_nonlin_rcs3,x_labels_fct,x_labels_tag)), 
  y_label="Primary outcome (EN)___Unfavorable", 
  cluster_label="PreVent study ID",
  r2=0.9,
  rcs5_low="0%",
  rcs4_low="0%",
  cv_nfold=5, 
  na_frac_max=1, 
  test_data=NULL, 
  num_labels_linear=c("Gestational Age", "PeriodicBreathing_v2 log(duration proportion) per day"),
  num_col2_label="None",
  imputation="Zero",
  impute_per_cluster=FALSE,
  winsorizing=FALSE,
  aggregation=FALSE,
  stratified_cv=FALSE,
  r_abs=0.8, 
  type=c("pearson","spearman")[1],
  rank=TRUE,
  seed_value=333,
  window_size=2, # the length of each window
  step_size=2, # each beginning of the window increase by step_size
  test_size = 1,
  lag_size = 0,
  fix_knots=FALSE
){
  
  # find the trim by column
  trim_by_col <- rownames(dict_data[which(dict_data$label_front==trim_by_label), ])
  
  # prepare start window points for each model trim_vec
  min_trim_start <- max(floor( min(data[,dict_ml$varname[which(dict_ml$label_front==trim_by_label)]]/time_unit, na.rm=TRUE) ), trim_vec[1], na.rm = TRUE)
  max_trim_start <- min(floor( max(data[,dict_ml$varname[which(dict_ml$label_front==trim_by_label)]]/time_unit, na.rm=TRUE) ), trim_vec[2], na.rm = TRUE)
  trim_start_list <- seq(from=min_trim_start, to=max_trim_start, by=step_size)
  
  # observation frequency table 
  freq_tbl_all <- c()
  # model scores table
  score_tbl_all <- c()
  # predictor importance table
  anova_tbl_all <- c()
  # external test result table
  test_tbl_all <- c()
  
  # loop through all start points for trim_vec
  for (trim_start in trim_start_list){
    try({
      # trim_vec for current model
      trim_vec = c(trim_start, trim_start+window_size)
      
      # prepare test data
      if(any(c(test_size,lag_size)<=0)){
        test_data<- NULL
      }else{
        test_trim_vec = c(trim_start+window_size+lag_size, trim_start+window_size+lag_size+test_size)
        test_data <- data %>% filter(data[,trim_by_col]>=test_trim_vec[1]*time_unit & data[,trim_by_col]<test_trim_vec[2]*time_unit ) %>% as.data.frame()
      }
      
      # model report object
      MLreports <- front_multi_regression(data = data,
                                          dict_data = dict_data,
                                          trim_by_label=trim_by_label, 
                                          trim_vec=as.numeric(trim_vec), 
                                          x_labels_linear=x_labels_linear,
                                          x_labels_nonlin_rcs5=x_labels_nonlin_rcs5,
                                          x_labels_nonlin_rcs4=x_labels_nonlin_rcs4,
                                          x_labels_nonlin_rcs3=x_labels_nonlin_rcs3,
                                          x_labels_fct = x_labels_fct,
                                          x_labels_tag = x_labels_tag,
                                          x_labels=unique(c(x_labels_linear,x_labels_nonlin_rcs5,x_labels_nonlin_rcs4,x_labels_nonlin_rcs3,x_labels_fct,x_labels_tag)), 
                                          y_label=y_label, 
                                          cluster_label=cluster_label,
                                          rcs5_low=rcs5_low,
                                          rcs4_low=rcs4_low,
                                          num_labels_linear=num_labels_linear, 
                                          num_col2_label="None", 
                                          na_frac_max=na_frac_max, 
                                          test_data=test_data, 
                                          engineer_test_data=FALSE,
                                          imputation=imputation,
                                          winsorizing=winsorizing,
                                          aggregation = aggregation,
                                          stratified_cv = stratified_cv,
                                          cv_nfold = as.numeric(cv_nfold),
                                          time_unit=time_unit,
                                          impute_per_cluster=impute_per_cluster,
                                          r_abs=r_abs, 
                                          r2=r2,
                                          type=type,
                                          rank=rank,
                                          seed_value=seed_value,
                                          fix_knots = fix_knots) 
      
      
      # observation frequency
      freq_tbl <- as.data.frame(MLreports$mdl_obj$freq)
      colnames(freq_tbl) <- c(y_label,"freq")
      freq_tbl$window <- paste0("[", trim_vec[1], ", ", trim_vec[2],")")
      freq_tbl$rel_time <- trim_vec[1]
      freq_tbl_all <- as.data.frame(bind_rows(freq_tbl_all, freq_tbl))
      
      # performance
      score_tbl <- MLreports$score_tbl
      score_tbl$window <- paste0("[", trim_vec[1], ", ", trim_vec[2],")")
      score_tbl$rel_time <- trim_vec[1]
      score_tbl_all <- bind_rows(score_tbl_all, score_tbl)
      
      # inference 
      anova_tbl <- data.frame(plot(anova(MLreports$mdl_obj)))
      colnames(anova_tbl) <- c("chi_square")
      anova_tbl$varname <- rownames(anova_tbl)
      rownames(anova_tbl) <- 1:nrow(anova_tbl)
      anova_tbl$rel_time <- trim_vec[1]
      anova_tbl$window <- paste0("[", trim_vec[1], ", ", trim_vec[2],")")
      anova_tbl$p_val <- round(as.data.frame(anova(MLreports$mdl_obj))[anova_tbl$varname,"P"],4)
      anova_tbl_all <- bind_rows(anova_tbl_all, anova_tbl)
      
      # external test result table if any
      if(!is.null(MLreports$test_tbl)){
        tryCatch({
          test_tbl <- MLreports$test_tbl[2,] # second row
          test_tbl$window <- paste0("[", test_trim_vec[1], ", ", test_trim_vec[2],")")
          test_tbl$rel_time <- test_trim_vec[1]
          test_tbl_all <- bind_rows(test_tbl_all, test_tbl)
        },error=function(e){
          print(paste0("Error! Fail to test on data in ", paste0("[", test_trim_vec[1], ", ", test_trim_vec[2],")")))
          print(e)
        })
      }
    },TRUE)
    
  }
  
  # frequency over time plot 
  freq_plot <- ggplot(freq_tbl_all, aes(x=rel_time, y=freq, color=freq_tbl_all[,y_label]))+
    geom_point()+
    geom_line()+
    scale_x_continuous(breaks = freq_tbl_all$rel_time,label = freq_tbl_all$window) +
    theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=0.5),
          legend.position = "top",
          legend.title = element_text(size=10)) +
    labs(x=trim_by_label, y="# obs", color = y_label)
  
  
  # scores over time / windows
  df_plt <- c()
  for(score in c("AUROC","logloss")){#, "AUPRC"
    df_plt <- bind_rows(
      df_plt,
      data.frame(score_mean = score_tbl_all[,paste0("train_",score,"_mean")],
                 score_se = score_tbl_all[,paste0("train_",score,"_se")],
                 rel_time = score_tbl_all$rel_time,
                 window = score_tbl_all$window,
                 score_name = score,
                 model_group = "train"),
      data.frame(score_mean = score_tbl_all[,paste0("valid_",score,"_mean")],
                 score_se = score_tbl_all[,paste0("valid_",score,"_se")],
                 rel_time = score_tbl_all$rel_time,
                 window = score_tbl_all$window,
                 score_name = score,
                 model_group = "valid"))
    
  }
  score_plot <- ggplot(df_plt,aes(x=rel_time, y=score_mean, color=model_group))+
    geom_point()+
    geom_errorbar(aes(ymin=score_mean-score_se,ymax=score_mean+score_se), width=0.2) +
    geom_line() + 
    facet_wrap(~score_name, scales = "free_y", ncol=1)+
    scale_x_continuous(breaks = score_tbl_all$rel_time,label = score_tbl_all$window) +
    theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=0.5),
          legend.position = "top",
          legend.title = element_text(size=10)) +
    labs(x=trim_by_label, y=NULL, color = NULL) +
    scale_color_manual(values = c("valid" = "blue", "train" = "orange"))
  
  
  
  # # winsorize the anova tables 
  # try({ 
  #   anova_tbl_all[,c("chi_square","p_val")] <- winsorize(anova_tbl_all[,c("chi_square","p_val")])
  # },TRUE)
  anova_tbl_all$chi_square[which(anova_tbl_all$chi_square>=100)] <- 100
  
  infer_plot <- ggplot(anova_tbl_all, aes(x=rel_time,y=varname))+
    geom_tile(aes(fill=chi_square)) +
    labs(fill="ChiSquare-df",x=trim_by_label) + 
    theme(axis.title.y=element_blank(),
          axis.text.x = element_text(angle = 30, vjust = 1, hjust=1),
          legend.position = "top",
          legend.title = element_text(size=10)) + 
    scale_x_continuous(breaks = score_tbl_all$rel_time,label = score_tbl_all$window) + scale_fill_gradientn(colours = rev(rainbow(4)))+
    geom_text(aes(label=round(p_val,3),angle = 30))
  
  test_plot <- NULL
  if(!is.null(test_tbl_all)){
    test_plot <- ggplot(test_tbl_all, aes(x=rel_time, y=AUROC))+
      geom_point()+
      geom_line()+
      scale_x_continuous(breaks = test_tbl_all$rel_time,label = test_tbl_all$window) +
      theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=0.5)) +
      labs(x=trim_by_label, y="test_AUROC")
  }
  
  
  return(list("timely_freq_plot" = freq_plot,
              "timely_freq_table" = freq_tbl_all,
              "timely_score_plot" = score_plot,
              "timely_score_table" = score_tbl_all,
              "timely_infer_plot" = infer_plot,
              "timely_infer_table" = anova_tbl_all,
              "timely_test_plot" = test_plot,
              "timely_test_table" = test_tbl_all
              ))
  
}
