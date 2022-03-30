front_uni_heatmap <- function(
  data,
  dict_data,
  num_labels, 
  y_label, 
  cluster_label,
  # --- engineer ---
  trim_by_label=NULL,
  trim_vec = c(-Inf, Inf),
  time_unit=1,
  pctcut_num_labels=c(),
  pctcut_num_vec=c(0.1, 99.9),
  pctcut_num_coerce = TRUE,
  filter_tag_labels=c(),
  imputation="None",
  impute_per_cluster=FALSE,
  winsorizing=FALSE,
  aggregate_per=c("row", "cluster_trim_by_unit", "cluster")[1],
  # --- local ---
  trim_ctrl=TRUE,
  num_adjust_label=NULL, 
  method="logit_rcs", 
  pct=TRUE,
  y_map_func=c("fold_risk", "probability", "log_odds")[1],
  y_map_max=3,
  label_y=TRUE
){
  
  tryCatch({
    data <- assign.dict(data, dict_data, overwrite = TRUE)
    dict_data <- get.dict(data)
  },error=function(e){
    print("--- Skip refine dictionary from data ---")
    print(e)
  })
  
  # ---- translate front end labels to column names ----
  trim_by_col <- dict_data$varname[which(dict_data$label==trim_by_label)]
  num_cols <- intersect(dict_data$varname[which(dict_data$label%in%num_labels)], dict_data$varname[which(dict_data$mlrole=="input"&dict_data$type=="num")])
  y_col <- dict_data$varname[which(dict_data$label==y_label)]
  fct_cols <- c(y_col)
  cluster_col <- dict_data$varname[which(dict_data$label==cluster_label)]
  pctcut_num_cols <- dict_data$varname[which(dict_data$label%in%pctcut_num_labels)]
  filter_tag_cols <- dict_data$varname[which(dict_data$label%in%filter_tag_labels)]
  num_adjust_col <- dict_data$varname[which(dict_data$label==num_adjust_label)]
  num_cols <- union(num_cols, num_adjust_col)
  
  # ---- engineering ----
  if(trim_ctrl){
    data_in <- engineer(data = data,
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
  }else{
    if (all(unique(as.character(data[,y_col])) %in% c(1,0,NA))){
      data_event <- engineer(data = data[which(data[,y_col]==1),],
                             num_cols = num_cols,
                             fct_cols = fct_cols,
                             cluster_col = cluster_col,
                             trim_by_col = trim_by_col,
                             trim_min=trim_vec[1],
                             trim_max=trim_vec[2],
                             trim_step_size = time_unit,
                             pctcut_num_cols = pctcut_num_cols,
                             pctcut_num_vec = pctcut_num_vec,
                             pctcut_num_coerce = pctcut_num_coerce,
                             filter_tag_cols = filter_tag_cols,
                             imputation = imputation,
                             impute_per_cluster = impute_per_cluster,
                             winsorizing = winsorizing,
                             aggregate_per = aggregate_per)
      data_cntrl <- engineer(data = data[which(data[,y_col]==0),],
                             num_cols = num_cols,
                             fct_cols = fct_cols,
                             cluster_col = cluster_col,
                             trim_by_col = trim_by_col,
                             trim_min=-Inf,
                             trim_max=Inf,
                             trim_keepna = TRUE,
                             trim_step_size = time_unit,
                             pctcut_num_cols = pctcut_num_cols,
                             pctcut_num_vec = pctcut_num_vec,
                             pctcut_num_coerce = pctcut_num_coerce,
                             filter_tag_cols = filter_tag_cols,
                             imputation = imputation,
                             impute_per_cluster = impute_per_cluster,
                             winsorizing = winsorizing,
                             aggregate_per = aggregate_per)
      data_in <- bind_rows(data_cntrl, data_event)
    }
  }
  data <- assign.dict(data_in, dict_data, overwrite = TRUE)
  
  # ---- run back end functions ----
  if(method=='Kernel Density Estimates'){
    df_result_all <- uni_kde_nums(data, num_cols, pct=pct)
    plot_obj <- ggplot(df_result_all, aes(x=pctl,y=var_name))+
      geom_tile(aes(fill=density_scaled)) +
      labs(fill="KDE",x="Percentile") + 
      theme(axis.title.y=element_blank()) + 
      scale_fill_gradientn(colours = terrain.colors(10))
  }else{
    df_result_all <- uni_tag_nums(data, 
                                  num_cols, 
                                  y_col, 
                                  cluster_col, 
                                  num_adjust_col, 
                                  method=method, 
                                  pct=pct, 
                                  y_map_func=y_map_func, 
                                  y_map_max=y_map_max)
    if ("c_score" %in% colnames(df_result_all)) {
      var_order <- df_result_all %>% group_by(var_name) %>% summarise(max_c=max(c_score)) %>% arrange(max_c) %>% as.data.frame()
      df_result_all$c_label <- NA
      for (var in unique(df_result_all$var_name) ){
        c <- round(unique(df_result_all$c_score[which(df_result_all$var_name==var)]),4)
        df_result_all$c_label[which(df_result_all$var_name==var)][1] <- paste0("C = ", c)
      }
    }else {
      var_order <- df_result_all %>% group_by(var_name) %>% summarise(max_prob=max(yhat)) %>% arrange(max_prob) %>% as.data.frame()
    }
    df_result_all_sort <- dplyr::left_join(var_order,df_result_all)
    # add raw data quantile back 
    df_result_all_sort$raw_value <- NA
    for(var in unique(df_result_all_sort$var_name)){
      df_result_all_sort$raw_value[which(df_result_all_sort$var_name==var)]<-as.numeric( quantile(data[,var],df_result_all_sort$pctl[which(df_result_all_sort$var_name==var)], na.rm=TRUE) )
    }
    
    plot_obj <- ggplot(df_result_all_sort, aes(x=pctl,y=var_name))+
      geom_tile(aes(fill=yhat)) +
      labs(fill=y_map_func,x="Percentile") + 
      theme(axis.title.y=element_blank()) + 
      scale_fill_gradientn(colours = rev(rainbow(7))) +
      scale_y_discrete(limits=var_order$var_name)
    if(label_y){
      tryCatch({
        dict_data <- get.dict(assign.dict(data,dict_data))
        label_list <- dict_data[var_order$var_name,"label"]
        label_list <- gsub("[^[:alnum:]]+"," ",label_list)
        label_list <- stringr::str_wrap(label_list, width=20)
        plot_obj <- plot_obj + scale_y_discrete(limits=var_order$var_name,labels=label_list)
      },error=function(e){
        print('Failed to use labels for y axis in uni heatmap')
        print(e)
      })
    }
    if("c_score" %in% colnames(df_result_all_sort)){
      plot_obj <- plot_obj + geom_text(aes(label=c_label), hjust="left", na.rm = TRUE, check_overlap = TRUE)
    }
  }
  
  
  return(list(plot_obj = plot_obj,
              df_result_all_sort = df_result_all_sort))
  
}

# ############################################ not run #############################################
# data = data_ml
# dict_data = dict_ml
# # --- setup ---
# trim_by_label = "Post-menstrual Age"
# trim_vec = c(22, 40)
# time_unit= 7
# trim_ctrl=FALSE
# imputation="None"
# impute_per_cluster=FALSE
# winsorizing=TRUE
# aggregate_per=c("row", "cluster_trim_by_unit", "cluster")[2]
# pctcut_num_labels=c()
# pctcut_num_vec=c(0.1, 99.9)
# pctcut_num_coerce = TRUE
# filter_tag_labels=c("On respiratory support with endotracheal tube (EN) == Yes", "Any  Doses of any medication today")
# # --- local ---
# num_labels = dict_data$label[which(dict_data$type=="num")][c(1:20)]
# y_label = "Primary outcome (EN) == Unfavorable"
# cluster_label = "PreVent study ID"
# num_adjust_label = NULL
# method="logit_rcs"
# pct = TRUE
# y_map_func=c("fold_risk", "probability", "log_odds")[1]
# y_map_max=3
