front_summary_tbl <- function(
  data, # original dataset
  dict_data, # dictionary of dataset
  cluster_label, # key / cluster column label
  # --- engineer ---
  trim_by_label= NULL, # relative time trimming column label
  trim_vec=c(-Inf, Inf), 
  time_unit=1,
  pctcut_num_labels=c(),
  pctcut_num_vec=c(0.1, 99.9),
  pctcut_num_coerce = TRUE,
  filter_tag_labels=c(),
  imputation=c("None","Mean", "Median", "Zero")[1],
  impute_per_cluster=FALSE,
  winsorizing=FALSE,
  aggregation=TRUE,
  # --- local ---
  trim_ctrl=TRUE,
  stratify_by=c("None")[1]
){
  
  res_df <- NULL
  num_detail_df <- NULL
  fct_detail_df <- NULL
  rsps_df <- NULL
  na_obj <- NULL
  summ_df_reformat <- NULL
  
  tryCatch({
    data <- assign.dict(data, dict_data, overwrite = TRUE)
    dict_data <- get.dict(data)
  },error=function(e){
    print("--- Skip refine dictionary from data ---")
    print(e)
  })
  
  # ---- translate front end labels to column names ----
  trim_by_col <- intersect(colnames(data),dict_data$varname[which(dict_data$label==trim_by_label)])
  num_cols <- intersect(colnames(data), dict_data$varname[which(dict_data$type=="num")] )
  fct_cols <- intersect(colnames(data), dict_data$varname[which(dict_data$type=="fct"&dict_data$unit!="tag01")] )
  tag_cols <- intersect(colnames(data), dict_data$varname[which(dict_data$type=="fct"&dict_data$unit=="tag01")] )
  fct_cols <- union(fct_cols, tag_cols)
  cluster_col <- intersect(colnames(data), dict_data$varname[which(dict_data$label==cluster_label)] )
  stratify_col <- intersect(colnames(data), dict_data$varname[which(dict_data$label==stratify_by)] )
  if(length(stratify_col)>0){
    if (dict_data[stratify_col,"type"]=="num"){
      num_cols <- union(num_cols, stratify_col)
    }else if (dict_data[stratify_col,"type"]=="fct"){
      fct_cols <- union(fct_cols, stratify_col)
    }
  }
  pctcut_num_cols <- intersect(colnames(data), dict_data$varname[which(dict_data$label %in% pctcut_num_labels)] )
  filter_tag_cols <- intersect(colnames(data), dict_data$varname[which(dict_data$label %in% filter_tag_labels)] )
    
  # ---- prepare engineered training and validation dataset (internal data) ----
  if(trim_ctrl){
    data <- engineer(data = data,
                     num_cols = num_cols,
                     fct_cols = fct_cols,
                     cluster_col = cluster_col,
                     trim_by_col = trim_by_col,
                     trim_min = trim_vec[1]*time_unit,
                     trim_max = trim_vec[2]*time_unit,
                     pctcut_num_cols = pctcut_num_cols,
                     pctcut_num_vec = pctcut_num_vec,
                     pctcut_num_coerce = pctcut_num_coerce,
                     filter_tag_cols = filter_tag_cols,
                     imputation = imputation,
                     impute_per_cluster = impute_per_cluster,
                     winsorizing = winsorizing,
                     aggregation = aggregation)
  }else{
    tryCatch({
      stopifnot(length(stratify_col)>0)
      if (all(unique(as.character(data[,stratify_col])) %in% c(1,0,NA))){
      data_event <- engineer(data = data[which(data[,stratify_col]==1),],
                             num_cols = num_cols,
                             fct_cols = fct_cols,
                             cluster_col = cluster_col,
                             trim_by_col = trim_by_col,
                             trim_min=trim_vec[1]*time_unit,
                             trim_max=trim_vec[2]*time_unit,
                             pctcut_num_cols = pctcut_num_cols,
                             pctcut_num_vec = pctcut_num_vec,
                             pctcut_num_coerce = pctcut_num_coerce,
                             filter_tag_cols = filter_tag_cols,
                             imputation = imputation,
                             impute_per_cluster = impute_per_cluster,
                             winsorizing = winsorizing,
                             aggregation = aggregation)
      data_cntrl <- engineer(data = data[which(data[,stratify_col]==0),],
                             num_cols = num_cols,
                             fct_cols = fct_cols,
                             cluster_col = cluster_col,
                             trim_by_col = trim_by_col,
                             trim_min=-Inf,
                             trim_max=Inf,
                             trim_keepna = TRUE,
                             pctcut_num_cols = pctcut_num_cols,
                             pctcut_num_vec = pctcut_num_vec,
                             pctcut_num_coerce = pctcut_num_coerce,
                             filter_tag_cols = filter_tag_cols,
                             imputation = imputation,
                             impute_per_cluster = impute_per_cluster,
                             winsorizing = winsorizing,
                             aggregation = aggregation)
      data <- bind_rows(data_cntrl, data_event)
      }
    },error=function(e){
      print("--- error engineer data with trim_ctrl == FALSE ---")
      print(e)
      print("--- set trim_ctrl = TRUE instead --- ")
      data <- engineer(data = data,
                       num_cols = num_cols,
                       fct_cols = fct_cols,
                       cluster_col = cluster_col,
                       trim_by_col = trim_by_col,
                       trim_min = trim_vec[1]*time_unit,
                       trim_max = trim_vec[2]*time_unit,
                       pctcut_num_cols = pctcut_num_cols,
                       pctcut_num_vec = pctcut_num_vec,
                       pctcut_num_coerce = pctcut_num_coerce,
                       filter_tag_cols = filter_tag_cols,
                       imputation = imputation,
                       impute_per_cluster = impute_per_cluster,
                       winsorizing = winsorizing,
                       aggregation = aggregation)
    })
  }
  data_engineered <- assign.dict(data, dict_data)
  
  
  
  
  # ---- summary table ----
  data <- data_engineered
  # engineer stratify_col
  if(length(stratify_col)>0){
    if (dict_data$type[which(dict_data$varname==stratify_col)]=="num"){
      # if stratify col is numeric, break it by percentiles
      data[,stratify_col] <- cut(est_pctl(data[,stratify_col] ), breaks = seq(0,1,0.25), labels = c("(0th,25th]", "(25th,50th]", "(50th,75th]", "(75th,100th]") )
    }
  }
  for (var in colnames(data)){
    label(data[,var]) <- dict_data[var,"label"]
    # fix infinite values by NA
    if(var %in% num_cols){
      data[which(!is.finite(data[,var])),var] <- NA
    }
  }
  tryCatch({
    # summary table stratefied by response
    tbl_obj <- do_summ_tbl(data=data, 
                        dict_data=get.dict(data),
                        num_vars = num_cols, 
                        fct_vars = fct_cols, 
                        num_denom = "avail", 
                        fct_denom = "known", 
                        keys=c(cluster_col),
                        y=c(stratify_col) )
    tbl_st <- print(tbl_obj$tbl, varLabels = TRUE)
    res_df <- as.data.frame(tbl_st) %>% tibble::rownames_to_column()
    res_df <- res_df[which(res_df$rowname!="n.1"),]
    res_df$rowname  <- gsub("..median..IQR..", " median[IQR]", res_df$rowname )
    res_df$rowname  <- gsub("..mean..SD..", " mean(SD)", res_df$rowname )
    res_df$rowname  <- gsub("..Yes....", " = Yes", res_df$rowname )
    stratify_label <- ifelse( dict_data[which(dict_data$varname==stratify_col),"label"] == "", stratify_col, dict_data[which(dict_data$varname==stratify_col),"label"]) 
    colnames(res_df)[which(!colnames(res_df)%in%c("rowname","Overall"))] <- paste0(stratify_label," = ",colnames(res_df)[which(!colnames(res_df)%in%c("rowname","Overall"))]) 
    
    #  response 
    data$cluster_col <- data[,cluster_col]
    data$trim_by_col <- data[,trim_by_col]
    rsps_df <- data %>% summarise(
      n_cluster = n_distinct(cluster_col),
      n_episode = sum( diff(trim_by_col[!is.na(trim_by_col)])<0 ), # number of episodes counted by jump in relative time 
      n_row = n() ) %>% as.data.frame()
    rsps_df$y <- "Overall"
    if(length(stratify_col)>0){
      data$y <- as.character( data[, stratify_col] )
      if(dict_data$type[which(dict_data$varname==stratify_col)]=="fct"){
        rsps_df_strat <- data %>% group_by(y) %>% summarise(
          n_cluster = n_distinct(cluster_col),
          n_episode = sum( diff(trim_by_col[!is.na(trim_by_col)])<0 ), # number of episodes counted by jump in relative time 
          n_row = n()
        ) %>% as.data.frame()
        rsps_df <- bind_rows(rsps_df, rsps_df_strat )
      }
    }
  },error=function(e){
    print(e)
  })
  
  # ---- create num_detail_df ----
  tryCatch({
    df <- tbl_obj$num_detail_df 
    df$N <- df$n-df$miss
    df$mean_sd <- paste0(round(df$mean,2), " (", round(df$sd,2), ")")
    df$range <- paste0(round(df$median,2), " [",round(df$p25,2),", ",round(df$p75,2),"] (",round(df$min,2),", ",round(df$max,2),")")
    df$summary_string <- paste(paste0("N = ", df$N), df$mean_sd, df$range, sep =" \n ")
    for (att in c("label", "unit", "source_file", "unique_per_sbj")){
      if (!att %in% colnames(dict_data)) next
      df[,att] <- ""
      for(varname in df$varname){
        df[which(df$varname==varname),att] <- dict_data[which(dict_data$varname==varname), att]
      }
    }
    num_detail_df <- df[,c("group", "label", "summary_string", "unit", "source_file", "N", "mean_sd", "range", "varname", "unique_per_sbj")]
  },error=function(e){
    print("--- Error in num_detail_df --- ")
    print(e)
  })
  
  # ---- create fct_detail_df ----
  tryCatch({
    df <- tbl_obj$fct_detail_df
    df$varname <- gsub("[.][0-9]$", "", df$varname)
    for (att in c("label", "unit", "source_file", "unique_per_sbj")){
      if (!att %in% colnames(dict_data)) next
      df[,att] <- ""
      for(varname in df$varname){
        tryCatch({
          df[which(df$varname==varname),att] <- dict_data[which(dict_data$varname==varname), att]
        },error=function(e){
          print(paste0("skip assign attribute ", att," to ",varname))
          print(e)
        })
      }
    }
    df$N <- df$n-df$miss
    df$n_freq <- as.numeric(as.character(df$freq))
    df$p <- as.numeric(as.character(df$freq))/df$N
    df$summary_string <- paste0(round(df$p,2), " (", df$n_freq, "/",df$N,")")
    df$label_org <- df$label
    df$label <- paste0(df$label_org, " = ", df$level)
    fct_detail_df <- df[,c("group", "label", "summary_string", "unit", "source_file", "N", "n_freq", "p","varname", "unique_per_sbj")]
  },error=function(e){
    print("--- Error in fct_detail_df --- ")
    print(e)
  })
  
  # ---- reformat fct_detail_df_reformat ----
  fct_detail_df_reformat <- NULL
  tryCatch({
    if(!is.null(fct_detail_df)){
      for (g in unique(fct_detail_df$group) ){
        df_chunk <- fct_detail_df[which(fct_detail_df$group==g), c("label", "summary_string")]
        colnames(df_chunk) <- c("label", g)
        if(is.null(fct_detail_df_reformat)) {
          fct_detail_df_reformat <- df_chunk
        }else{
          fct_detail_df_reformat <- merge(fct_detail_df_reformat, df_chunk)
        }
      }
    }
  },error=function(e){
    print("Error! deriving fct_detail_df_reformat ")
  })
 
  # ---- reformat num_detail_df_reformat ----
  num_detail_df_reformat <- NULL
  tryCatch({
    if(!is.null(num_detail_df)){
      for (g in unique(num_detail_df$group) ){
        df_chunk <- num_detail_df[which(num_detail_df$group==g), c("label", "summary_string")]
        colnames(df_chunk) <- c("label", g)
        if(is.null(num_detail_df_reformat)) {
          num_detail_df_reformat <- df_chunk
        }else{
          num_detail_df_reformat <- merge(num_detail_df_reformat, df_chunk)
        }
      }
    }
  })
  
  # ---- combine numeric + factor + response in one  ----
  summ_df_reformat <- NULL
  summ_df_reformat <- bind_rows(num_detail_df_reformat, fct_detail_df_reformat)
  summ_df_reformat <- summ_df_reformat[, union("label",colnames(summ_df_reformat))]
  
  # reformat response table
  rsps_reformat <- NULL
  rsps_reformat <- as.data.frame(t(rsps_df))
  rsps_reformat["y",][which(is.na(rsps_reformat["y",]))] <- "NotAvailable"
  colnames(rsps_reformat) <- as.character( rsps_reformat["y",] )
  rsps_reformat$label <- rownames(rsps_reformat)
  rsps_reformat <- rsps_reformat[setdiff(rownames(rsps_reformat), "y"),]
  rownames( rsps_reformat ) <- 1:nrow(rsps_reformat)
  rsps_reformat <- rsps_reformat[,union("label",colnames(rsps_reformat))]

  #summ_df_reformat <- bind_rows(rsps_reformat, summ_df_reformat)
  # reformat column names of summary table
  for (col in setdiff(colnames(summ_df_reformat), "label") ){
    colname_new <- NULL
    colname_new <- paste0(col,"    N=", as.numeric(rsps_reformat[which(rsps_reformat$label=="n_cluster"), col]) )
    colname_new <- paste0(colname_new, "; NROW=", as.numeric(rsps_reformat[which(rsps_reformat$label=="n_row"),col]) )
    colnames(summ_df_reformat)[which(colnames(summ_df_reformat)==col)] <- colname_new
  }
  
  # --- NA plot ---
  for (fct_col in fct_cols){
    data[which(data[,fct_col]=="None_level"), fct_col] <- NA
  }
  tryCatch({
    if(length(c(num_cols,fct_cols))<100) {
      na_plot_cols <- c(num_cols,fct_cols)
    }else{
      na_plot_cols <- sample(c(num_cols,fct_cols), size=100)
    }
    na_obj <- Hmisc::naclus(data[,na_plot_cols])
  },error=function(e){
    print("--- Error in na_plot --- ")
    print(e)
  })
  
  return(list("summ_df" = res_df, 
              "summ_df_reformat" = summ_df_reformat,
              "num_detail_df" = num_detail_df,
              "fct_detail_df" = fct_detail_df,
              "rsps_df" = rsps_df,
              "na_obj"=na_obj))
  
}







# ############################# not run #############################
# data = data_ml
# dict_data = dict_ml
# cluster_label = "PreVent study ID"
# stratify_by=c("None", # no breakdown
#               "Mode of birth (EN)", # breakdown by >2 level categorical variable
#               "On respiratory support with endotracheal tube (EN)___Yes", # tag_col
#               "Brady_80_v4_dpd_min"
#               )[2]
# # --- engineer ---
# trim_by_label = "Post-menstrual Age"  # reltive time info
# trim_vec = c(-Inf, Inf) # trim relative time [from, to)
# time_unit = 7 # the increment scale of relative time
# trim_ctrl = TRUE
# pctcut_num_labels = c("PeriodicBreathing_v3 duration per day", "ABD_v3 number of events per day")
# pctcut_num_vec = c(0.1, 99.9)
# pctcut_num_coerce=TRUE
# filter_tag_labels=c()
# imputation=c("None","Mean", "Median", "Zero")[1]
# impute_per_cluster=FALSE
# winsorizing=TRUE
# aggregation=TRUE
# 
