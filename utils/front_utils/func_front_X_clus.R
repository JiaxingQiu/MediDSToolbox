front_X_clus <- function(
  data = data_ml,
  dict_data=dict_ml,
  trim_by_label="Post-menstrual Age",
  trim_vec = c(22, 40),
  time_unit=7,
  x_labels=c(c("Gestational Age", "PeriodicBreathing_v2 log(duration proportion) per day", "Birth weight"),"baby_insurance", "Mode of respiratory support (Positive Airway Pressure) without endotracheal tube (EN)___Nasal Cannula with flow"), 
  y_label="Primary outcome (EN)___Unfavorable", 
  cluster_label="PreVent study ID",
  r2=0.9,
  imputation="None",
  impute_per_cluster=FALSE,
  winsorizing=FALSE,
  aggregation=FALSE,
  r_abs=0.8, 
  type=c("pearson","spearman")[1],
  rank=TRUE,
  rcs5_low="70%", # knots to use talk to matthew (AIC? BIC? )
  rcs4_low="50%",
  trim_ctrl=TRUE 
){
  
  # front end wrapper for any type of regression
  
  # ---- pre-process ----
  x_cols <- rownames(dict_data[which(dict_data$label_front%in%x_labels), ])
  num_cols <- intersect(x_cols, rownames(dict_data[which(dict_data$mlrole=="input"&dict_data$type=="num"), ]))
  fct_cols <- setdiff(x_cols, num_cols)
  y_col <- rownames(dict_data[which(dict_data$label_front==y_label),])
  cluster_col <- rownames(dict_data[which(dict_data$label_front==cluster_label),])
  rel_time_col <- rownames(dict_data[which(dict_data$label_front==trim_by_label),])
  trim_by_col <- rownames(dict_data[which(dict_data$label_front==trim_by_label), ])
  
  # keep original df to keep temperal information
  data_org <- data
  data_org$cluster_id <- data_org[,cluster_col]
  data_org$rel_time <-  unlist(data_org %>%
                                 group_by(cluster_id) %>%
                                 group_map(~(.x[,rel_time_col] - max(.x[,rel_time_col],na.rm=TRUE)) ), use.names = FALSE)
  
  data_org <- assign.dict(data_org, dict_data)
  
  # trim time info
  if (length(trim_by_col)>0 ){
    # if there is a valid event / control group split, and user choose not to trim the control group
    if (all(unique(as.character(data[,y_col])) %in% c(1,0,NA)) & !trim_ctrl){
      data_pos <- data %>% filter(data[,y_col]==1 & data[,trim_by_col]>=trim_vec[1]*time_unit & data[,trim_by_col]<trim_vec[2]*time_unit ) %>% as.data.frame()
      data_ctrl <- data %>% filter(data[,y_col]==0) %>% as.data.frame()
      data <- bind_rows(data_pos, data_ctrl)
    }else{
      data <- data %>% filter(data[,trim_by_col]>=trim_vec[1]*time_unit & data[,trim_by_col]<trim_vec[2]*time_unit ) %>% as.data.frame()
    }
  }
  
  
  data$cluster_id <- data[,cluster_col]
  # impute numeric inputs
  if(!impute_per_cluster){
    if(imputation=="Mean"){
      data <- data %>% mutate_at(vars(num_cols), ~tidyr::replace_na(., mean(.,na.rm = TRUE))) %>% as.data.frame()
    }
    if(imputation=="Median"){
      data <- data %>% mutate_at(vars(num_cols), ~tidyr::replace_na(., median(.,na.rm = TRUE))) %>% as.data.frame()
    }
    if(imputation=="Zero"){
      data <- data %>% mutate_at(vars(num_cols), ~tidyr::replace_na(., 0)) %>% as.data.frame()
    }
  }else{
    if(imputation=="Mean"){
      for(sbj in unique(data[,cluster_col]) ){
        for(col in num_cols){
          data[which(data[,cluster_col]==sbj & is.na(data[,col])), col] <- mean(data[which(data[,cluster_col]==sbj), col], na.rm=TRUE)
        }
      }
    }
    if(imputation=="Median"){
      for(sbj in unique(data[,cluster_col]) ){
        for(col in num_cols){
          data[which(data[,cluster_col]==sbj & is.na(data[,col])), col] <- median(data[which(data[,cluster_col]==sbj), col], na.rm=TRUE)
        }
      }
    }
    if(imputation=="Zero"){
      for(sbj in unique(data[,cluster_col]) ){
        for(col in num_cols){
          data[which(data[,cluster_col]==sbj & is.na(data[,col])), col] <- 0
        }
      }
    }
  }
  
  
  # winsorize numeric columns
  if(winsorizing){
    data[,num_cols] <- winsorize(data[,num_cols])
  }
  # aggregatin if required
  if(aggregation){
    df_y <- NULL
    df_fct <- NULL
    df_num <- NULL
    
    # y tag - max 
    if(dict_data[y_col, "type"]=="num"){
      df_y <- data %>% group_by(data[,cluster_col]) %>% summarise_at(vars(y_col), ~mean(.,na.rm = TRUE)) %>% as.data.frame()
      colnames(df_y) <- c(cluster_col, y_col)
    }else if(dict_data[y_col, "type"]=="fct"){
      df_y <- data %>% group_by(data[,cluster_col]) %>% summarise_at(vars(y_col), ~max(.,0,na.rm = TRUE)) %>% as.data.frame()
      colnames(df_y) <- c(cluster_col, y_col) 
    }
    # x num - mean
    if(length(num_cols)>0){
      df_num <- data %>% group_by(data[,cluster_col]) %>% summarise_at(vars(num_cols), ~mean(.,na.rm = TRUE)) %>% as.data.frame()
      colnames(df_num) <- c(cluster_col, num_cols)
    }
    #x fct - max
    if(length(fct_cols)>0){
      df_fct <- data %>% group_by(data[,cluster_col]) %>% summarise_at(vars(fct_cols), ~max(.,0,na.rm = TRUE)) %>% as.data.frame()
      colnames(df_fct) <- c(cluster_col, fct_cols)
    }
    data <- df_y
    if (!is.null(df_fct)){
      data <- merge(data, df_fct)
    }
    if (!is.null(df_num)){
      data <- merge(data, df_num)
    }
  }
  data <- assign.dict(data, dict_data)
  
  
  # ---- redundancy clus ----
  results <- do_x_redun(df=data, 
                        dict_df=dict_data, 
                        x_cols=x_cols, 
                        r_abs=r_abs, 
                        type=type, 
                        rank=rank, 
                        r2=r2)
  
  
  # redundency object
  if(!is.null(results$rmcor_obj)){
    x_corre_obj <- list("trace" = results$rmcor_obj$cor_trace,
                        "cor_df_org" = results$rmcor_obj$cor_df_org,
                        "keep" = paste0("Correlation criteria keep variables --- ",paste0(results$rmcor_obj$keep,collapse = ", ")),
                        "delete" = paste0("Correlation criteria delete variables --- ",paste0(results$rmcor_obj$delete,collapse = ", ")) )
    
  }else{
    x_corre_obj <- list("trace" = data.frame( fail_message = "correlation analysis not run", stringsAsFactors = FALSE),
                        "cor_df_org" = data.frame( fail_message = "less than 2 numeric variables", stringsAsFactors = FALSE), 
                        "keep" = "Correlation criteria keep variables --- ",
                        "delete" =  "Correlation criteria delete variables --- ")
    
  }
  x_redun_obj = ifelse(is.null(results$redun_obj), "", results$redun_obj)
  
  
  # ---- missingness and summary clus ----
  summ_cols <- unique(c(x_cols, num_cols, fct_cols, y_col, cluster_col, rel_time_col, trim_by_col))
  summ_cols <- intersect(colnames(results$df_final), summ_cols)
  print(colnames(results$df_final))
  print(summ_cols)
  ml_summ_obj <- front_summary_tbl(data = results$df_final[,summ_cols],
                                   dict_data = results$dict_final,
                                   trim_by_label=trim_by_label,
                                   trim_vec = trim_vec,
                                   stratify_by=y_label,
                                   cluster_label=cluster_label)
  
  
  # ---- spearman2 clus for degree of freedom ----
  dof_obj <- NULL
  if (length(num_cols)>0){
    fml <- formula(paste(y_col," ~ ", paste(num_cols,collapse = "+")))
    rho <- spearman2(fml, data=data, p=2)
    rho_df <- data.frame(adj_rho=rho[,"Adjusted rho2"])
    rho_df$cols <- rownames(rho)
    rho_df <- rho_df[order(rho_df$adj_rho, decreasing = TRUE),]
    scaler <- data.frame(q=quantile(rho_df$adj_rho, probs = seq(0, 1, 0.05) , na.rm = TRUE))
    print(scaler)
    rcs5_cut <- scaler[rcs5_low,"q"]
    rcs4_cut <- scaler[rcs4_low,"q"]
    dof_obj <- list("rho" = rho,
                    "rcs5_cut" = rcs5_cut,
                    "rcs4_cut" = rcs4_cut)
  }
  
  
  return(list("x_corre_obj"=x_corre_obj,
              "x_redun_obj"=x_redun_obj,
              "ml_summ_obj"=ml_summ_obj,
              "dof_obj"=dof_obj))
  
  
}
